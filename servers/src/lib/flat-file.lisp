(defpackage :flat-file-record
  (:use :cl :local-time)
  (:export #:define-format
           #:start
           #:end
           #:picture
           #:write*
           #:read*
           #:field-parse-error
           #:postgres-column-definitions
           #:postgres-column-inserter
           #:pretty-print-record
           #:excel-import-help
           #:flat-file-record-class
           #:mapped-flat-file-record-class
           #:constant-field-p
           #:field-slots
           #:do-records-from-file
           #:do-records-from-file*
           ;; Mapped files support
           #:get-record
           #:number-of-records
           #:mapped-string
           #:name
           #:size
           #:make-file-mapped-string
           #:record-length
           #:postgres-column-names
           #:postgres-column-values))

(in-package :flat-file-record)

(define-condition field-parse-error (error)
  ((error :initarg :error)
   (file-position :initarg :file-position :initform nil)
   (field-name :initarg :field-name)
   (field-start :initarg :field-start)
   (field-end :initarg :field-end)
   (field-value :initarg :field-value)
   (picture :initarg :picture)
   (constraint :initarg :constraint))
  (:report (lambda (c s)
             (with-slots (error file-position field-name field-value 
                                field-start field-end picture constraint) c
               (format s "~@[At ~:d:~][cols ~d‐~d]: Field ~a (~a~@[ ~s~]~@[*~]: ~a"
                       file-position field-start field-end
                       field-name picture field-value contraint
                       error)))))

(defun daynum-of-year (timestamp)
  (floor (1+ (/ (timestamp-difference 
                 timestamp (timestamp-minimize-part timestamp :month)) (* 24 60 60)))))

(defclass flat-file-record-class ()
  ((record-length :reader record-length)
   (line-oriented-p :reader line-oriented-p :initform nil :initarg :line-oriented-p)))

(defmethod initialize-instance :after ((class flat-file-record-class) &key line-oriented-p)
  (when line-oriented-p
    (setf (slot-value class 'line-oriented-p) (first line-oriented-p))))

(defmethod reinitialize-instance :after ((class flat-file-record-class) &key line-oriented-p)
  (when line-oriented-p
    (setf (slot-value class 'line-oriented-p) (first line-oriented-p))))

(defmethod closer-mop:validate-superclass ((sub flat-file-record-class) (super closer-mop:standard-class))
  t)

(defclass flat-file-record-slot-definition ()
  ((start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (picture :initarg :picture :reader picture)
   (constant-field-p :initarg :constant-field-p :reader constant-field-p))
  (:default-initargs :constant-field-p nil))

(defclass direct-flat-file-record-slot-definition (flat-file-record-slot-definition
                                                     )
  ())

(defclass effective-flat-file-record-slot-definition (flat-file-record-slot-definition
                                                        )
  ())

(defclass effective-constant-fixed-with-record-slot-definiiton (flat-file-record-slot-definition
                                                                closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class flat-file-record-class) &key start end picture)
  (cond
    ((or start end picture)
     (assert (and start end picture) () "need to specify all of :START :END :PICTURE when creating fixed width record slot")
     'direct-flat-file-record-slot-definition)
    (t
     (call-next-method))))

(defvar *effective-field-slot-class*)

(defmethod closer-mop:effective-slot-definition-class ((class flat-file-record-class) &key)
  (or *effective-field-slot-class*
      (call-next-method)))

(defmethod closer-mop:compute-effective-slot-definition ((class flat-file-record-class) (name t) direct-slot-definitions)
  ;; there must be a better way to initialize the effective slot definition
  (let* ((first-slot (first direct-slot-definitions))
         (*effective-field-slot-class*
           (when (typep first-slot 'direct-flat-file-record-slot-definition)
             (if (constant-field-p first-slot)
                 'effective-constant-fixed-with-record-slot-definiiton
                 'effective-flat-file-record-slot-definition)))
         (effective-slot-definition (call-next-method)))
    (when *effective-field-slot-class*
      (dolist (slot-name (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of first-slot))))
        (when (find slot-name (closer-mop:class-slots (class-of effective-slot-definition)) :key #'closer-mop:slot-definition-name)
          (setf (slot-value effective-slot-definition slot-name)
                (slot-value first-slot slot-name)))))
    effective-slot-definition))

(defun field-slots (class)
  (remove-if-not (lambda (slot-definition)
                   (typep slot-definition 'flat-file-record-slot-definition))
                 (closer-mop:class-slots class)))

(defun finalize-inheritance (class)
  (let ((position 1)
        field-slots)
    (dolist (slot-definition (sort (field-slots class) #'< :key #'start))
      (with-accessors ((name closer-mop:slot-definition-name)
                       (start start)
                       (end end)) slot-definition
        (unless (= start position)
          (warn "field ~A in ~A unexpectedly starts at position ~A, ~A expected"
                name class start position))
        (setf position (1+ end))
        (push slot-definition field-slots)))
    (setf field-slots (nreverse field-slots)
          (slot-value class 'record-length) (1- position))
    class))

(defmethod closer-mop:finalize-inheritance :after ((class flat-file-record-class))
  (finalize-inheritance class))

(defgeneric field-type-default (type)
  (:documentation "Return the (internal) default value for a field of the given TYPE"))

(defgeneric field-formatter (type value length &key alternative)
  (:documentation "Return string representing VALUE according to TYPE"))

(defgeneric field-parser (type buffer)
  (:documentation "Parse TYPE from BUFFER and return the parsed value"))

(defgeneric field-db-type (type length)
  (:documentation "Return the database type for a field of TYPE with the given calculated LENGTH in characters")
  (:method ((type t) (length t))
    nil))

(defgeneric field-null-character (type)
  (:documentation "Return the character that is used to denote a
NULL value in all field positions for fields of the given TYPE, or NIL
there is no NULL value for fields of this type. Returns a string if
the null value of this field must be represented by that string,
padded with blanks.")
  (:method ((type t))
    nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun picture-symbol (picture)
    (intern (string picture) :flat-file-record)))

(defparameter *all-field-type-docs* nil)

(defun enumerate-fields ()
  "Print a little documentation about all of the field types defined
to work with this engine."
  (format t "~& *** All defined field types: ~%~%~{~A~}" *all-field-type-docs*))

(defmacro define-field-type (picture &key default-value null-character formatter parser db-type name)
  "This macro is a bit nasty because it is unhygienic to make using
it more terse.  The db-type keyword argument uses unusual (lack of)
quoting.  The field-formatter and field-parser methods return code that
is spliced into the generated writer and reader methods and that
captures variables from the generated lexical environment."
  (let ((picture (picture-symbol picture)))
    `(progn
       (pushnew 
        ,(format nil
                 "~%~% * Picture: ~A ~:[~*~;(~A)~]~%Default value: ~A~%Null character: ~S~%~:[~;Writeable~] ~:[~;Readable~]"
                 picture name name default-value (or null-character "(None)") formatter parser)
        *all-field-type-docs* :test #'equal)
       
       (defmethod field-type-default ((type (eql ',picture)))
         ',default-value)
       
       (defmethod field-formatter ((type (eql ',picture)) value length &key alternative)
         ,(or formatter
              (error "no :formatter for picture ~A" picture)))
       
       (defmethod field-parser ((type (eql ',picture)) buffer)
         ,parser)
       
       ,@(when null-character
           `((defmethod field-null-character ((type (eql ',picture)))
               ,null-character)))
       ,@(when db-type
           `((defmethod field-db-type ((type (eql ',picture)) length)
               (declare (ignorable length))
               ,(if (listp db-type)
                    `(list ',(first db-type) ,(second db-type))
                    `',db-type)))))))

(define-field-type N
  :name "Numeric"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,'0D" length value)
                 (error "~A is not a number" value))
  :parser (utils:parse-monetary-amount buffer)
  :db-type (integer length))

(define-field-type N$
  :name "Numeric with implied hundreds decimal (dollars)"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,'0D" length (round (* 100 value)))
                 (error "~A is not a number" value))
  :parser (utils:parse-monetary-amount buffer)
  :db-type (integer length))

(define-field-type $
  :name "Monetary amount with 2 digits after the comma"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,2,,,'0F" length value)
                 (error "~A is not a number" value))
  :parser (utils:parse-monetary-amount buffer)
  :db-type float)

(define-field-type D3
  :name "Number with 3 digits after the comma"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,3,,,'0F" length value)
                 (error "~A is not a number" value))
  :parser (utils:parse-decimal-amount buffer)
  :db-type float)

(define-field-type D4
  :name "Number with 4 digits after the comma"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,4,,,'0F" length value)
                 (error "~A is not a number" value))
  :parser (utils:parse-decimal-amount buffer)
  :db-type :double)


(define-field-type D5
  :name "Number with 5 digits after the comma"
  :default-value 0
  :formatter (if (numberp value)
                 (format nil "~V,5,,,'0F" length value)
                 (error "~A is not a number" value))
  :parser (utils:parse-decimal-amount buffer)
  :db-type :double)

(define-field-type A
  :name "Alphabetic"
  :default-value nil
  :null-character #\Space
  :formatter (format nil "~VA" length value)
  :parser (let ((trimmed (string-right-trim '(#\Space) buffer)))
            (unless (zerop (length trimmed))
              trimmed))
  :db-type (varchar length))

(define-field-type AN
  :name "Alphanumeric"
  :default-value nil
  :null-character #\Space
  :formatter (format nil "~VA" length value)
  :parser (let ((trimmed (string-right-trim '(#\Space) buffer)))
            (unless (zerop (length trimmed))
              trimmed))
  :db-type (varchar length))

(define-field-type AN*
  :default-value nil
  :null-character #\Space
  :formatter (let ((s (typecase value
                        (string value)
                        (t (princ-to-string value)))))
               (format nil "~VA" length (subseq s 0 (min (length s) length))))
  :parser (let ((trimmed (string-right-trim '(#\Space) buffer)))
            (unless (zerop (length trimmed))
              trimmed))
  :db-type (varchar length))

(define-field-type X
  :default-value nil
  :null-character #\Space
  :formatter (format nil "~VA" length value)
  :parser buffer
  :db-type (varchar length))

(define-field-type YYYYJJJHHMMSS
  :name "Full year, month, day, hour, minutes, and seconds"
  :null-character #\0
  :formatter (format nil "~4,'0D~3,'0D~2,'0D~2,'0D~2,'0D"
                     (timestamp-year value)
                     (daynum-of-year value)
                     (timestamp-hour value)
                     (timestamp-minute value)
                     (timestamp-second value))
  :parser (unless (or (equal "0000000000000" buffer)
                      (equal "9999999999999" buffer))
            (timestamp+ (encode-timestamp 0
                                                                (parse-integer (subseq buffer 11 13))
                                                                (parse-integer (subseq buffer 9 11))
                                                                (parse-integer (subseq buffer 7 9))
                                                                1
                                                                1
                                                                (parse-integer (subseq buffer 0 4)))
                                   (1- (parse-integer (subseq buffer 4 7)))
                                   :day))
  :db-type time)

(define-field-type MMDDYYYY
    :null-character #\0
    :formatter (typecase value
                 (timestamp 
                  (format nil "~2,'0D~2,'0D~4,'0D"
                          (timestamp-month value)
                          (timestamp-day value)
                          (timestamp-year value)))
                 (t alternative))
    :parser (unless (or (equal "00000000" buffer)
                        (equal "99999999" buffer))
              (encode-timestamp 0 0 0 0
                                (parse-integer (subseq buffer 2 4))
                                (parse-integer (subseq buffer 0 2))
                                (parse-integer (subseq buffer 4 8))))
    :db-type date)

(define-field-type YYYY-MM-DD
    :null-character #\0
    :formatter (format nil "~4,'0D-~2,'0D-~2,'0D"
                       (timestamp-year value)
                       (timestamp-month value)
                       (timestamp-day value))
    :parser (if
             (or (equal (string-trim " " buffer) "") 
                 (< (parse-integer (string-trim " " buffer)) 1900))
             nil
             (cl-ppcre:register-groups-bind (year month day) ("([0-9]+)-([0-9]+)-([0-9]+)" buffer)
               (encode-timestamp 0 0 0 0
                                 (parse-integer day)
                                 (parse-integer month)
                                 (parse-integer year))))
    :db-type date)

(define-field-type MM/DD/YYYY
    :null-character #\0
    :formatter (format nil "~2,'0D/~2,'0D/~4,'0D"
                       (timestamp-month value)
                       (timestamp-day value)
                       (timestamp-year value))
    :parser (encode-timestamp 0 0 0 0
                              (parse-integer (subseq buffer 3 5))
                              (parse-integer (subseq buffer 0 2))
                              (parse-integer (subseq buffer 6 10)))
    :db-type date)

(define-field-type YYYYMMDD
    :name "Date, 8 characters, descending size order: YYYY MM DD"
    :null-character #\0
    :formatter (typecase value
                 (timestamp (format nil "~4,'0D~2,'0D~2,'0D"
                                    (timestamp-year value)
                                    (timestamp-month value)
                                    (timestamp-day value)))
                 (t alternative))
    :parser (cond
              ((zerop (length (string-right-trim (list (code-char 0) #\Space) buffer))) nil)
              ((string= buffer "00000000") nil)
              ((string= buffer "99999999") nil)
              ((string= buffer "99991231") nil)
              ((string= buffer "19000101") nil)
              ((string= buffer "19000000") nil)
              ((string= buffer "18999999") nil)
              ((every #'digit-char-p buffer) (encode-timestamp 0 0 0 0
                                                               (parse-integer (subseq buffer 6 8))
                                                               (parse-integer (subseq buffer 4 6))
                                                               (parse-integer (subseq buffer 0 4))))
              (t (warn "YYYYMMDD field could not parse '~A'" buffer)
                 nil))
    :db-type date)

(define-field-type YYYYMM
    :null-character #\0
    :formatter (format nil "~4,'0D~2,'0D"
                       (timestamp-year value)
                       (timestamp-month value))
    :parser (unless (or (equal buffer "000000")
                        (equal buffer "999999"))
              (encode-timestamp 0 0 0 0
                                1
                                (parse-integer (subseq buffer 4 6))
                                (parse-integer (subseq buffer 0 4))))
    :db-type date)

(define-field-type YN
    :null-character #\Space
    :formatter (cond
                 ((eql value :null) " ")
                 (value "Y")
                 (t "N"))
    :parser (cond
              ((member (char buffer 0) '(#\Y #\y)) t)
              ((eql (char buffer 0) #\Space) :null)
              (t nil))
    :db-type boolean)

(define-field-type S
  :formatter (string #\Newline)
  :parser (assert (member (char buffer 0) '(#\Return #\Linefeed))
                  () "Unexpected line termination, expected (single-char) #\\Return or #\\Linefeed, got ~S"
                  (char buffer 0)))

(define-field-type CRLF
  :formatter (load-time-value (with-output-to-string (s)
                                (write-char #\Return s)
                                (write-char #\Linefeed s)))
  :parser (assert (and (member (char buffer 0) '(#\Return #\Linefeed))
                       (member (char buffer 1) '(#\Return #\Linefeed)))
                  () "Unexpected line termination, expected #\\Return #\\Linefeed (would accept any combo), got ~S ~S"
                  (char buffer 0) (char buffer 1)))

(define-field-type LF
  :formatter (string #\Linefeed)
  :parser (assert (eql (char buffer 0) #\Linefeed)
                  () "Unexpected line termination, expected #\\Linefeed, got ~S"
                  (char buffer 0)))

(defun string-all-p (character string)
  (not (find character string :test (complement #'eql))))

(defun write-chars (length character stream)
  (dotimes (i length)
    (write-char character stream)))

(define-condition field-formatter-error (error)
  ((field-name :initarg :field-name)
   (start :initarg :start)
   (end :initarg :end)
   (picture :initarg :picture)
   (value :initarg :value)
   (overflow-length :initarg :overflow-length))
  (:report (lambda (c s)
             (with-slots (field-name start end picture value overflow-length) c
               (format s "The field ~S (positions ~A-~A, picture ~A) with value ~S ~:[under~;over~]flowed the allocated space by ~A character~:P"
                       field-name start end picture value (plusp overflow-length) (abs overflow-length))))))

(defun stringify-to-length (entity length)
 "Prepares 'entity to fit in a string of length 'length.
  Characters, integers, strings only"
  (etypecase entity
    (character (make-string length :initial-element entity))
    (integer (make-string length :initial-element (code-char entity)))
    (string (replace (make-string length :initial-element #\Space)
                     entity
                     :end2 (min length (length entity))))))

(defun format-field (value length picture)
  (alexandria:if-let (nullish (field-null-character picture))
    (let ((formatted-nullish (stringify-to-length nullish length))) 
      (if value
          (field-formatter picture value length :alternative formatted-nullish)
          formatted-nullish))
    (field-formatter picture value length)))

(defun write* (record stream)
  (let ((buffer (make-string (record-length (class-of record)))))
    (dolist (slot-definition (field-slots (class-of record)))
      (with-accessors ((name closer-mop:slot-definition-name)
                       (start start)
                       (end end)
                       (picture picture)) slot-definition
        (let* ((value (cond
                        ((string-equal name 'filler) (if (string-equal picture 'N) 0 ""))
                        ((string-equal name 'reserved) (if (string-equal picture 'N) 0 ""))
                        ((string-equal picture 'S) #\Newline)
                        (t (if (and (slot-boundp record name)
                                    (slot-value record name))
                               (slot-value record name)
                               (field-type-default picture)))))
               (length (1+ (- end start)))
               (formatted-field (handler-case
                                    (format-field value length picture)
                                  ((or type-error simple-error) (e) (warn "Error ~A in field ~A" e name)
                                    (error e)))))
          (unless (= length (length formatted-field))
            (error 'field-formatter-error
                   :field-name name
                   :start start
                   :end end
                   :picture picture
                   :value value
                   :overflow-length (- (length formatted-field) length)))
          (replace buffer formatted-field :start1 (1- start)))))
    (write-string buffer stream)
    (when (line-oriented-p (class-of record))
      (write-char #\Return stream)
      (write-char #\Linefeed stream))))

(defclass mapped-flat-file-record-class (flat-file-record-class)
  ())

(defun parse-slot-from-buffer (slot-definition buffer)
  (let ((buffer (nsubstitute-if-not #\? (lambda (c) (typep c 'base-char))
                                    (subseq buffer (1- (start slot-definition)) (end slot-definition)))))
    (handler-case
        (field-parser (picture slot-definition) buffer)
      (error (e)
        (error 'field-parse-error
               :error e
               :field-name (closer-mop:slot-definition-name slot-definition)
               :field-start (start slot-definition)
               :field-end (end slot-definition)
               :picture (picture slot-definition)
               :field-value buffer)))))

(defmethod closer-mop:slot-value-using-class ((class mapped-flat-file-record-class) record (slot-definition effective-flat-file-record-slot-definition))
  (parse-slot-from-buffer slot-definition (buffer record)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *record-length*)
  
  (defun field-to-slots (field-definition)
    (destructuring-bind (name start end picture &key value (null-value value)) field-definition
      (let ((picture (picture-symbol picture))
            (constant-field-p (or (string-equal name 'filler)
                                  (string-equal name 'reserved)
                                  (when (member picture '(S CRLF) :test #'string-equal)
                                    t)))
            (field-length (1+ (- end start))))
        (incf *record-length* field-length)
        `(,@(if constant-field-p
                `(,(gensym (string name)) :allocation :class)
                `(,name :initarg ,(intern (string name) :keyword)
                        :accessor ,name))
            :initform ,null-value
            :start ,start
            :end ,end
            :picture ,picture
            :constant-field-p ,constant-field-p
            ,@(alexandria:when-let (col-type (and (not constant-field-p)
                                                  `(or nil ,(field-db-type picture field-length))))
                `(:col-type ,col-type))))))

  (defun fields-to-slots (fields)
    (let ((*record-length* 0))
      (mapcar 'field-to-slots fields))))

(defun read* (record-class-name stream &rest initargs)
  (let* ((record-class (closer-mop:ensure-finalized (find-class record-class-name)))
         (buffer (make-string (record-length record-class))))
    (prog1
        (progn
          (unless (= (record-length record-class) (read-sequence buffer stream))
            (error 'end-of-file :stream stream))
          (let ((record (allocate-instance record-class)))
            (dolist (slot-definition (field-slots record-class))
              (setf (slot-value record (closer-mop:slot-definition-name slot-definition)) (parse-slot-from-buffer slot-definition buffer)))
            (apply #'initialize-instance record initargs)))
      (when (line-oriented-p record-class)
        (loop for char = (peek-char nil stream nil)
           while (member char '(#\Return #\Linefeed))
           do (read-char stream))))))

(defmacro define-format (name supers fields &rest options)
  (let ((mappedp (second (find :mappedp options :key #'first))))
    `(prog1
         (closer-mop:finalize-inheritance
          (defclass ,name ,supers
            ((buffer :reader buffer :initarg :buffer)
             ,@(fields-to-slots fields))
            (:metaclass ,(if mappedp 'mapped-flat-file-record-class 'flat-file-record-class))
            ,@options))
       ,(when mappedp
          `(defmethod initialize-instance :after ((record ,name) &key buffer)
                      (setf (slot-value record 'buffer) (or buffer
                                                            (make-string (record-length (class-of record)) :initial-element #\Space))))))))

(defun excel-import-help (record-class)
  (let ((record-class (etypecase record-class
                        (class record-class)
                        (symbol (find-class record-class)))))
    (closer-mop:finalize-inheritance record-class)
    (let ((data-slots (remove 'crlf (field-slots record-class)
                              :test #'string-equal
                              :key #'picture)))
      (format t ".TextFileColumnDataTypes = Array(~{~A~^,~})~%"
              (mapcar (lambda (slot-definition)
                        (if (string-equal (picture slot-definition) 'N)
                            1 2))
                      data-slots))
      (format t ".TextFileFixedColumnWidths = Array(~{~A~^,~})~%"
              (mapcar (lambda (slot-definition)
                        (1+ (- (end slot-definition) (start slot-definition))))
                      data-slots))
      (princ (closer-mop:slot-definition-name (first data-slots)))
      (dolist (slot (rest data-slots))
        (write-char #\tab)
        (princ (closer-mop:slot-definition-name slot)))
      (terpri))))

(defmacro do-records-from-file ((record record-type source &optional result) &body body)
  (alexandria:with-gensyms (stream)
    `(utils:with-open-source (,stream ,source)
                             (loop
                                (unless (peek-char nil ,stream nil)
                                  (return))
                                (let ((,record (flat-file-record:read* ,record-type ,stream)))
                                  ,@body
                                  ,result)))))

(defclass file-mapped-string ()
  ((name :reader name
         :initarg :pathname)
   (size :reader size
         :initarg :size)
   (mapped-string :reader mapped-string
                  :initarg :mapped-string)))

(defmethod print-object ((file-mapped-string file-mapped-string) stream)
  (print-unreadable-object (file-mapped-string stream :type t :identity t)
    (format stream "~S, ~:D bytes mapped" (name file-mapped-string) (size file-mapped-string))))

;; The following SBCL specific code is courtesy of Stas Boukarev
#+sbcl
(defun make-file-mapped-string (pathname)
  (let* ((fd (sb-posix:open (namestring (merge-pathnames pathname)) sb-posix:o-rdonly))
         (size (sb-posix:stat-size (sb-posix:fstat fd)))
         (page-size (sb-posix:getpagesize))
         (header-start-sap (sb-posix:mmap (sb-sys:int-sap 0)
                                          (+ page-size size)
                                          (logior sb-posix:prot-read
                                                  sb-posix:prot-write)
                                          (logior sb-posix:map-shared
                                                  sb-posix:map-anon)
                                          0 0))
         (string-start-sap (sb-posix:mmap (sb-sys:sap+ header-start-sap page-size)
                                          size
                                          sb-posix:prot-read (logior sb-posix:map-shared
                                                                     sb-posix:map-fixed)
                                          fd 0)))
    (sb-posix:close fd)
    ;; Create the string header
    (setf (sb-sys:sap-ref-word string-start-sap (- sb-vm:n-word-bytes))
          (ash size sb-vm:n-fixnum-tag-bits))
    (setf (sb-sys:sap-ref-word string-start-sap (- (* sb-vm:n-word-bytes 2)))
          sb-vm:simple-base-string-widetag)
    ;; Create a non-collected Lisp object using the constructed memory
    ;; layout (header followed by mapped file with string data); make
    ;; sure that we unmap the file when the object is garbage
    ;; collected.
    (trivial-garbage:finalize 
     (make-instance 'file-mapped-string
                    :pathname pathname
                    :size size
                    :mapped-string (sb-kernel:%make-lisp-obj
                                    (logior (- (sb-sys:sap-int string-start-sap)
                                               (* sb-vm:n-word-bytes 2))
                                            sb-vm:other-pointer-lowtag)))
     (lambda ()
       (sb-posix:munmap string-start-sap size)
       (sb-posix:munmap header-start-sap (+ page-size size))))))

(defmethod get-record ((file-mapped-string file-mapped-string) 
                       (record-class-name symbol) record-number &rest initargs)
  (let* ((class (find-class record-class-name))
         (start (* (record-length class) record-number)))
    (apply #'make-instance record-class-name
           :buffer (subseq (mapped-string file-mapped-string) start (+ start (record-length class)))
           initargs)))

(defun pretty-print-record (record &optional (stream t))
  (let* ((slot-names (remove-if (lambda (slot-name)
                                  (or (null (symbol-package slot-name))
                                      (eq (symbol-package slot-name) (find-package :flat-file-record))))
                                (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of record)))))
         (label-width (reduce #'max (mapcar (alexandria:compose #'length #'string) slot-names))))
    (format stream "Record ~A~%" record)
    (dolist (slot-name slot-names)
      (format stream "~VA: ~@[~S~]~%" label-width slot-name (ignore-errors (slot-value record slot-name))))))

(defmethod number-of-records ((file-mapped-string file-mapped-string) record-class-name)
  (let ((class (find-class record-class-name)))
    (truncate (size file-mapped-string) 
              (record-length class))))
