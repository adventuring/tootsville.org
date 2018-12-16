(in-package :Tootsville)



(defmethod jonathan.encode:%to-json ((uuid uuid:uuid))
  (jonathan.encode:%write-string (princ-to-string uuid)))

(defmethod jonathan.encode:%to-json ((color24 color24))
  (jonathan.encode:%write-string (color24-name  color24)))

(defmethod jonathan.encode:%to-json ((timestamp timestamp))
  (jonathan.encode:%write-string (princ-to-string (timestamp-to-unix timestamp))))



(defmacro ignore-not-found (&body body)
  `(handler-case (progn ,@body)
     (not-found (c)
       (declare (ignore c))
       nil)))

(defun ensure-record (type &rest columns+values)
  (or (values (ignore-not-found (apply #'find-record type columns+values)) t)
      (values (apply #'make-record type columns+values) nil)))

(defmacro do-records ((record-var type &rest columns+values) &body body)
  "Apply BODY to each row as if `FIND-RECORDS' were called."
  `(do-db-records-simply (,record-var ,(db-table-for type) ,@columns+values)
     ,@body))



(defgeneric db-table-for (type)
  (:documentation
   "Which database table contains the data mirrored by the ORM TYPE"))
(defgeneric id-column-for (type)
  (:documentation
   "Which column (if any) provides the primary key for TYPE"))
(defgeneric make-record (type &rest columns+values)
  (:documentation
   "Create a new record of TYPE with initial values COLUMNS+VALUES."))
(defgeneric find-record (type &rest columns+values)
  (:documentation
   "Find a record of TYPE where each of COLUMNS+VALUES are exact matches.

Expects to find 0 or 1 result."))
(defgeneric find-records (type &rest columns+values)
  (:documentation
   "Find all records of TYPE where each of COLUMNS+VALUES are exact matches."))
(defgeneric find-reference (object column)
  (:documentation
   "Following the reference COLUMN on OBJECT, return the object referred-to."))
(defgeneric load-record (type columns)
  (:documentation
   "Create an object of TYPE from the raw data in COLUMNS"))
(defgeneric save-record (object)
  (:documentation
   "Write OBJECT to the database, with any changes made"))
(defgeneric destroy-record (object)
  (:documentation
   "Delete the row in the database representing OBJECT"))



(defun lisp-to-db-name (name)
  "Convert a Lispy name to an SQL-type one.

Particularly, changes CAPS-WITH-KEBABS to lower_with_snakes."
  (cffi:translate-name-to-foreign (make-keyword (string name)) *package*))

(defun column-save-value (value type)
  "Convert VALUE into the database's representation of TYPE"
  (ecase type
    (:string value)
    (:keyword (and value (string value)))
    (:yornp (if value "Y" "N"))
    (:number value)
    (:json (and value
                (with-output-to-string (*standard-output*)
                  (jonathan.encode:%to-json value))))
    (:uri (and value (etypecase value
                       (puri:uri (puri:render-uri value nil))
                       (string value))))
    (:color24 (and value (format nil "~6,'0x"
                                 (color24-to-integer value))))
    (:uuid (etypecase value
             (null nil)
             (string (subseq (cl-base64:usb8-array-to-base64-string
                              (uuid:uuid-to-byte-array
                               (uuid:make-uuid-from-string value)))
                             0 22))
             (uuid:uuid (subseq (cl-base64:usb8-array-to-base64-string
                                 (uuid:uuid-to-byte-array value))
                                0 22))))
    (:timestamp (and value (timestamp-to-unix value)))))

(defun column-load-value (value type)
  "For a column of TYPE, interpret raw VALUE"
  (ecase type
    (:string value)
    (:keyword (make-keyword value))
    (:yornp (ecase
                (make-keyword value)
              (:y t) (:n nil)))
    (:number (etypecase value
               (null nil)
               (integer value)
               (rational (format nil "~f" (* 1.0 value)))
               (real (format nil "~f" value))
               (t (error "Can't record number ~s?" value))))
    (:json (and value
                (< 0 (length value))
                (jonathan.decode:parse value)))
    (:uri (puri:parse-uri value))
    (:color24
     (etypecase value
       (null nil)
       (integer (integer-to-color24 value))
       (string (parse-color24 value))))
    (:uuid (uuid:byte-array-to-uuid
            (cl-base64:base64-string-to-usb8-array
             (format nil "~a==" value))))
    (:timestamp (let ((τ value))
                  (etypecase τ
                    (null nil)
                    (integer (unix-to-timestamp τ))
                    (string (if (equalp τ "0000-00-00")
                                nil
                                (parse-timestring (substitute #\T #\Space τ))))
                    (vector (if (equalp τ #(48 48 48 48 45 48 48 45 48 48))
                                nil
                                (parse-timestring (substitute #\T #\Space (map 'string #'code-char τ))))))))))



(eval-when (:load-toplevel :execute :compile-toplevel)
  (defun column-load-mapping (column)
    "Map COLUMN from a database record into internal form.

Used in `DEFRECORD', qv."
    (let ((key (make-keyword (lisp-to-db-name (car column)))))
      (list (make-keyword (string (car column)))
            `(column-load-value (getf record ,key) ,(make-keyword (string (second column)))))))

  (defun column-save-mapping (column)
    (let ((slot (first column)))
      `(column-save-value ,slot ,(make-keyword (string (second column))))))

  (defun column-normalizer (column)
    (let ((name (intern (string (car column)))))
      (list name
            (list 'and name
                  (ecase (make-keyword (string (second column)))
                    (:string `(typecase ,name (string ,name) (t (princ-to-string ,name))))
                    (:keyword `(make-keyword (string ,name)))
                    (:yornp `(or ,name t)) ; T or NIL
                    (:number `(etypecase ,name
                                (number ,name)
                                (string (parse-number ,name))))
                    (:json name)        ; TODO
                    (:uri name)         ; TODO
                    (:color24 `(etypecase ,name
                                 (color24 ,name)
                                 (string (parse-color24 ,name))
                                 (integer (integer-to-color24 ,name))))
                    (:uuid `(etypecase ,name
                              (uuid:uuid ,name)
                              (vector (uuid:byte-array-to-uuid ,name))
                              (number (uuid:byte-array-to-uuid
                                       (integer-to-byte-vector ,name)))
                              (string (uuid:byte-array-to-uuid
                                       (cl-base64:base64-string-to-usb8-array ,name)))))
                    (:timestamp `(etypecase ,name
                                   (timestamp ,name)
                                   (number (universal-to-timestamp ,name))
                                   (string (parse-timestring ,name))))))))))



(defun defrecord/load-record (name columns)
  `(defmethod load-record ((class (eql ',name)) record)
     (,(intern (concatenate 'string "MAKE-" (string name)))
       ,@(mapcan #'column-load-mapping columns))))

(defun arrange-columns+values-for-find (columns+values column-definitions)
  (when columns+values
    (loop for (column value) on columns+values by #'cddr
       for column-def = (assoc column column-definitions :test #'string=)
       do (unless column-def
            (error "Can't search on unknown column ~:(~a~); ~
columns are ~{~:(~a~)~^, ~}" column (mapcar #'car column-definitions)))
       append (list (lisp-to-db-name column)
                    (column-save-value value
                                       (make-keyword (string (second column-def))))))))

(defun defrecord/find-record (name table columns)
  `(defmethod find-record ((class (eql ',name)) &rest columns+values)
     (load-record ',name (apply #'db-select-single-record
                                ,(lisp-to-db-name table)
                                (arrange-columns+values-for-find
                                 columns+values ',columns)))))

(defun defrecord/find-records (name table columns)
  `(defmethod find-records ((class (eql ',name)) &rest columns+values)
     (mapcar (lambda (record) (load-record ',name record))
             (apply #'db-select-records-simply ,table
                    (arrange-columns+values-for-find
                     columns+values ',columns)))))

(defun defrecord/before-save-normalize (name columns)
  `(defmethod before-save-normalize ((object ,name))
     (with-slots ,(mapcar #'car columns) object
       (setf ,@(mapcan #'column-normalizer columns)))))

(defun get-last-insert-id ()
  (let ((id-query (cl-dbi:prepare
                   *dbi-connection*
                   "SELECT LAST_INSERT_ID();")))
    (cl-dbi:execute id-query)
    (caar (cl-dbi:fetch-all id-query))))

(defun defrecord/save-record/insert (id-accessor table columns)
  `(progn
     ,(when (string-equal (caar columns) "UUID")
        `(setf (,id-accessor object) (uuid:make-v4-uuid)))
     (before-save-normalize object)
     (let* ((query (cl-dbi:prepare
                    *dbi-connection*
                    ,(format nil "INSERT INTO `~a` (~{`~a`~^, ~})~
~:* VALUES (~{?~*~^, ~});"
                             table
                             (mapcar (compose #'lisp-to-db-name #'car) columns)))))
       (v:info :db "Inserting new record ~s" (type-of object))
       (with-slots ,(mapcar #'car columns) object
         (cl-dbi:execute query
                         ,@(mapcar #'column-save-mapping columns))))
     ,(when (string-equal (caar columns) "ID")
        `(setf (,id-accessor object) (get-last-insert-id)))))

(define-condition update-nil (condition) ())

(defun defrecord/save-record/update (table columns)
  `(let ((query (cl-dbi:prepare
                 *dbi-connection*
                 ,(format nil "UPDATE `~a` ~
 SET ~{`~a` = ?~^, ~} WHERE `~a` = ?;"
                          table
                          (mapcar (compose #'lisp-to-db-name #'car)
                                  (rest columns))
                          (lisp-to-db-name (caar columns))))))
     (with-slots ,(mapcar #'car columns) object
       (v:info :db "Updating record in ~a ∀ ~a=~a" ,table
               ,(lisp-to-db-name (caar columns))
               ,(caar columns))
       (cl-dbi:execute query
                       ,@(mapcar #'column-save-mapping (rest columns))
                       ,(column-save-mapping (car columns)))
       (let ((rows (cl-dbi:row-count *dbi-connection*)))
         (when (zerop rows)
           (signal 'update-nil))
         rows))))

(defun defrecord/save-record (name id-accessor database table columns )
  `(defmethod save-record ((object ,name))
     (with-dbi (,database)
       (if (null (,id-accessor object))
           ,(defrecord/save-record/insert id-accessor table columns)
           ,(defrecord/save-record/update table columns)))))

(defun defrecord/id-column-for (name columns)
  (when (or (string-equal "ID" (caar columns))
            (string-equal "UUID" (caar columns)))
    `(defmethod id-column-for ((type (eql ',name)))
       ',(caar columns))))

(defun defrecord/destroy-record (name id-accessor database table columns)
  (declare (ignore id-accessor))
  `(defmethod destroy-record ((object ,name))
     (with-dbi (,database)
       (let ((q (cl-dbi:prepare *dbi-connection*
                                ,(format nil "DELETE FROM ~a WHERE `~a` = ?"
                                         table
                                         (lisp-to-db-name (caar columns))))))
         (with-slots (,(caar columns)) object
           (cl-dbi:execute q ,(column-save-mapping (car columns))))
         (let ((rows (cl-dbi:row-count *dbi-connection*)))
           (when (zerop rows)
             (signal 'update-nil))
           rows)))))

(defun defrecord/save-record-with-id-column (name database table columns)
  (when (or (string-equal "ID" (caar columns))
            (string-equal "UUID" (caar columns)))
    (let ((id-accessor (intern (concatenate 'string (string name) "-"
                                            (string (caar columns))))))
      `(progn
         ,(defrecord/save-record name id-accessor database table columns)
         ,(defrecord/destroy-record name id-accessor database table columns)))))

(defun defrecord/find-reference (name column)
  `(defmethod find-reference
       ((object ,name)
        (reference (eql ,(make-keyword (string (first column))))))
     (find-record ',(fourth column)
                  ,(make-keyword (id-column-for (fourth column)))
                  (,(intern (concatenate 'string (string name)
                                         "-"
                                         (string (first column))))
                    object))))

(defun defrecord/find-reference-columns (name columns)
  (when (find-if (lambda (column) (< 2 (length column))) columns)
    (cons 'progn
          (loop for column in columns
             when (and (= 4 (length column))
                       (string-equal "REF" (third column)))
             collecting
               (defrecord/find-reference name column)))))

(defun defrecord/make-record (name)
  `(defmethod make-record ((class (eql ',name)) &rest columns+values)
     (let ((record (apply (function
                           ,(intern (concatenate 'string "MAKE-" (string name))))
                          columns+values)))
       (save-record record)
       record)))

(defun defrecord/column-to-json-pair (column)

  (list (intern (symbol-munger:lisp->camel-case (first column)) :keyword)
        (list 'jonathan.encode:%to-json
              (list (intern (concatenate 'string
                                         (string name) "-"
                                         (string (first column))))
                    basename))))

(defun defrecord/to-json (name columns)
  (let ((basename (if (string-begins "DB." (string name))
                      (intern (subseq (string name) 3))
                      name)))
    `(defmethod jonathan.encode:%to-json ((,basename ,name))
       (jonathan.encode:%to-json
        (list :|isA| ,(symbol-munger:lisp->studly-caps basename)
              ,@(mapcan 
                 #'defrecord/column-to-json-pair
                 columns))))))



(defmacro defrecord (name (database table &key pull) &rest columns)
  "Define a database-mapping object type NAME, for DATABASE and TABLE, with COLUMNS.

DATABASE  is the  symbolic name  of the  database, mapped  via `CONFIG';
eg, :friendly

TABLE is  the string table-name, exactly  as it exists in  the database;
eg, \"toots\"

PULL is NOT implemented; TODO.

PULL  is meant  to indicate  an infrequently-changed,  short table  (ie,
basically a  small enumeration) that  should be pulled into  local cache
up-front.

COLUMNS are a table of names, types, and foreign-key references, in the form:
 (LABEL TYPE &rest REFERENCE)

The LABEL  of a column is  mapped via `LISP-TO-DB-NAME'; it  is the Lisp
name which is essentially the same  as the SQL name, but with KEBAB-CASE
rather than snake_case.

When   present,  REFERENCE   is   the  symbol   REF   followed  by   the
record-type  (class) to  whose primary  key (ID  or UUID)  the reference
is made. NUMBER REF columns point to ID, UUID REF columns to UUID.

TYPE is one of the following:

@table
@item NUMBER
map to an integer or real column in the database
@item STRING
map to a CHAR, CHAR VARYING, or TEXT column, or ENUM
@item COLOR24
stored in the database as a 24-bit BINARY (3 bytes)
@item KEYWORD
map to a CHAR or CHAR VARYING column, or ENUM
@item UUID
stored as a 128-bit BINARY (16 bytes)
@item JSON
stored as a TEXT column, but parsed on loading via Jonathan
@item YORNP
a boolean, stored as (typically an enum) 'Y' or 'N'.
@item URI
stored as CHAR VARying or TEXT, parsed at load time as a PURI:URI.
@item TIMESTAMP
translates to a LOCAL-TIME:TIMESTAMP on loading.
@end table
"
  (declare (ignore pull)) ; TODO
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defstruct ,name
       ,@(mapcar #'car columns))
     (defmethod db-table-for ((class (eql ',name))) ,table)
     ,(defrecord/id-column-for name columns)
     ,(defrecord/make-record name)
     ,(defrecord/load-record name columns)
     ,(defrecord/find-record name table columns)
     ,(defrecord/find-records name table columns)
     ,(defrecord/before-save-normalize name columns)
     ,(defrecord/save-record-with-id-column name database table columns)
     ,(defrecord/to-json name columns)
     ,(defrecord/find-reference-columns name columns)))
