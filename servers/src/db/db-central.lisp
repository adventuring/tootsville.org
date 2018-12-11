(in-package :Tootsville)

(defun lisp-to-db-name (name)
  (cffi:translate-name-to-foreign (make-keyword (string name)) *package*))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (defun column-load-mapping (column index)
    (list (make-keyword (string (car column)))
          (ecase (make-keyword (string (second column)))
            (:string `(nth ,index record))
            (:keyword `(make-keyword (nth ,index record)))
            (:yornp `(ecase
                         (make-keyword (nth ,index record))
                       (:y t) (:n nil)))
            (:number `(nth ,index record))
	  (:json `(jonathan.decode:parse (nth ,index record)))
	  (:uri `(puri:parse-uri (nth ,index record)))
	  (:color24 `(integer-to-color24 (nth ,index record)))
            (:uuid `(uuid:byte-array-to-uuid 
                     (integer-to-byte-vector (nth ,index record)
                                             (make-array 16 :element-type 
                                                         '(unsigned-byte 8)))))
            (:timestamp `(universal-to-timestamp (nth ,index record))))))
  
  (defun column-save-mapping (column)
    (let ((slot (first column)))
      (ecase (make-keyword (string (second column)))
        (:string slot)
        (:keyword `(string ,slot))
        (:yornp `(if ,slot "Y" "N"))
        (:number slot)
        (:json `(jonathan.encode:to-json ,slot))
        (:uri `(puri:render-uri ,slot nil))
        (:color24 `(color24-to-integer ,slot))
        (:uuid `(vector-to-int ,slot))       
        (:timestamp `(timestamp-to-universal  ,slot))))))

(defun defrecord/load-record (name columns)
  `(defmethod load-record ((class (eql ',name)) row)
     (,(intern (concatenate 'string "MAKE-" (string name)))
       ,@(loop for i from 0 
            for column in columns
            appending (column-load-mapping column i)))))

(defun defrecord/find-record (name table)
  `(defmethod find-record ((class (eql ',name)) &rest columns+values)
     (load-record ',name (apply #'db-select-single-record 
                                ,(lisp-to-db-name table)
                                columns+values))))

(defun defrecord/find-records (name table)
  `(defmethod find-records ((class (eql ',name)) &rest columns+values)
     (mapcar #'load-record ',name
	   (apply #'db-select-records-simply ,(lisp-to-db-name table)
		columns+values))))

(defun defrecord/row<-record (name columns)
  `(defmethod row<-record  ((object ,name))
     (with-slots ,(mapcar #'car columns) object
       (list ,@(mapcar #'column-save-mapping columns)))))

(defun defrecord/save-record (name id-accessor database table columns )
  `(defmethod save-record ((object ,name))
     (with-dbi (,database)
       (if (null (,id-accessor object))
           (progn 
             (when (string-equal (caar columns) "UUID")
               (setf (,id-accessor object) (uuid:make-v4-uuid)))
             (let* ((query (cl-dbi:prepare 
                            *dbi-connection*
                            ,(format nil "INSERT INTO `~a` (~{`~a`~^, ~})~
~:*VALUES (~{?~*~^, ~})"
                                     table
                                     (mapcar (compose #'lisp-to-db-name #'car) columns)))))
               (cl-dbi:execute query
                               ,@(mapcar #'column-save-mapping columns)))
             (when (string-equal (caar columns) "ID")
               (let* ((id-query (cl-dbi:prepare 
                                 *dbi-connection*
                                 "SELECT LAST_INSERT_ID()"))
                      (id-results (cl-dbi:execute id-query)))
                 (setf (,id-accessor object) (caar (cl-dbi:fetch-all 
                                                    id-results))))))
           (let ((query (cl-dbi:prepare
                         *dbi-connection*
                         ,(format nil "UPDATE `~a` ~
SET ~{`~a` = ?~^, ~} WHERE ~a = ?"
                                  table
                                  (mapcar (compose #'lisp-to-db-name #'car)
                                          (rest columns))
                                  (lisp-to-db-name (caar columns))))))
             (cl-dbi:execute query
                             ,@(mapcar #'column-save-mapping (rest columns))
                             ,(caar columns)))))))

(defun defrecord/save-record-with-id-column (name database table columns)
  (when (or (string-equal "ID" (caar columns))
            (string-equal "UUID" (caar columns)))
    (let ((id-accessor (intern (concatenate 'string (string name) "-" 
                                            (string (caar columns))))))
      `(progn
         ,(defrecord/save-record name id-accessor database table columns )))))

(defun defrecord/find-reference (name column)
  `(defmethod find-reference
       ((object ,name)
        (reference (eql ,(make-keyword (string (first column))))))
     (find-record ',(fourth column)
	        ,(lisp-to-db-name (first column)) 
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
     (let ((record (apply #'make-instance ',name columns+values)))
       (save-record record)
       record)))

(defmacro defrecord (name (database table) &rest columns)
  `(progn 
     (defstruct ,name
       ,@(mapcar #'car columns))
     ,(defrecord/make-record name)
     ,(defrecord/load-record name columns)
     ,(defrecord/find-record name table)
     ,(defrecord/find-records name table)
     ,(defrecord/row<-record name columns)
     ,(defrecord/save-record-with-id-column name database table columns)
     ,(defrecord/find-reference-columns name columns)))
