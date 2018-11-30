(in-package :Tootsville)

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
	    (:json `(jonathan:parse-json (nth ,index record)))
	    (:uri `(puri:parse-uri (nth ,index record)))
	    (:color24 `(integer-to-color24 (nth ,index ,record)))
            (:uuid `(uuid:byte-array-to-uuid 
                     (integer-to-byte-vector (nth ,index record)
                                             (make-array 16 :element-type '(unsigned-byte 8)))))
            (:timestamp `(universal-to-timestamp (nth ,index record))))))
  
  (defun column-save-mapping (column)
    (let ((slot (first column)))
      (ecase (make-keyword (string (second column)))
        (:string slot)
        (:keyword `(string ,slot))
        (:yornp `(if ,slot "Y" "N"))
        (:number slot)
	(:json `(jonathan:json-encode ,slot))
	(:uri `(puri:uri-string ,slot))
	(:color24 `(color24-to-integer ,slot))
	(:uuid `(vector-to-int))       
        (:timestamp `(timestamp-to-universal  ,slot))))))

(defmacro defrecord (name (database table) &rest columns)
  `(progn 
     (defstruct ,name
       ,@(mapcar #'car columns))
     (defmethod load-record ((class (eql ',name)) row)
       (,(intern (concatenate 'string "MAKE-" (string name)))
         ,@(loop for i from 0 
              for column in columns
              appending (column-load-mapping column i))))
     (defmethod find-record ((class (eql ',name)) &rest columns+values)
       (load-record ',name (apply #'select-single-record ,(string-downcase table)
                                  columns+values)))
     (defmethod row<-record  ((object ,name))
       (with-slots (object ,@(mapcar #'car
                                     columns))
           (list ,@(mapcar #'column-save-mapping columns))))
     ,(when (or (string-equal "ID" (caar columns))
                (string-equal "UUID" (caar columns)))
        `(defmethod save-record ((object ,name))
           (with-db (,database)
             (let ((query (cl-dbi:prepare 
                           (format nil "UPDATE `~a` SET ~{`~a` = ?~^, ~} WHERE ~a = ?"
                                   ,table
                                   ,@(mapcar (lambda (column)
                                               (string-downcase (car column)))
                                             (rest columns))
                                   ,(string-downcase (caar columns))))))
               (cl-dbi:execute query
                               ,@(mapcar #'car (rest columns))
                               ,(caar columns))))))))
