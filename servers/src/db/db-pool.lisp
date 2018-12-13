(in-package :Tootsville)


(defun query-to-memcache-key (db query)
  (if (< (length query) 128)
      (format nil "~:(~a~):~a" db query)
      (format nil "~:(~a~):~a…#~a"
              db (subseq query 0 100) (sha1-hex query))))

(defmacro with-memcached-query ((db query) &body body)
  (let (($db (gensym "DB-"))
        ($query (gensym "QUERY-"))
        ($key (gensym "KEY-"))
        ($value (gensym "VALUE-")))
    `(if cl-memcached:*memcache*
         (let* ((,$db ,db) (,$query ,query) 
                (,$key (query-to-memcache-key ,$db ,$query)))
           (if-let (,$value (cl-memcached:mc-get-value ,$key))
             (apply #'values (read-from-string ,$value))
             (let ((,$value (values-list (progn ,@body))))
               (cl-memcached:mc-store ,$key (princ-to-string ,$value))
               (apply #'values ,$value))))
         (progn ,@body))))



(defvar *dbi-connection* :not-connected
  "The connection selected by a WITH-MARIA block")

(defun db-config (&optional (moniker *db*))
  (cdr (assoc moniker (config :databases))))

(defmacro with-dbi ((moniker) &body body)
  (let ((connection$ (gensym "DBI-")))
    `(let* ((,connection$ (apply #'cl-dbi:connect-cached (db-config ,moniker)))
            (*dbi-connection* ,connection$))
       (unwind-protect
            (cl-dbi:with-transaction *dbi-connection*
              ,@body)
         (cl-dbi:disconnect ,connection$)))))

(defvar *db* :friendly)

(defun split-plist (plist)
  (loop for (key value) on plist by #'cddr
     collect key into keys
     collect value into values
     finally (return (list keys values))))

(defun build-simple-query (table columns)
  (format nil "SELECT * FROM `~a` ~@[WHERE ~{`~a` = ? ~}~]" table columns))

(defun build-simple-column-query (table column columns)
  (format nil "SELECT `~a` FROM `~a` ~@[WHERE ~{`~a` = ? ~}~]"
          table column columns))



(defun db-select-single-column (table column &rest columns+values)
  "Select COLUMN from TABLE where columns = values as in plist COLUMNS+VALUES.

Expects to find only one row and return the one column value as an atom.
Signal  an assertion error if more rows are returned.

Uses MemCacheD when available."
  (let ((results
         (with-dbi (*db*)
           (destructuring-bind (columns values) (split-plist columns+values)
             (let* ((query (cl-dbi:prepare tootsville::*dbi-connection* 
                                           (build-simple-column-query 
                                            table column columns)))
                    (result-set (apply #'cl-dbi:execute query values))) 
               (with-memcached-query (*db* query)
                 (cl-dbi:fetch-all result-set)))))))
    (assert (= 1 (length results)) (results)
            "Expected a single record; got ~:d" (length results))
    (caar results)))

(defun db-select-single-record (table &rest columns+values)
  "Select a single record from TABLE where columns = values as in COLUMNS+VALUES.

Calls `DB-SELECT-RECORDS-SIMPLY'  which in  turn may use  MemCacheD when
it's available.

Signals an assertion error if more than one record is returned."
  (let ((results (apply #'db-select-records-simply table columns+values)))
    (assert (= 1 (length results)) (results)
            "Expected a single record; got ~:d" (length results))
    (first results)))

(defun db-select-records-simply (table &rest columns+values)
  "Query TABLE where columns = values from the plist COLUMNS+VALUES.

Returns all results in a list, so don't use it with a (potentially) large set.

Uses MemCache when available."
  (with-dbi (*db*)
    (if columns+values
        (destructuring-bind (columns values) (split-plist columns+values)
          (let* ((query (cl-dbi:prepare tootsville::*dbi-connection* 
                                        (build-simple-query table columns)))
                 (result-set (apply #'cl-dbi:execute query values))) 
            (with-memcached-query (*db* query)
              (cl-dbi:fetch-all result-set))))
        (let* ((query (cl-dbi:prepare tootsville::*dbi-connection*
                                      (format nil "SELECT * FROM `~a`" table)))
               (result-set (cl-dbi:execute query)))
          (with-memcached-query (*db* query)
            (cl-dbi:fetch-all result-set))))))

(defmacro do-db-records-simply ((record-var table &rest columns+values)
                                &body body)
  "Iterate RECORD-VAR over TABLE where columns = values as in plist COLUMNS+VALUES.

Selects one record at a time from TABLE. Does not use MemCacheD."
  (let (($columns (gensym "COLUMNS-"))
        ($values (gensym "VALUES-"))
        ($query (gensym "QUERY-"))
        ($result-set (gensym "RESULT-SET-")))
    `(with-dbi (*db*)
       (destructuring-bind (,$columns ,$values) (split-plist (list ,@columns+values))
         (let* ((,$query (cl-dbi:prepare tootsville::*dbi-connection* 
                                         (build-simple-query ,table ,$columns)))
                (,$result-set (apply #'cl-dbi:execute ,$query ,$values))) 
           (loop for ,record-var = (cl-dbi:fetch ,$result-set)
              while ,record-var
              do (progn ,@body)))))))
