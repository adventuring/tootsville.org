(in-package :Tootsville)


(defun query-to-memcache-key (db prepared args)
  (let ((query (format nil "~a➕~{~a~^✜~}" prepared args)))
    (if (< (length query) 128)
        (format nil "~:(~a~):~a" db query)
        (format nil "~:(~a~):~a…#~a"
                db (subseq query 0 100) (sha1-hex query)))))

(defmacro with-memcached-query ((db query args) &body body)
  (let (($db (gensym "DB-"))
        ($query (gensym "QUERY-"))
        ($key (gensym "KEY-"))
        ($value (gensym "VALUE-"))
        ($args (gensym "ARGS-")))
    `(if cl-memcached:*memcache*
         (let* ((,$db ,db) (,$query ,query) (,$args ,args)
                (,$key (query-to-memcache-key ,$db ,$query ,$args)))
           (if-let (,$value (cl-memcached:mc-get-value ,$key))
             (apply #'values (read-from-string (trivial-utf-8:utf-8-bytes-to-string ,$value)))
             (let ((,$value (multiple-value-list (progn ,@body))))
               (cl-memcached:mc-store ,$key (trivial-utf-8:string-to-utf-8-bytes (princ-to-string ,$value)))
               (apply #'values ,$value))))
         (progn ,@body))))



(defvar *dbi-connection* :not-connected
  "The connection selected by a WITH-MARIA block")

(defvar *db* :friendly
  "The default database moniker")

(defun db-config (&optional (moniker *db*))
  (or (cdr (assoc moniker (config :databases)))
      (and (load-config)
           (cdr (assoc moniker (config :databases))))
      (error "No database configuration for ~s" moniker)))

(defmacro with-dbi ((moniker) &body body)
  (let ((connection$ (gensym "DBI-")))
    `(let* ((,connection$ (apply #'cl-dbi:connect-cached (db-config ,moniker)))
            (*dbi-connection* ,connection$))
       (unwind-protect
            ,@body
         (cl-dbi:disconnect ,connection$)))))



(defun split-plist (plist)
  "Split a PLIST into two lists, of keys and values."
  (loop for (key value) on plist by #'cddr
     collect key into keys
     collect value into values
     finally (return (list keys values))))

(defun build-simple-query (table columns)
  (format nil "SELECT * FROM `~a`~@[ WHERE ~{`~a`=?~^ AND ~}~];" table columns))

(defun build-simple-column-query (table column columns)
  (format nil "SELECT `~a` FROM `~a`~@[ WHERE ~{`~a`=?~^ AND ~}~];"
          table column columns))



(defun db-select-single-column (table column &rest columns+values)
  "Select COLUMN from TABLE where columns = values as in plist COLUMNS+VALUES.

Expects to find only one row and return the one column value as an atom.

Signal  an  error if more rows are returned.

Signals NOT-FOUND if none are found.

Uses MemCacheD when available."
  (let ((results
         (with-dbi (*db*)
           (destructuring-bind (columns values) (split-plist columns+values)
             (let* ((query (cl-dbi:prepare tootsville::*dbi-connection*
                                           (build-simple-column-query
                                            table column columns)))
                    (result-set (apply #'cl-dbi:execute query values)))
               (with-memcached-query (*db* (slot-value query 'dbi.driver::sql) values)
                 (cl-dbi:fetch-all result-set)))))))
    (cond ((= 1 (length results))
           (caar results))
          ((zerop (length results))
           (error 'not-found :the (cons table columns+values)))
          (t (error "Found ~p record~:p when expecting one" (length results))))))

(defun db-select-single-record (table &rest columns+values)
  "Select a single record from TABLE where columns = values as in COLUMNS+VALUES.

Calls `DB-SELECT-RECORDS-SIMPLY'  which in  turn may use  MemCacheD when
it's available.

Signals an error if more than one record is returned.

Signals NOT-FOUND if none are found."
  (let ((results (apply #'db-select-records-simply table columns+values)))
    (cond ((= 1 (length results))
           (first results))
          ((zerop (length results))
           (error 'not-found :the (cons table columns+values)))
          (t (error "Found ~p record~:p when expecting one" (length results))))
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
            (with-memcached-query (*db* (slot-value query 'dbi.driver::sql) values)
              (cl-dbi:fetch-all result-set))))
        (let* ((query (cl-dbi:prepare tootsville::*dbi-connection*
                                      (format nil "SELECT * FROM `~a`" table)))
               (result-set (cl-dbi:execute query)))
          (with-memcached-query (*db* (slot-value query 'dbi.driver::sql) nil)
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
