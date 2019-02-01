;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/maria.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)

(declaim (optimize (speed 3)))

(defvar *dbi-connection* :not-connected
  "The connection selected by a WITH-MARIA block")

(defvar *db* :friendly
  "The default database moniker")

(defun db-config (&optional (moniker *db*))
  (or (cdr (assoc (the symbol moniker) (config :databases)))
      (and (load-config)
           (cdr (assoc (the symbol moniker) (config :databases))))
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
  (destructuring-bind (table &key pull)
      (if (consp table) table (list table))
    (declare (ignore pull))
    (let ((results
           (with-dbi (*db*)
             (destructuring-bind (columns values) (split-plist columns+values)
               (let* ((query (cl-dbi:prepare tootsville::*dbi-connection*
                                             (build-simple-column-query
                                              table column columns)))
                      (result-set (apply #'cl-dbi:execute query values)))
                 (with-memcached-query (*db* (slot-value query 'dbi.driver::sql) values)
                   (cl-dbi:fetch-all result-set)))))))
      (cond ((= 1 (length (the proper-list results)))
             (caar results))
            ((zerop (length results))
             (error 'not-found :the (cons table columns+values)))
            (t (error "Found ~p record~:p when expecting one" (length results)))))))

(defun db-select-single-record (table &rest columns+values)
  "Select a single record from TABLE where columns = values as in COLUMNS+VALUES.

Calls `DB-SELECT-RECORDS-SIMPLY'  which in  turn may use  MemCacheD when
it's available.

Signals an error if more than one record is returned.

Signals NOT-FOUND if none are found."
  (destructuring-bind (table &key pull)
      (if (consp table) table (list table))
    (let ((results (apply #'db-select-records-simply (list table :pull pull)
                          columns+values)))
      (cond ((= 1 (length (the proper-list results)))
             (first results))
            ((zerop (length results))
             (error 'not-found :the (cons table columns+values)))
            (t (error "Found ~p record~:p when expecting one" (length results))))
      (first results))))

(defun db-select-records-simply (table &rest columns+values)
  "Query TABLE where columns = values from the plist COLUMNS+VALUES.

Returns all results in a list, so don't use it with a (potentially) large set.

Uses MemCache when available."
  (destructuring-bind (table &key pull)
      (if (consp table) table (list table))
    (declare (ignore pull))
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
              (cl-dbi:fetch-all result-set)))))))

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




(defun connect-maria ()
  "Make a connection to MariaDB.

This  ensures that  it is  reachable,  and that  there is  at least  one
connection in the pool."
  (with-dbi (:friendly)          ; XXX Each DB defined in the config
    (let ((q (cl-dbi:prepare *dbi-connection* "SELECT 1 AS one;")))
      (cl-dbi:execute q)
      (assert (equalp '((:|one| 1)) (cl-dbi:fetch-all q)))))
  t)




(defmacro with-cluster-wide-lock-held ((lock-string &key (timeout nil)
                                                    (if-not-locked :wait))
                                       &body body)
  "Execute BODY in a dynamic context owning database lock LOCK-STRING.

LOCK-STRING is  passed to the MariaDB  server and a global  lock by that
name is obtained via mySQL function GET_LOCK(STRING), if possible.

If the lock is busy, IF-NOT-LOCKED determines the next action.

@table

@item :WAIT

Wait for  up to TIMEOUT seconds  for the lock  to be freed. If  the lock
cannot  be obtained  within TIMEOUT  seconds,  signal an  error of  type
CLUSTER-WIDE-LOCK-BUSY-ERROR. If TIMEOUT is  NIL, wait indefinitely until
the lock can be obtained.

@item :SKIP

Skip BODY and return NIL.

@item :WARN

Signal a warning of  type CLUSTER-WIDE-LOCK-BUSY-WARNING, then skip BODY
and return NIL.

@item :ERROR

Signal an error of type CLUSTER-WIDE-LOCK-BUSY-ERROR.

@end table

Returns the values of BODY.
"
  (let (($lock (gensym "LOCK-")))
    `(let ((,$lock (get-mariadb-lock ,lock-string
                                     :if-not-locked ,if-not-locked
                                     :timeout ,timeout)))
       (unwind-protect
            (progn ,@body)
         (yield-mariadb-lock ,$lock)))))


(defun get-mariadb-lock (lock-string &key if-not-locked timeout)
  "Obtain database lock LOCK-STRING.

See `WITH-CLUSTER-WIDE-LOCK-HELD' for a practical use of this.

LOCK-STRING is  passed to the MariaDB  server and a global  lock by that
name is obtained via mySQL function GET_LOCK(STRING), if possible.

If the lock is busy, IF-NOT-LOCKED determines the next action.

@table

@item :WAIT

Wait for  up to TIMEOUT seconds  for the lock  to be freed. If  the lock
cannot  be obtained  within TIMEOUT  seconds,  signal an  error of  type
CLUSTER-WIDE-LOCK-BUSY-ERROR. If TIMEOUT is  NIL, wait indefinitely until
the lock can be obtained.

@item :SKIP

Skip BODY and return NIL.

@item :WARN

Signal a warning of  type CLUSTER-WIDE-LOCK-BUSY-WARNING, then skip BODY
and return NIL.

@item :ERROR

Signal an error of type CLUSTER-WIDE-LOCK-BUSY-ERROR.

@end table

Returns an  opaque identifier that  can be passed to  `YIELD-DB-LOCK' to
release the lock.

LOCK-NAME is case-insensitive.
"
  (let* ((query (cl-dbi:prepare *dbi-connection*
                                "SELECT GET_LOCK(?, ?)"))
         (result-set (cl-dbi:execute query lock-name timeout)))
    (ecase (caar result-set)
      (0 ; timed out
       )
      (1 ; success
       )
      (nil ; error other than timeout
       ))))
(defun yield-mariadb-lock (lock-name)
  "Release the lock identified by LOCK-NAME.

LOCK-NAME is case-insensitive."
  (let* ((query (cl-dbi:prepare *dbi-connection*
                                "SELECT RELEASE_LOCK(?)"))
         (result-set (cl-dbi:execute query lock-name)))
    (ecase (caar result-set)
      (0 ; not owned by us
       )
      (1 ; success
       )
      (nil ; was not locked
       ))))
