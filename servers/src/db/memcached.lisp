;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/memcached.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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



(defun query-to-memcache-key (db prepared args)
  (let ((query (format nil "~a➕~{~a~^✜~}" prepared args)))
    (if (< (length query) 128)
        (format nil "~:(~a~):~a" db query)
        (format nil "~:(~a~):~a…#~a"
                db (subseq query 0 100) (sha1-hex query)))))

(defmacro with-memcached-query ((db query args &key (timeout 120)) &body body)
  (let (($db (gensym "DB-"))
        ($query (gensym "QUERY-"))
        ($key (gensym "KEY-"))
        ($value (gensym "VALUE-"))
        ($args (gensym "ARGS-")))
    `(if cl-memcached:*memcache*
         (let* ((,$db ,db) (,$query ,query) (,$args ,args)
                (,$key (query-to-memcache-key ,$db ,$query ,$args))
                (cl-memcached:*mc-use-pool* t)
                (cl-memcached:*mc-default-encoding* :utf-8)
                (*print-readably* t)
                (*print-pretty* nil)
                (*print-right-margin* 120))
           (if-let (,$value (cl-memcached:mc-get-value ,$key))
             (apply #'values (read-from-string
                              (trivial-utf-8:utf-8-bytes-to-string ,$value)))
             (let ((,$value (multiple-value-list (progn ,@body))))
               (cl-memcached:mc-store ,$key
                                      (trivial-utf-8:string-to-utf-8-bytes
                                       (format nil "~s" ,$value))
                                      :timeout ,timeout
                                      :noreply t)
               (apply #'values ,$value))))
         (progn ,@body))))



(defun connect-cache ()
  (dolist (server (config :cache))
    (cl-memcached:make-memcache :ip (extract server :ip)
                                :port (or (extract server :port) 11211)
                                :name (extract server :name)))
  (length (config :cache)))



(defpost memcached-quick-test ()
  "Quick test provided by CL-MemCacheD library"
  (handler-case
      (cl-memcached::mc-quick-test)
    (cl-memcached::memcached-server-unreachable (c)
      (warn (princ-to-string (the condition c))))))

(defpost memcached-random-number-test ()
  "Store and fetch a random number"
  (handler-case
      (let ((n (princ-to-string (the (integer 0 *) 
                                     (random (the (integer 0 *) 
                                                  (expt 2 63))))))
            (key (format nil "post.~a.~a" (machine-instance) (cluster-name))))
        (cl-memcached:mc-set key n)
        (let ((m (cl-memcached:mc-get key)))
          (assert (equal n m) ()
                  "MemCacheD did not return the random number (~x) for key ~a"
                  n key))
        (cl-memcached:mc-del key))
    (cl-memcached::memcached-server-unreachable (c)
      (warn (princ-to-string c)))))



(defun powerset (list)
  (if list
      (mapcan (lambda (el) (list (cons (car list) el) el))
              (powerset (cdr list)))
      '(())))

(defun erase-all-memcached-for (name &rest columns+values)
  (let ((db (second (database-for name)))
        (table (db-table-for name))
        (columns (plist-keys columns+values)))
    (loop for set in (powerset columns)
       for columns+values-subset = (loop for column in set
                                      collecting column
                                      collecting (getf column columns+values))
       do (cl-memcached:mc-del (query-to-memcache-key db table columns+values-subset)))))
