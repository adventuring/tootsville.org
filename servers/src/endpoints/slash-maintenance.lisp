;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-maintenance.lisp    is     part    of
;;;; Tootsville
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

;;;; maintenance.lisp — systems maintenance functions
(in-package :Tootsville)

(defparameter *maintenance-tasks-performed* nil)

(defendpoint (:get "/maintenance/" "text/plain")
  (list 401 nil "You are not the boss of me."))

(defmacro with-continuable-errors-skipped (&body body)
  `(handler-case
       (progn ,@body)
     (serious-condition (c)
       (format t "…encountered a serious condition:~%~s~:*~%~a" c)
       (dolist (kind '(:ignore :continue :take-new :accept))
         (when (find-restart kind)
           (format t "~&Found a ~a restart; invoking." kind)
           (invoke-restart kind)))
       (format t "~&No “continuable” restarts found; aborting.")
       (abort))))

(defmacro with-standard-streams-to-string (&body body)
  `(with-output-to-string (s)
     (let ((*standard-output* s)
           (*error-output* s)
           (*trace-output* s))
       ,@body)))

(defmacro with-maintenance-times ((task-name task-string
                                             start-delay finish-delay)
                                  &body body)
  (let ((task-sym (make-keyword (string task-name)))
        (task-start-sym (make-keyword (concatenate 'string (string task-name)
                                                   (string :-started)))))
    `(block nil
       (when-let (last (getf *maintenance-tasks-performed* ,task-sym))
         (when (> last (- (get-universal-time) ,finish-delay))
           (return
             (list 420 ()
                   ,(format nil "Task “~a” was performed less than ~a ago."
                            task-string (human-duration (eval finish-delay)))))))
       (when-let (last (getf *maintenance-tasks-performed* ,task-start-sym))
         (when (> last (- (get-universal-time) ,start-delay))
           (return
             (list 420 ()
                   ,(format nil "Task “~a” was started less than ~a ago."
                            task-string (human-duration (eval start-delay)))))))
       (prog2
           (setf (getf *maintenance-tasks-performed* ,task-start-sym)
                 (get-universal-time))
           (with-standard-streams-to-string
             (with-continuable-errors-skipped
               ,@body))
         (setf (getf *maintenance-tasks-performed* ,task-sym)
               (get-universal-time))))))

(defmacro define-maintenance-task (label (name start-delay finish-delay)
                                   &body body)
  `(defendpoint (:post ,(concatenate 'string
                                     "/maintenance/"
                                     (string-downcase label)))
     nil
     (verbose:info :maintenance
                   ,(format nil "Maintenance request: ~a (~a)" label name))
     (with-maintenance-times (,label
                              ,name ,start-delay ,finish-delay)
       (list 200 () (progn ,@body)))))

(define-maintenance-task quicklisp-update
    ("Updating the Quicklisp client and distributions"
     (* 20 60) (* 24 60 60))
  (ql:update-client)
  (ql:update-all-dists))

(define-maintenance-task hot-reload
    ("Reloading from local sources"
     (* 5 60) (* 30 60))
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (asdf:load-system :Tootsville)))

(defvar *compilation* (make-string-output-stream))

(define-maintenance-task buildapp
    ("Recompiling Tootsville executable"
     (* 20 60) (* 3 60 60))
  (uiop:chdir (asdf:system-relative-pathname :Tootsville "./"))
  (setf *compilation* (make-string-output-stream))
  (format *compilation* "Running “make Tootsville” to rebuild executable.
Build starting at: ~a" (now))
  (uiop:run-program "make Tootsville"
                    :output *compilation* :error-output :output))

(define-maintenance-task buildapp/status
    ("Checking on the last BuildApp request" 10 10)
  (let ((s (get-output-stream-string *compilation*)))
    (if (plusp (length s))
        s
        "(No output.)")))

(define-maintenance-task reload-jscl
    ("Recompiling jscl.js"
     (* 20 60) (* 3 60 60))
  (uiop:chdir (asdf:system-relative-pathname :Tootsville "./"))
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (uiop:chdir (asdf:system-relative-pathname :Tootsville "./src/lib/jscl/"))
    (load (asdf:system-relative-pathname :Tootsville "./src/lib/jscl/jscl.lisp"))
    (funcall (intern "BOOTSTRAP-CORE" :jscl/bootstrap))))
