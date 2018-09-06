;;;; maintenance.lisp — systems maintenance functions
(in-package :tootsville)

(defparameter *maintenance-tasks-performed* nil)

(defendpoint (:get "/maintenance/" "text/plain")
  (list 401 nil "You are not the boss of me."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pretty-time (seconds)
    (cond
      ((< seconds 90)
       (format nil "~d second~:p" seconds))
      ((< seconds (* 90 60))
       (format nil "~d minutes" (round seconds 60)))
      ((< seconds (* 36 60 60))
       (format nil "~d hours" (round seconds (* 60 60))))
      (t (format nil "~d days" (round seconds (* 24 60 60)))))))

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
       (setf (getf (response-headers *response*) :content-type)
             "text/plain;charset=utf-8")
       (when-let (last (getf *maintenance-tasks-performed* ,task-sym))
         (when (> last (- (get-universal-time) ,finish-delay))
           (return
             ,(format nil "Task “~a” was performed less than ~a ago."
                      task-string (pretty-time (eval finish-delay))))))
       (when-let (last (getf *maintenance-tasks-performed* ,task-start-sym))
         (when (> last (- (get-universal-time) ,start-delay))
           (return
             ,(format nil "Task “~a” was started less than ~a ago."
                      task-string (pretty-time (eval start-delay))))))
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
     (with-maintenance-times (,label
                              ,name
                              ,start-delay ,finish-delay)
       ,@body)))

(define-maintenance-task quicklisp-update
    ("Updating the Quicklisp distribution"
     (* 20 60) (* 24 60 60))
  (ql:update-client)
  (ql:update-all-dists))

(define-maintenance-task hot-reload
    ("Reloading from local sources"
     (* 5 60) (* 30 60))
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (asdf:load-system :tootsville)))

(define-maintenance-task buildapp
    ("Recompiling Tootsville executable"
     (* 20 60) (* 3 60 60))
  (uiop:chdir (asdf:system-relative-pathname :tootsville "./"))
  (uiop:run-program "make Tootsville"
                    :output :string :error-output :output))

(define-maintenance-task reload-jscl
    ("Recompiling jscl.js"
     (* 20 60) (* 3 60 60))
  (uiop:chdir (asdf:system-relative-pathname :tootsville "./"))
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (uiop:chdir (asdf:system-relative-pathname :tootsville "./src/lib/jscl/"))
    (load (asdf:system-relative-pathname :tootsville "./src/lib/jscl/jscl.lisp"))
    (funcall (intern "BOOTSTRAP-CORE" :jscl/bootstrap))))
