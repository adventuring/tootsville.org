(in-package #:thread-pool-taskmaster)

(declaim (optimize (safety 3) (speed 3)))

(defclass thread-pool-taskmaster (one-thread-per-connection-taskmaster)
  ((thread-pool :accessor taskmaster-thread-pool))
  (:default-initargs
   :worker-thread-name-format "Web Worker ~a")
  (:documentation
   "A taskmaster that uses a thread pool to dispatch incoming requests."))

(defconstant +threads-per-core+ 2
  "Must be an (UNSIGNED-BYTE 15) and non-zero. 2 seems nice?")

(defconstant +single-core-threads+ 4
  "More threads than otherwise expected on a single-core machine.")

(defconstant +max-queue-size-for-thread-pool+ #x100)

(declaim (type (integer (0) (#. (expt 2 15))) +threads-per-core+))
(declaim (type (integer (0) (#. (expt 2 15))) +single-core-threads+))

(defun name-all-threads-idle (taskmaster)
  (loop for thread in (slot-value
                       (slot-value (taskmaster-thread-pool taskmaster)
                                   'cl-threadpool::threads)
                       'cl-threadpool::threads)
     for i fixnum from 1
     with count = (taskmaster-max-thread-count taskmaster)
     do (setf (sb-thread:thread-name thread)
              (format nil "Idle Web Worker (#~d of ~d)" i count))))

(defun swank-connected-p ()
  (and (find-package "SWANK")
       (ignore-errors
         (funcall (coerce (intern "CONNECTION-INFO" :swank) 'function)))))

(defmethod initialize-instance :after ((taskmaster thread-pool-taskmaster)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (taskmaster-thread-pool taskmaster)
        (cl-threadpool:make-threadpool
         (taskmaster-max-thread-count taskmaster)
         :max-queue-size +max-queue-size-for-thread-pool+
         :name "Web Workers"
         :resignal-job-conditions (not (swank-connected-p))))
  (cl-threadpool:start (taskmaster-thread-pool taskmaster))
  (name-all-threads-idle taskmaster))

(defmethod shutdown ((taskmaster thread-pool-taskmaster))
  (when-let (pool (taskmaster-thread-pool taskmaster))
    ;; NB: harmless to call more than once.
    (setf (taskmaster-thread-pool taskmaster) nil)
    ;; Haven't actually seen any errors, but seems wise to be safe here,
    ;; since we're about to lose the only reference to it.
    (ignore-errors (cl-threadpool:stop pool)))
  (call-next-method))

(define-memo-function cores*threads-per-core (cores)
  (declare (type (integer 0 #.(expt 2 15)) +threads-per-core+ cores))
  (if (= 1 cores)
      +single-core-threads+
      (the (unsigned-byte 63) (* (the (unsigned-byte 15) +threads-per-core+)
                                 (the (unsigned-byte 15) cores)))))

(defmethod taskmaster-max-thread-count ((taskmaster thread-pool-taskmaster))
  (cores*threads-per-core (processor-count)))

(defmethod taskmaster-thread-count ((taskmaster thread-pool-taskmaster))
  (if (taskmaster-thread-pool taskmaster)
      (taskmaster-max-thread-count taskmaster)
      0))

(defmethod taskmaster-max-accept-count ((taskmaster thread-pool-taskmaster))
  (the fixnum (1+ (the fixnum (cores*threads-per-core (processor-count))))))

(defparameter *mulligans* 5)

(defparameter *developmentp* nil)

(defmacro with-mulligan-handlers ((name mulligan) &body body)
  `(handler-bind
       (#+sbcl
        (Sb-Bsd-Sockets:Bad-File-Descriptor-Error
         (lambda (condition)
           (verbose:fatal '(:thread-pool-worker :peer-gone)
                          "Error signalled: worker ~a: ~
SB-BSD-Sockets:Bad-File-Descriptor-Error:~%~a"
                          ,name condition)
           (invoke-restart 'abandon)))
        (error
         (lambda (condition)
           (verbose:fatal '(:thread-pool-worker :worker-error)
                          "Error signalled: worker ~a: ~:(~a~)~%~a"
                          ,name (class-of condition) condition)
           (cond
             (*developmentp*
              (signal condition))
             ((plusp (the fixnum ,mulligan))
              (verbose:info '(:thread-pool-worker :worker-mulligan)
                            "With ~r mulligan~:p left: Trying again (~a stopped by ~:(~a~) ~a)"
                            ,mulligan ,name (class-of condition) condition)
              (decf (the fixnum ,mulligan))
              (invoke-restart 'restart))
             (t
              (verbose:info '(:thread-pool-worker :work-abandoned)
                            "Out of mulligans, abandoning ~a" ,name)))))
        (condition
         (lambda (condition)
           (verbose:debug '(:thread-pool-worker :worker-signal :work-abandoned)
                          "Condition signalled: worker ~a signal ~:(~a~)~%~a"
                          ,name (class-of condition) condition)
           (invoke-restart 'abandon))))
     ,@body))

(defmacro with-pool-thread-restarts ((name) &body body)
  (let ((restart-top (gensym "RESTART-TOP-"))
        (mulligan (gensym "MULLIGAN-")))
    `(tagbody ,restart-top
        (let ((,mulligan *mulligans*))
          (restart-bind
              ((restart (lambda () (go ,restart-top))
                 :report-function (lambda (s)
                                    (princ (concatenate 'string "Restart " ,name) s)))
               (abandon #'null
                 :report-function (lambda (s)
                                    (princ (concatenate 'string "Abandon " ,name) s)))
               (continue (lambda () (go ,restart-top))
                 :report-function (lambda (s)
                                    (princ "(synonym for Restart)" s)))
               (abort #'null
                 :report-function (lambda (s)
                                    (princ "Skip this job (lose it)" s))))
            (with-mulligan-handlers (,name ,mulligan)
              ,@body))))))

(defmacro named-thread-pool-runner ((&key (name "Thread pool worker")) &body body)
  #+sbcl
  (let ((idle-name (gensym "IDLE-NAME-"))
        (thread-name (gensym "THREAD-NAME-")))
    `(lambda ()
       (let* ((,idle-name (thread-name (current-thread)))
              (,thread-name ,name))
         (setf (sb-thread:thread-name (current-thread)) ,thread-name)
         (unwind-protect
              (with-pool-thread-restarts (,thread-name)
                (verbose:info '(:threadpool-worker :web-worker :worker-start) "~a working" ,thread-name)
                ,@body)
           (verbose:info '(:threadpool-worker :web-worker :worker-finish) "~a done" ,thread-name)
           (setf (sb-thread:thread-name (current-thread)) ,idle-name)))))
  #-sbcl
  `(lambda () ,@body))

(defun client-as-string (socket)
  "A helper function which returns the client's address and port as a string
 and tries to act robustly in the presence of network problems.  This will
 also check the current HTTP request context to see if it's a forwarded
 connection, and report that, as well.

This version, unlike Hunchentoot's builtins, should work with IPv6 🤞"
  (if-let ((f-f (and (boundp 'hunchentoot::*request*)
                     (assoc :x-forwarded-for (hunchentoot::headers-in*)))))
    (format nil "~a (via ~a:~d; local ~a:~d)"
            (cdr f-f)
            (usocket::host-to-hostname (usocket:get-peer-address socket))
            (usocket:get-peer-port socket)
            (usocket::host-to-hostname (usocket:get-local-address socket))
            (usocket:get-local-port socket))
    (format nil "~a:~d (local: ~a:~d)"
            (usocket::host-to-hostname (usocket:get-peer-address socket))
            (usocket:get-peer-port socket)
            (usocket::host-to-hostname (usocket:get-local-address socket))
            (usocket:get-local-port socket))))

(defun make-thread-name (taskmaster socket)
  (declare (ignore taskmaster))
  (format nil "Web Worker serving ~a" (safe-client-as-string socket)))

(defun handle-incoming-connection% (taskmaster socket)
  (hunchentoot::increment-taskmaster-accept-count taskmaster)
  (handler-bind
      ((cl-threadpool:threadpool-error
        (lambda (cond)
          (verbose:fatal '(:threadpool-worker :web-worker :worker-error) "{~a} Thread pool error: ~a"
                         (thread-name (current-thread)) cond)
          (too-many-taskmaster-requests taskmaster socket)
          (hunchentoot::send-service-unavailable-reply taskmaster socket))))
    (verbose:info '(:threadpool-worker :web-worker :accepting) "{~a} processing ~s via ~a"
                  (thread-name (current-thread)) (safe-client-as-string socket) (taskmaster-acceptor taskmaster))
    (hunchentoot::process-connection (taskmaster-acceptor taskmaster) socket)))

(defun safe-client-as-string (socket)
  (handler-bind
      ((usocket:bad-file-descriptor-error
        (lambda (c) (declare (ignore c))
                "Disconnected Client")))
    (client-as-string socket)))

(defmethod handle-incoming-connection ((taskmaster thread-pool-taskmaster)
                                       socket)
  (handler-bind
      ((error
        (lambda (cond)
          ;; need  to  bind  *ACCEPTOR*  so that  LOG-MESSAGE*  can  do
          ;; its work.
          (let ((*acceptor* (taskmaster-acceptor taskmaster)))
            (ignore-errors
              (usocket:socket-close socket))
            (log-message* *lisp-errors-log-level*
                          "Error while assigning worker thread for ~
new incoming connection ~a: ~a"
                          (safe-client-as-string socket)
                          cond)))))
    (cl-threadpool:add-job (taskmaster-thread-pool taskmaster)
                           (named-thread-pool-runner
                               (:name (make-thread-name taskmaster socket))
                             (handle-incoming-connection% taskmaster socket)))))

(defmethod start-thread ((taskmaster thread-pool-taskmaster)
                         thunk &key name)
  ;; XXX: if this happens, we should really toss it into add-job . . .
  (error "Thread-Pool-Taskmaster does not start new threads while running.
Tried to start a thread named ~a with ~s"
         name thunk))

(defmethod execute-acceptor ((taskmaster thread-pool-taskmaster))
  (declare (optimize (speed 1)))
  (verbose:info '(:taskmaster-acceptor) "Starting acceptor thread for ~a" taskmaster)
  (setf (hunchentoot::acceptor-process taskmaster)
        (make-thread
         (lambda ()
           (accept-connections (taskmaster-acceptor taskmaster)))
         :name (format nil "Hunchentoot Listening on ~
~:[all interfaces~;~:*Address ~a~], ~
Port ~d"
                       (acceptor-address (taskmaster-acceptor taskmaster))

                       (the (integer 1 65534) (acceptor-port (taskmaster-acceptor taskmaster)))))))
