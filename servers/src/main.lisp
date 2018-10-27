(in-package :Tootsville)



(defclass Tootsville-restas-acceptor (restas:restas-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :request-class 'restas::restas-request
    :error-template-directory (config :templates :errors)
    :access-log-destination (config :log :access)
    :message-log-destination (config :log :message)))

(defmethod initialize-instance :after
    ((acceptor Tootsville-restas-acceptor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value acceptor 'hunchentoot::taskmaster)
        (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))

(defun not-found-if-null (thing)
  "If THING is null, then abort with a 404 Not Found."
  (unless thing
    (verbose:info :not-found "{~a} 404: object not found"
                  (current-thread))
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  thing)

(defgeneric respond-to-error (condition)
  (:method ((error error))
    (hunchentoot:maybe-invoke-debugger error))
  (:method ((error unimplemented))
    (verbose:info :unimplemented "Unimplemented function called: ~s" error)))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor Tootsville-restas-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (verbose:info :request "Dispatching request ~s via acceptor ~s"
                request acceptor)
  (let ((vhost (restas::find-vhost
                (restas::request-hostname-port acceptor request)))
        (hunchentoot:*request* request))
    (verbose:info :route "Mapping ~{~a:~a~} to ~s"
                  (destructuring-bind (host . port)
                      (restas::request-hostname-port acceptor request)
                    (list host port))
                  vhost)
    (when (and (null vhost)
               restas:*default-host-redirect*)
      (verbose:info :route "Unrecognized hostname and port ~s; ~
redirect to default host"
                    (restas::request-hostname-port acceptor request))
      (hunchentoot:redirect (hunchentoot:request-uri*)
                            :host (restas::vhost-hostname
                                   restas:*default-host-redirect*)
                            :port (restas::vhost-port
                                   restas:*default-host-redirect*)))
    (verbose:info :vhost "{~a} Request ~s on VHost ~s"
                  (thread-name (current-thread)) request vhost)
    (not-found-if-null vhost)
    (multiple-value-bind (route bindings)
        (routes:match (slot-value vhost 'restas::mapper)
          (hunchentoot:request-uri*))
      (unless route
        (verbose::info :not-found "{~a} No match for requested URI ~s on vhost ~s"
                       (thread-name (current-thread))
                       (hunchentoot:request-uri*) vhost)
        (verbose::info :not-found "{~a} Mapper: ~s"
                       (thread-name (current-thread))
                       (slot-value vhost 'restas::mapper)))
      (verbose:info :route "{~a} Route is ~s"  (thread-name (current-thread)) route)
      (not-found-if-null route)
      (handler-bind ((sb-int:closed-stream-error
                      (lambda (c)
                        (verbose:info :disconnect "~a" c)
                        (abort)))
                     (error (lambda (c) (respond-to-error c))))
        (verbose:info :route "{~a} URI ~s leads to ~s"
                      (thread-name (current-thread))
                      (hunchentoot:request-uri*) route)
        (verbose:info :route "{~a} Invoking endpoint for ~a" (thread-name (current-thread)) route)
        (prog1 (restas:process-route route bindings)
          (verbose:info :route "{~a} Done processing route ~a" (thread-name (current-thread)) route))))))


(defun find-acceptor (host port)
  "Find an active Acceptor running on the given HOST address and PORT"
  (dolist (acceptor restas::*acceptors*)
    (when (and (typep acceptor 'Tootsville-restas-acceptor)
               (equal host
                      (hunchentoot:acceptor-address acceptor))
               (= port
                  (hunchentoot:acceptor-port acceptor)))
      (return-from find-acceptor acceptor))))

(defvar *async-tasks* nil)

(defconstant +async-worker-threads+ 2)

(defun name-all-async-threads-idle ()
  (loop for thread in (slot-value *async-tasks*
                                  'cl-threadpool::threads)
     for i fixnum from 1
     with count = (taskmaster-max-thread-count taskmaster) ;; FIXME
     do (setf (sb-thread:thread-name thread)
              (format nil "Idle Asyncronous Worker (#~d of ~d)" i count))))

(defun swank-connected-p ()
  (when (swank:connection-info) t))

(defun init-async ()
  (setf *async-tasks*
        (cl-threadpool:make-threadpool
         +async-worker-threads+
         :max-queue-size 1024
         :name "Asynchronous Workers"
         :resignal-job-conditions (not (swank-connected-p))))
  (cl-threadpool:start *async-tasks*)
  (name-all-async-threads-idle))

(defun run-async (function)
  (unless *async-tasks*
    (init-async))
  (cl-threadpool:add-job *async-tasks*
                         (lambda ()
                           (let ((idle-name (thread-name (current-thread))))
                             (setf (sb-thread:thread-name (current-thread)) (format nil "Async: run ~s" function))
                             (unwind-protect
                                  (thread-pool-taskmaster::with-pool-thread-restarts ((thread-name (current-thread)))
                                    (verbose:info '(:threadpool-worker :async-worker :worker-start) "{~a}: working" (thread-name (current-thread)))
                                    (funcall function))
                               (verbose:info '(:threadpool-worker :async-worker :worker-finish) "{~a}: done" (thread-name (current-thread)))
                               (setf (sb-thread:thread-name (current-thread)) idle-name))))))


(defun start (&key (host "localhost") (port 5000))
  "Start a local Hunchentoot server.

HOST is an address of a live interface; PORT may be a port number.

The server will  be started running on port  5000 on local-loopback-only
addresses  (127.0.0.1  and  ::1).  If an  existing  server  is  running,
a restart will be presented to allow you to kill it (RESTART-SERVER)."
  (when-let ((previous (find-acceptor host port)))
    (restart-case (error "Server is already running on ~a port ~a" host port)
      (stop-previous ()
        :report "Stop it (restart)"
        (restart-case (stop previous)
          (ignore-error ()
            :report "Ignore error and try to start anyway ")))
      (change-port (port*)
        :report "Use a different port"
        (start :host host :port port*))))
  (setf hunchentoot:*log-lisp-errors-p* t
        hunchentoot:*log-lisp-backtraces-p* t
        hunchentoot:*log-lisp-warnings-p* t)
  (when (developmentp)
    (setf hunchentoot:*catch-errors-p* nil
          hunchentoot:*show-lisp-errors-p* t
          hunchentoot:*show-lisp-backtraces-p* t))
  (restart-case
      (if (config :ssl)
          (restas:start 'Tootsville
                        :port port
                        :address host
                        :hostname host
                        :ssl-certificate-file (config :ssl :certificate-file)
                        :ssl-privatekey-file (config :ssl :private-key-file)
                        :ssl-privatekey-password (config :ssl :private-key-password)
                        :acceptor-class 'Tootsville-restas-acceptor)
          (restas:start 'Tootsville
                        :port port
                        :address host
                        :hostname host
                        :acceptor-class 'Tootsville-restas-acceptor))
    (change-port (port*)
      :report "Use a different port"
      (start :host host :port port*))
    (stonith ()
      :report "Shoot the other node in the head (kill listening process)"
      (stonith :host host :port port)
      (start :host host :port port)))
  (let ((vhost (restas::find-vhost (cons host port))))
    (cond (vhost
           (setf restas:*default-host-redirect* vhost))
          (t (error "Can't find the default VHost?"))))
  (let ((acceptor (find-acceptor host port)))
    (cond (acceptor
           (setf (hunchentoot:acceptor-name acceptor)
                 (format nil "Tootsville ~:[Non-TLS ~;~](~a port ~d)"
                         (config :ssl) host port))
           acceptor)
          (t
           (error "Did that even work? Acceptor seems not to have started.")))))




(defmethod usocket:socket-close ((socket null))
  (warn "Ignoring request to close NIL"))

(defun stop (&optional (acceptor (first restas::*acceptors*)))
  "Stop the Hunchentoot server process started by `START'"
  (when acceptor
    (ignore-errors
      (hunchentoot:stop acceptor :soft t))
    ;; TODO: wait for process to really be done
    (setf restas::*acceptors*
          (delete-if (curry #'eql acceptor)
                     restas::*acceptors*))))



;;; build date/timestamp

(defparameter *compiled* :never
  "A string representing the (fairly  precise) time at which the program
 was compiled.")

(defparameter *build-date* :never
  "A string representing  the year, month, and day at  which the program
 was compiled.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *compiled* (with-output-to-string (s)
                     (print-object (now) s))
        *build-date* (format-timestring nil (now)
                                        :format '(:year #\- :month #\- :day))))


;;; REPL

(defun start-repl ()
  "Starts a PREPL REPL."
  (ql:quickload :prepl)
  (restart-bind
      ((quit #'cl-user::exit
         :report-function (format *query-io* "Quit the REPL")))
    (let ((*package* :Oliphaunt-User))
      (funcall (intern "REPL" (find-package :prepl))))))


;;; Swank

(defun start-swank (&optional (port (+ 46046 (* 2 (random 500)))))
  "Starts a SWANK server on PORT.

Writes  the   port  number   to  a  file   named  after   this  (parent)
process's PID."
  (asdf:load-system :swank)
  (v:info :swank "~&Starting Swank listener on port ~d" port)
  (swank:create-server :port port :dont-close t)
  (ensure-directories-exist "~/run/")
  (with-output-to-file (s (format nil "~~/run/~D.swank.port" 
                                  (swank/backend:getpid))))
  port)


;;; Web servers

(defun start-hunchentoot (&key host port)
  "Start a Hunchentoot  server via `START' and fall through  into a REPL
to keep the process running."
  (start :host host :port port)
  (print "Hunchentoot server running. Evaluate (TOOTSVILLE:STOP) to stop, or exit the REPL.")
  (start-repl))

(defparameter *trace-output-heartbeat-time* 90)

(defun start-production (&key host port)
  "Start a Hunchentoot  server via `START' and daemonize with Swank"
  (disable-sbcl-ldb)
  (set-up-for-daemon/start-logging)
  (start :host host :port port)
  (start-swank)
  (loop
     (trace-output-heartbeat)
     (sleep *trace-output-heartbeat-time*))) 


;;; Recompilation

(defvar *location-of-main* (or *load-pathname*
                               *compile-file-pathname*))

(defun rebuild-myself ()
  "Recompile the running server.

Hopefully you've already tested the changes?"
  (load (merge-pathnames
         #p"Tootsville.asd"
         (or (when *location-of-main*
               (merge-pathnames
                (make-pathname :directory '(:relative :up))
                (make-pathname :directory (pathname-directory
                                           *location-of-main*))))
             (merge-pathnames #p"servers/"
                              (user-homedir-pathname)))))
  (ql:quickload :Tootsville))

