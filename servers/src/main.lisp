(in-package :tootsville)



(defclass tootsville-restas-acceptor (restas:restas-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :request-class 'restas::restas-request
    :error-template-directory (config :templates :errors)
    :access-log-destination (config :log :access)
    :message-log-destination (config :log :message)))

(defmethod initialize-instance :after
    ((acceptor tootsville-restas-acceptor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value acceptor 'hunchentoot::taskmaster)
        (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))

(defun not-found-if-null (thing)
  "If THING is null, then abort with a 404 Not Found."
  (unless thing
    (verbose:info :not-found "404: object not found")
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  thing)

(defgeneric respond-to-error (condition)
  (:method ((error error))
    (hunchentoot:maybe-invoke-debugger error)))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor tootsville-restas-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (verbose:info :request "Dispatching request ~s via acceptor ~s" request acceptor)
  (let ((vhost (restas::find-vhost
                (restas::request-hostname-port acceptor request)))
        (hunchentoot:*request* request))
    (verbose:info :route "Mapping ~s to ~s" (restas::request-hostname-port acceptor request) vhost)
    (when (and (null vhost)
               restas:*default-host-redirect*)
      (verbose:info :route "Unrecognized hostname and port ~s; redirect to default host"
                    (restas::request-hostname-port acceptor request))
      (hunchentoot:redirect (hunchentoot:request-uri*)
                            :host (restas::vhost-hostname restas:*default-host-redirect*)
                            :port (restas::vhost-port     restas:*default-host-redirect*)))
    (verbose:info :vhost "Request ~s on VHost ~s" request vhost)
    (not-found-if-null vhost)
    (multiple-value-bind (route bindings)
        (routes:match (slot-value vhost 'restas::mapper)
          (hunchentoot:request-uri*))
      (unless route
        (verbose::info :not-found "No match for requested URI ~s on vhost ~s"
                       (hunchentoot:request-uri*) (restas::vhost-hostname-port vhost))
        (verbose::info :not-found "Mapper: ~s"
                       (slot-value vhost 'restas::mapper)))
      (verbose:info :route "Route is ~s" route)
      (not-found-if-null route)
      (handler-bind ((error (lambda (c) (respond-to-error c))))
        (verbose:info :route "URI ~s mapped to route ~s"
                      (hunchentoot:request-uri*) route)
        (verbose:info :route "Processing route")
        (prog1 (restas:process-route route bindings)
          (verbose:info :route "Done processing route"))))))


(defun find-acceptor (host port)
    "Find an active Acceptor running on the given HOST address and PORT"
    (dolist (acceptor restas::*acceptors*)
      (when (and (typep acceptor 'tootsville-restas-acceptor)
                 (equal host
                        (hunchentoot:acceptor-address acceptor))
                 (= port
                    (hunchentoot:acceptor-port acceptor)))
        (return-from find-acceptor acceptor))))

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
        (restas:start 'tootsville
                      :port port
                      :address host
                      :hostname host
                      :ssl-certificate-file (config :ssl :certificate-file)
                      :ssl-privatekey-file (config :ssl :private-key-file)
                      :ssl-privatekey-password (config :ssl :private-key-password)
                      :acceptor-class 'tootsville-restas-acceptor)
        (restas:start 'tootsville
                        :port port
                        :address host
                        :hostname host
                        :acceptor-class 'tootsville-restas-acceptor))
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

(defun start-repl ()
  "Starts a PREPL REPL."
  (ql:quickload :prepl)
  (restart-bind
      ((quit #'cl-user::exit
         :report-function (format *query-io* "Quit the REPL")))
    (funcall (intern "REPL" (find-package :prepl)))))

(defun start-swank (&optional (port 46046))
  "Starts a SWANK server."
  (asdf:load-system :swank)
  (v:info :swank
          "~&Started Swank listener on port ~d"
          (funcall (intern "CREATE-SERVER"
                           (find-package :swank))
                   :port port :dont-close t)))

(defun start-hunchentoot (&key port)
  "Start a Hunchentoot  server via `START' and fall through  into a REPL
to keep the process running."
  (start :port port)
  (print "Hunchentoot server running. Evaluate (TOOTSVILLE:STOP) to stop, or exit the REPL.")
  (start-repl))

(defvar *location-of-main* (or *load-pathname*
                               *compile-file-pathname*))

(defun rebuild-myself ()
  "Recompile the running server.

Hopefully you've already tested the changes?"
  (load (merge-pathnames
         #p"tootsville.asd"
         (or (when *location-of-main*
               (merge-pathnames
                (make-pathname :directory '(:relative :up))
                (make-pathname :directory (pathname-directory
                                           *location-of-main*))))
             (merge-pathnames #p"servers/"
                              (user-homedir-pathname)))))
  (ql:quickload :tootsville))

(defun start-production (&key port)
  "Start a Hunchentoot  server via `START' and daemonize with Swank"
  (set-up-for-daemon/start-logging)
  (start :port port)
  (start-swank)
  (loop
     (trace-output-heartbeat)
     (sleep 90))) 




