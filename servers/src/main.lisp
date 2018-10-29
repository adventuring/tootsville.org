(in-package :Tootsville)



(defclass Tootsville-REST-acceptor (hunchentoot:easy-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :error-template-directory (config :templates :errors)
    :access-log-destination (config :log :access)
    :message-log-destination (config :log :message)))

(defmethod initialize-instance :after
    ((acceptor Tootsville-REST-acceptor) &rest initargs)
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

(defun request-accept-types ()
  (when-let (accept (assoc :accept (hunchentoot:headers-in*)))
    (mapcar (curry #'string-trim +whitespace+)
            (split-sequence #\, (rest accept)))))

(defun template-match (template list)
  (if (every #'stringp template)
      (equalp template list)
      (loop for tmpl in template
         for el in list
         with result = nil
         do (etypecase tmpl
              (string (unless (string= tmpl el)
                        (return nil)))
              (symbol (push el result)))
         finally (return (nreverse result)))))

(assert (template-match '("foo" "bar" "baz") '("foo" "bar" "baz")))
(assert (equalp '("42" "99")
                (template-match '("foo" :bar :baz) '("foo" "42" "99"))))

(defun strip-after-sem (s)
  (if-let ((sem (position #\; s :Test #'char=)))
    (subseq s 0 sem)
    s))

(defun accept-type-equal (a b &key (allow-wildcard-p t))
  (let ((a (strip-after-sem a))
        (b (strip-after-sem b)))
    (or (string-equal a b)
        (and allow-wildcard-p
             (or (and (string-ends "/*" a)
                      (let ((slash (position #\/ a)))
                        (string-equal a b :end1 slash :end2 slash)))
                 (and (string-ends "/*" b)
                      (let ((slash (position #\/ b))) 
                        (string-equal a b :end1 slash :end2 slash)))
                 (equal a "*/*")
                 (equal b "*/*"))))))

(assert (accept-type-equal "text/html" "text/html"))
(assert (accept-type-equal "text/html" "text/html;charset=utf-8"))
(assert (accept-type-equal "text/html" "text/*"))
(assert (accept-type-equal "text/html" "text/*;charset=utf-8"))
(assert (accept-type-equal "text/html" "*/*"))
(assert (not (accept-type-equal "text/html" "text/*" :allow-wildcard-p nil)))

(defun dispatch-request% (&optional (request hunchentoot:*request*))
  (let ((uri-parts (split-sequence #\/ (hunchentoot:request-uri request)
                                   :remove-empty-subseqs t))
        (ua-accept (request-accept-types)))
    (labels ((maybe-dispatch (path allow-wildcard-p)
               (when path
                 (destructuring-bind (method template length accept function) path
                   (when (and (eql method (hunchentoot:request-method*))
                              (= length (length uri-parts))
                              (member accept ua-accept :test (rcurry #'accept-type-equal
                                                                     :allow-wildcard-p allow-wildcard-p)))
                     (when-let (bound (template-match template uri-parts))
                       (verbose:info :path "Matched ~s to (~s ~s)"
                                     (hunchentoot:request-uri request) function bound)
                       (return-from dispatch-request% (if (eql t bound)
                                                          (funcall function)
                                                          (apply function bound)))))))))
      (dolist (path *paths*) (maybe-dispatch path nil))
      (dolist (path *paths*) (maybe-dispatch path t)))
    (verbose:info :path "No match for ~s accepting ~s" uri-parts ua-accept)
    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler)))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor Tootsville-REST-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (verbose:info :request "Dispatching request ~s via acceptor ~s"
                request acceptor)
  (let ((hunchentoot:*request* request))
    (verbose:info :request "{~a} Accepting request ~s"
                  (thread-name (current-thread)) request)
    (dispatch-request%)))


(defvar *acceptors* nil)

(defun find-acceptor (host port)
  "Find an active Acceptor running on the given HOST address and PORT"
  (dolist (acceptor *acceptors*)
    (when (and (typep acceptor 'Tootsville-REST-acceptor)
               (equal host
                      (hunchentoot:acceptor-address acceptor))
               (= port
                  (hunchentoot:acceptor-port acceptor)))
      (return-from find-acceptor acceptor))))

(defvar *async-tasks* nil)

(defconstant +async-worker-threads+ 2)

(defun name-all-async-threads-idle ()
  (let ((length (length (slot-value *async-tasks*
                                    'cl-threadpool::threads))))
    (loop for thread in (slot-value *async-tasks*
                                    'cl-threadpool::threads)
       for i fixnum from 1
       do (setf (sb-thread:thread-name thread)
                (format nil "Idle Asyncronous Worker (#~d of ~d)" i length)))))

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
      (push (let ((acceptor
                   (hunchentoot:start
                    (if (config :ssl)
                        (make-instance 'Tootsville-REST-SSL-Acceptor
                                       :ssl-certificate-file (config :ssl :certificate-file)
                                       :ssl-privatekey-file (config :ssl :private-key-file)
                                       :ssl-privatekey-password (config :ssl :private-key-password)
                                       :address host
                                       :port port)
                        (make-instance 'Tootsville-REST-Acceptor
                                       :address host
                                       :port port)))))
              (setf (hunchentoot:acceptor-name acceptor)
                    (format nil "Tootsville ~:[Non-TLS ~;~](~a port ~d)"
                            (config :ssl) host port)))
            *acceptors*)
    (change-port (port*)
      :report "Use a different port"
      (start :host host :port port*))
    (stonith ()
      :report "Shoot the other node in the head (kill listening process)"
      (stonith :host host :port port)
      (start :host host :port port))))




(defmethod usocket:socket-close ((socket null))
  (warn "Ignoring request to close NIL"))

(defun stop (&optional (acceptor (first *acceptors*)))
  "Stop the Hunchentoot server process started by `START'"
  (when acceptor
    (ignore-errors
      (hunchentoot:stop acceptor :soft t))
    ;; TODO: wait for process to really be done
    (setf *acceptors* (delete-if (curry #'eql acceptor)
                                 *acceptors*))))



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
    (let ((*package* (find-package :Oliphaunt-User)))
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

