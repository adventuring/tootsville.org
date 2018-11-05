(in-package :Tootsville)





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

(defun (setf thread-name) (name thread)
  #+sbcl (setf (sb-thread:thread-name thread) name))

(defun name-all-async-threads-idle ()
  (let ((length (length (slot-value *async-tasks*
                                    'cl-threadpool::threads))))
    (loop for thread in (slot-value *async-tasks*
                                    'cl-threadpool::threads)
       for i fixnum from 1
       do (setf (thread-name thread)
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
  (cl-threadpool:add-job 
   *async-tasks*
   (lambda ()
     (let ((idle-name (thread-name (current-thread))))
       (setf (thread-name (current-thread)) (format nil "Async: run ~s" function))
       (unwind-protect
            (thread-pool-taskmaster:with-pool-thread-restarts
                ((thread-name (current-thread)))
              (verbose:info '(:threadpool-worker :async-worker :worker-start)
                            "{~a}: working" (thread-name (current-thread)))
              (funcall function))
         (verbose:info '(:threadpool-worker :async-worker :worker-finish)
                       "{~a}: done" (thread-name (current-thread)))
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



(defun connect-databases ()
  (dolist (thread (mapcar (lambda (n)
                            (make-thread n :name (string-capitalize n)))
                          '(connect-mixer connect-directory #+ (or) connect-cache)))
    (join-thread thread)))

(defun connect-mixer ()
  (setf clouchdb:*couchdb* 
        (clouchdb:make-db :host (or (config :mixer :host))
                          :port (or (config :mixer :port) "5984")
                          :user (config :mixer :admin :name)
                          :password (config :mixer :admin :password)
                          :name "tootsville/5"))
  (v:info :mixer "MOTD from Mixer: ~a" (cdr (assoc :|motd| (clouchdb:get-document "motd")))))

(defun connect-directory ())
#+ (or)
(defun connect-cache ()
  (setf cl-memcached:*memcache*
        (cl-memcached:make-memcache :ip (config :cache :host)
                                    :port (or (config :cache :port) 11211)
                                    :name (cluster-name)))
  (let ((n (princ-to-string (random (expt 2 63))))
        (key (format nil "~a.~a" (machine-instance) (cluster-name))))
    (cl-memcached:mc-set key n)
    (let ((m (cl-memcached:mc-get key)))
      (assert (= n m) ()
              "MemCacheD did not return the random number (~x) for key ~a" 
              n key))))
