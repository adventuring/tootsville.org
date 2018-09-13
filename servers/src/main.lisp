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
        (make-instance ' thread-pool-taskmaster:thread-pool-taskmaster)))

(defun not-found-if-null (thing)
  "If THING is null, then abort with a 404 Not Found."
  (unless thing
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  thing)

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor tootsville-restas-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
  (let ((vhost (restas::find-vhost
                (restas::request-hostname-port acceptor request)))
        (hunchentoot:*request* request))
    (when (and (not vhost) restas:*default-host-redirect*)
      (hunchentoot:redirect (hunchentoot:request-uri*)
                            :host restas:*default-host-redirect*))
    (not-found-if-null vhost)
    (multiple-value-bind (route bindings)
        (routes:match (slot-value vhost 'restas::mapper)
          (hunchentoot:request-uri*))
      (unless route
        (break "404, no match for requested URI ~s in ~s"
               (hunchentoot:request-uri*) vhost))
      (not-found-if-null route)
      (handler-bind ((error #'hunchentoot:maybe-invoke-debugger))
        (restas:process-route route bindings)))))
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
  (if (config :ssl)
      (restas:start 'tootsville
                    :port port
                    :address host
                    :hostname host
                    :ssl-certificate-file (config :ssl :certificate-file)
                    :ssl-privatekey-file (config :ssl :private-key-file)
                    :ssl-privatekey-password (config :ssl :private-key-password)
                    :acceptor-class 'tootsville-restas-acceptor)
      (restas:start ' tootsville
                      :port port
                      :address host
                      :hostname host
                      :acceptor-class 'tootsville-restas-acceptor))
  (let ((vhost (restas::find-vhost (cons host port))))
    (cond (vhost
           (setf restas:*default-host-redirect* vhost))
          (t (error "Can't find teh default VHost?"))))
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




(defun print-help ()
  "Prints a short usage summary  to *STANDARD-OUTPUT*. Note that this is
invoked  by calling  the  program  with “help”  as  its first  argument,
explicitly — the default behaviour is to run as a FastCGI server."
  (format t "~|
Usage: Run this program with one of these verbs.
No verb at all defaults to “repl”

check — perform a very simple power-on self-test
fast-cgi — run in FastCGI mode under an appropriate server (eg, Apache)
repl — run a REPL (you might want to rlwrap it)
server — start a Hunchentoot server for testing
daemon — start a Hunchentoot server for production
swank — start a Swank server
version — print the precise time and date compiled (DEPRECATED)
version-info — extract specfic version information
write-docs — write out TeΧInfo documentation
help — print this
"))

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

(defun inform-declt-of-agplv3 ()
  "Adds the AGPLv3 to the list of licenses for DECLT."
  (let ((licenses (intern "*LICENSES*" (find-package :net.didierverna.declt))))
    (set licenses
         (cons (eval licenses)
               '((:agplv3
                  "The GNU Affero General Public License"
                  "This  program is  free  software; you  can redistribute  it
and/or  modify it  under  the terms  of the  GNU  Affero General  Public
License as  published by  the Free  Software Foundation;  either version
3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program; if not,  write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA."))))))

(defun write-docs ()
  "Write out the documentation in TeΧinfo format using DECLT.

Note that DECLT  is not usually compiled into the  binary by default, so
this  may  have  to  download  DECLT  and/or  its  dependencies  through
Quicklisp when called."
  (format *trace-output* "~& Writing documentation…")

  (ql:quickload :net.didierverna.declt)
  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootsville))))
    (inform-declt-of-agplv3)
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (funcall (intern "DECLT" (find-package :net.didierverna.declt))
             :tootsville
             :library "Tootsville Ⅴ (Romance Ⅱ)"
             :texi-file (merge-pathnames #p"doc/tootsville.texi"
                                         source-dir)
             :info-file (merge-pathnames #p "doc/tootsville"
                                         source-dir)
             :license :agplv3
             :declt-notice :short
             :hyperlinks nil
             :introduction (alexandria:read-file-into-string
                            (merge-pathnames #p"src/doc/introduction"
                                             source-dir))
             :conclusion (alexandria:read-file-into-string
                          (merge-pathnames #p"src/doc/conclusion"
                                           source-dir)))))

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

(defun trace-log-file (log-dir)
  (merge-pathnames (make-pathname :name "Tootsville.trace"
                                  :type "log")
                   log-dir))

(defun find-log-dir ()
  (merge-pathnames #p"./logs/Tootsville/" (user-homedir-pathname)))

(defun standard-log-file (log-dir)
  (merge-pathnames (make-pathname :name "Tootsville.standard"
                                  :type "log")
                   log-dir))

(defun error-log-file (log-dir)
  (merge-pathnames (make-pathname :name "Tootsville.error"
                                  :type "log")
                   log-dir))

(defun verbose-log-file (log-dir)
  (merge-pathnames (make-pathname :name "Tootsville.verbose"
                                  :type "log")
                   log-dir))

(defun open-log-file (pathname)
  (open pathname :direction :output
        :if-exists :append
        :if-does-not-exist :create))

(defun greeting/daemon/error-output ()
  (format *error-output*
          "~%Error-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun greeting/daemon/log-output ()
  (v:info :logging
          "~%Log-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun greeting/daemon/standard-output ()
  (format t "~%Standard-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun greeting/daemon/trace-output ()
  (format *trace-output*
          "~%Trace-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun set-up-for-daemon/standard-output (log-dir)
  (setf *standard-output* (open-log-file (standard-log-file log-dir)))
  (greeting/daemon/standard-output))

(defun set-up-for-daemon/log-output (log-dir)
  (v:output-here (open-log-file (verbose-log-file log-dir)))
  (greeting/daemon/log-output))

(defun set-up-for-daemon/error-output (log-dir)
  (setf *error-output* (open-log-file (error-log-file log-dir)))
  (greeting/daemon/error-output))

(defun set-up-for-daemon/trace-output (log-dir)
  (setf *trace-output* (open-log-file (trace-log-file log-dir)))
  (greeting/daemon/trace-output))

(defun set-up-for-daemon/start-logging ()
  (let ((log-dir (find-log-dir)))
    (ensure-directories-exist log-dir)
    (set-up-for-daemon/log-output log-dir)
    (set-up-for-daemon/standard-output log-dir)
    (set-up-for-daemon/error-output log-dir)
    (set-up-for-daemon/trace-output log-dir)))

(defun start-production (&key port)
  "Start a Hunchentoot  server via `START' and daemonize with Swank"
  (set-up-for-daemon/start-logging)
  (start :port port)
  (start-swank)
  (loop
     (format *trace-output* "~&//* Still Alive (~a)" (now))
     (force-output *trace-output*)
     (sleep 90))) 

(defun post/read-version-page (port)
  "Power-On-Self-Test:  Checks  that  the  server  can  respond  to  the
version-page query locally."
  (let ((retries 9))
    (tagbody retry-post
       (handler-case
           (return-from post/read-version-page
             (drakma:http-request
              (format nil "http://localhost:~d/version.txt" port)))
         (usocket:connection-refused-error (c)
           (cond ((minusp (decf retries))
                  (error "Failed POST: Can't connect to local server ~
(after retries)~%~a" c))
                 (t (format *error-output*
                            "~&~a~%Hmm, maybe we need to wait ~
a moment and try that again.~%" c)
                    (force-output *error-output*)
                    (sleep 1)
                    (go retry-post))))))))

(defun power-on-self-test (&key (exitp t))
  "Perform some sanity checking as a part of testing.

This testing should  be much more complete  than it really is  — it will
need to be expanded a great deal to increase confidence in these tests."
  (fresh-line)
  (princ "Power-on self-test:")
  (fresh-line)
  (let ((port (+ (random 10) 27700)))
    (handler-case (start :port port)
      (simple-error (c) (if (find-restart :restart-server)
                            (invoke-restart :restart-server)
                            (signal c))))
    (sleep 1/2) ; start time
 ;;; something that appears on the version page, but no error pages.
    (let ((reply (prog1 (post/read-version-page port)
                   (stop))))
      (unless (search "Bruce-Robert Pocock" reply)
        (warn "Failed POST~%got~%~a" reply)
        (if exitp
            (cl-user::exit :code 27 :abort t :timeout 5)
            (return-from power-on-self-test nil))
        nil)))
  (fresh-line)
  (princ "Passed POST")
  (fresh-line)
  (stop)
  t)

(defun banner/query-io ()
  (format *query-io*
          "~&~|
Tootsville Ⅴ
Copyright © ~d, Bruce-Robert Pocock
Licensed under the terms of the GNU Affero General Public License, version 3~%~%"
          (romance-ii-copyright-latest))
  (finish-output *query-io*))

(defun banner/log ()
  (v:info :startup
          "~&~|
Tootsville Ⅴ
Copyright © ~d, Bruce-Robert Pocock
Licensed under the terms of the GNU Affero General Public License, version 3~%~%"
          (romance-ii-copyright-latest))
  (finish-output *query-io*))

(defun banner/standard-output ()
  (format t "~&~|~%~a (© ~d)" (tootsville::romance-ii-program-name/version)
          (romance-ii-copyright-latest))
  (finish-output))

(defun banner/error-output ()
  (format *error-output* "~&~|
~a Starting Tootsville Ⅴ, error log begins"
          (local-time:format-timestring nil (local-time:now))))

(defun banner/trace-output ()
  (format *error-output* "~&~|
~a Starting Tootsville Ⅴ, trace log begins"
          (local-time:format-timestring nil (local-time:now))))

(defun banner ()
  (banner/log)
  (banner/query-io)
  (banner/standard-output)
  (banner/error-output)
  (banner/trace-output))

(defun entry (argv)
  "Top-level  entry-point  for  the  compiled  executable  binary  form.

Dispatches   based   upon   the   single  argument,   expected   to   be
a verb (case-insensitive) from the hard-coded table in this function."
  (case (intern (string-upcase (typecase argv
                                 (cons (if (< 1 (length argv))
                                           (second argv)
                                           "REPL"))
                                 (null "HELP")
                                 (t argv))) :keyword)
    (load-config)
    (banner)
    (:fast-cgi (or #-common-lisp (fastcgi-entry) (error 'unimplemented)))
    (:server (start-hunchentoot :port (if (and (consp argv)
                                               (< 2 (length argv)))
                                          (parse-integer (third argv))
                                          5000)))
    (:daemon (start-production :port (if (and (consp argv)
                                              (< 2 (length argv)))
                                         (parse-integer (third argv))
                                         5000)))
    (:check (power-on-self-test))
    (:repl (start-repl))
    (:version (print *compiled*))
    (:swank (start-swank)
     (start-repl))
    (:write-docs (write-docs))
    (:version-info (version-info-report (nthcdr 2 argv)))
    (otherwise (print-help))))
