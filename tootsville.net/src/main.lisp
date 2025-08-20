;;;; -*- lisp -*-
;;;
;;;; src/main.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2024  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org);
;;;; Copyright © 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA.
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
;;; You can reach Interworldly Adventuring, LLC at https://interworldly.com/

(in-package :Tootsville)



(defvar *motd*
  "Welcome to Tootsville! Let's make some noise!
<BR>
This is experimental server software for Tootsville V.
<BR>
Lots of things will not work yet."
  "The message of the day. 

This is served up to every person who signs in. It can be altered easily
using the `TOOTSVILLE-USER::MOTD' command.")

(defvar *acceptors* nil
  "The set of listening acceptors awaiting incoming connections.")

(defvar *started* nil
  "The time at which the server was started")

(defvar *stable-nonce*
  (map 'string #'code-char
       (loop for n below (+ 10 (random 25))
             collecting (+ 32 (random 1000))))
  "A nonce value used as a non-cryptographically-secure salt value for
casual purposes such as generating buddy list request signatures.  It
should remain stable for a long time, but can change occassionally
(e.g. due to server reboots)")

(defun find-acceptor (host port)
  "Find an active Acceptor running on the given HOST address and PORT"
  (dolist (acceptor *acceptors*)
    (when (and (typep acceptor 'Tootsville-REST-acceptor)
               (equal host
                      (hunchentoot:acceptor-address acceptor))
               (= port
                  (hunchentoot:acceptor-port acceptor)))
      (return-from find-acceptor acceptor))))

(defvar *async-tasks* nil
  "An LPARALLEL kernel used for running asynchronous tasks.")
(defvar *async-channel* nil
  "An LPARALLEL channel used for running asynchronous tasks.")

(defun (setf thread-name) (name thread)
  "Set the NAME of THREAD.

This isn't supported by Bordeaux, so we need per-implementation versions here.

Presently only supported for SBCL."
  #+sbcl (setf (sb-thread:thread-name thread) name))

(defun swank-connected-p ()
  "Is Swank currently connected to this Lisp image?"
  swank::*connections*)

(defun name-idle-threads-sequentially (count)
  "Name all of the idle asynchronous worker threads with numbers up to COUNT."
  (let ((n 0))
    (dolist (thread (all-threads))
      (when (string-equal "Idle Asynchronous Worker" (thread-name thread))
        (setf (thread-name thread)
              (format nil "Idle Asynchronous Worker № ~:d (of ~:d)"
                      (incf (the fixnum n)) count))))))

(defun init-async ()
  "Initialize LPARALLEL for running tasks asynchronously."
  (let ((count-threads (max 1 (round (/ (processor-count) 2)))))
    (setf *async-tasks*
          (lparallel:make-kernel count-threads
                                 :name "Idle Asynchronous Worker"))
    (name-idle-threads-sequentially count-threads))
  (let ((lparallel:*kernel* *async-tasks*))
    (setf *async-channel* (lparallel:make-channel)))
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* *async-tasks*)))

(defun run-async (function &key name)
  "Run FUNCTION asynchronously in a thread named NAME.

If NAME is omitted, a generic name will be created based on FUNCTION."
  (unless *async-tasks*
    (init-async))
  (let ((lparallel:*kernel* *async-tasks*))
    (lparallel:submit-task
     *async-channel*
     (lambda ()
       (catch 'thread-pool-taskmaster::bazinga
         (let ((idle-name (thread-name (current-thread)))
               (start-time (get-internal-real-time)))
           (setf (thread-name (current-thread)) (or name
                                                    (format nil "Async: run ~s" function)))
           (unwind-protect
                (thread-pool-taskmaster:with-pool-thread-restarts
                    ((thread-name (current-thread)))
                  (verbose:info '(:threadpool-worker :async-worker :worker-start)
                                "{~a}: working" (thread-name (current-thread)))
                  (funcall function))
             (verbose:info '(:threadpool-worker :async-worker :worker-finish)
                           "{~a}: done (~,2f sec)" (thread-name (current-thread))
                           (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second))
             (setf (sb-thread:thread-name (current-thread)) idle-name))))))))

(defun background-gc ()
  "Start a garbage collection in a different thread.

This starts an asynchronous run of the garbage collector, but of course,
based on  implementation characteristics, this could  affect all threads
in this image.

Presently only works in SBCL."
  (run-async (lambda () #+sbcl (sb-ext:gc))
             :name "Garbage Collection"))

(defun start (&key (host "0.0.0.0") (port 5000) (fullp t))
  "Start a local Hunchentoot server on HOST and PORT.

HOST is an address of a live interface; PORT may be a port number.

The  server will  be  started running  on port  PORT  (default 5000)  on
HOST (default local-loopback-only address \"localhost\"). If an existing
server is  running, a  restart will  be presented to  allow you  to kill
it (RESTART-SERVER).

When FULLP is true, a  complete start-up including reading config files,
connecting to the databases, power-on self-test, &c. will be performed.

In addition,  if a  TSL (SSL)  certificate for this  host appears  to be
present, created by  Let's Encrypt, then a TLS acceptor  will be started
on  a  port as  identified  in  the  configuration  file, if  that  port
is available."
  (when fullp
    (load-config)
    (connect-databases)
    (power-on-self-test)
    (cassandra-boot)
    (background-gc))
  
  (v:info '(:starting :hack) "HACK … reloading friendly neighborhood database definition file …")
  (load (asdf:system-relative-pathname :Tootsville "src/db/friendly.lisp") :external-format :utf-8)
  
  (setf hunchentoot:*hunchentoot-default-external-format* :utf-8)
  
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
      (progn (if (enable-ssl-p)
                 (let ((ssl (hunchentoot:start
                             (make-instance 'Tootsville-REST-SSL-Acceptor
                                            :ssl-certificate-file (ssl-certificate)
                                            :ssl-privatekey-file (ssl-private-key)
                                            :address host
                                            :port (or port (config :ssl :port))))))
                   (setf (hunchentoot:acceptor-name ssl)
                         (format nil "Tootsville at ~a port ~d" host port))
                   (push ssl *acceptors*))
                 (let ((acceptor (hunchentoot:start
                                  (make-instance 'Tootsville-REST-Acceptor
                                                 :address host
                                                 :port port))))
                   (setf (hunchentoot:acceptor-name acceptor)
                         (format nil "Tootsville Non-TLS at ~a port ~d"
                                 host port))
                   (push acceptor *acceptors*)))
             (listen-for-websockets)
             (setf *started* (get-universal-time*))
             (start-game-metronome))
    (change-port (port*)
      :report "Use a different port"
      (start :host host :port port*))
    (stonith ()
      :report "Shoot the other node in the head (kill listening process)"
      (stonith :host host :port port)
      (start :host host :port port))))



(defmethod usocket:socket-close ((socket null))
  "This is a NO-OP methed.

Sometimes  we  attempt  to  close  NIL  as  though  it  were  a  socket.
This prevents that from signaling an error, giving a warning instead."
  (warn "Ignoring request to close NIL"))

(defun stop (&optional (acceptor (first *acceptors*)))
  "Stop the Hunchentoot server process started by `START'"
  (when acceptor
    (ignore-errors
      (hunchentoot:stop acceptor :soft t))
    (removef *acceptors* acceptor)))


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
  (ql:quickload :swank)
  (v:info :swank "~&Starting Swank listener on port ~d" port)
  (swank:create-server :port port :dont-close t)
  (ensure-directories-exist "~/run/")
  (with-output-to-file (s (format nil "~~/run/~D.swank.port"
                                  (swank/backend:getpid))))
  port)



(defvar *verbose-logging-lock* (make-lock "Verbose logging lock")
  "A lock used to prevent the Verbose library from cross-talking over itself.

When multiple threads try to write at the same time, you can get partial
messages mixed together in a confusing way. This lock prevents that from
occurring when  we use our definition  of `VERBOSE:FORMAT-MESSAGE' which
observes it.")

(defmethod verbose:format-message ((stream stream) (message v:message))
  "Sets an ornate formatting for messages logged via the Verbose library."
  (with-lock-held (*verbose-logging-lock*)
    (format stream "~&~a	{~a}	[~a; ~{~a~^, ~}]~%⮕ ~a~%"
            (format-timestring nil (v:timestamp message)
                               :format
                               '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\Space
                                 (:hour 2) #\: (:min 2) #\: (:sec 2)
                                 #\Space
                                 :nsec))
            (thread-name (v:thread message))
            (v:level message)
            (mapcar #'symbol-munger:lisp->english (v:categories message))
            (v:content message))))



;;; Web servers

(defun start-hunchentoot (&key (host "localhost") (port 5000))
  "Start a Hunchentoot  server via `START' and fall through  into a REPL
to keep the process running."
  (start :host host :port port)
  (print "Hunchentoot server running. Evaluate (TOOTSVILLE:STOP) to stop, ~
or exit the REPL.")
  (start-repl))

(defun debugger ()
  "Start up Swank in the project directory and start a server on the default port."
  (swank:set-default-directory (asdf:component-relative-pathname
                                (asdf:find-system  :Tootsville)))
  (swank:set-package :Tootsville)
  (start)
  (banner)
  (init-characters))

(defun destroy-all-listeners ()
  "Destroy all Hunchentoot listener threads violently.

Normally only needed during debugging. See instead `STOP'."
  (map nil #'destroy-thread
       (remove-if-not (lambda (th) (search "Hunchentoot Listening on Address"
                                           (thread-name th)))
                      (all-threads))))

(defun destroy-all-idle-workers ()
  "Destroy all idle web worker threads violently.

Normally only needed during debugging. See instead `STOP'."
  (let ((workers (remove-if-not (lambda (th) (search "Idle Web Worker"
                                                     (thread-name th)))
                                (all-threads))))
    (map nil #'destroy-thread workers)
    workers))

(defun destroy-all-web-tasks ()
  "Destroy all web listeners and worker threads.

May  make a  second (or  subsequent) pass  to try  to clean  up non-idle
worker threads  after 1 second,  but no  guarantee that it  will destroy
them all."
  (destroy-all-listeners)
  (while (destroy-all-idle-workers)
    (sleep 1)))

(defparameter *trace-output-heartbeat-time* 90
  "A  thread  listing   is  dumped  every  *TRACE-OUTPUT-HEARTBEAT-TIME*
seconds into the verbose log.")

(defvar *running-main-loop* nil)

(defun register-signal-handlers ()
  (setf (trivial-signal:signal-handler :term) (lambda (signal)
                                                (declare (ignore signal))
                                                (setf *running-main-loop* nil))
        (trivial-signal:signal-handler :hup) (lambda (signal)
                                               (declare (ignore signal))
                                               (setf *running-main-loop* :reload))))

(defun start-production (&key host port)
  "Start a Hunchentoot  server via `START' and daemonize with Swank.

This is the entry point for running a Production, stand-alone server.

SBCL's Low-level Debugger  is disabled, so crashes  are instantly fatal,
allowing SystemD to start a new instance in case of a fatal error."
  (disable-sbcl-ldb)
  (hook-into-debugger)
  (set-up-for-daemon/start-logging)
  (v:info :starting "Starting on host interface ~a port ~a" host port)
  (start :host host :port port)
  (v:info :starting "Starting Swank")
  (start-swank)
  (register-signal-handlers)
  (setf *running-main-loop* t)
  (loop
     (case *running-main-loop*
       ((t) (when (zerop (mod (get-universal-time*)
                              *trace-output-heartbeat-time*))
              (trace-output-heartbeat)))
       ((nil) (stop-production))
       (:reload (reload-production)))
     (sleep 30)))

(defun stop-production ()
  (stop-listening-for-websockets)
  (ws-evacuate-all)
  (stop-game-metronome)
  (stop)
  (sb-ext:quit :unix-status 0))

(defun reload-production ()
  (v:warn :reload "About to reload source and config")
  (asdf:load-asd (asdf:system-source-file :Tootsville))
  (ql:quickload :Tootsville)
  (v:warn :reload "Reload complete, now running Tootsville server version ~a"
          (asdf:component-version (asdf:find-system :Tootsville)))
  (v:warn :reload "Reloading config file ~s"
          (load-config)))


;;; Recompilation

(defun rebuild-myself ()
  "Recompile the running server.

Hopefully you've already tested the changes?"
  (asdf:load-asd (asdf:system-relative-pathname :Tootsville "Tootsville.asd"))
  (ql:quickload :Tootsville))



(defun connect-databases ()
  "Connect all database systems in parallel (each in its own thread)"
  (dolist (thread (mapcar (lambda (n)
                            (make-thread n :name (symbol-munger:lisp->english n)))
                          '(connect-cache connect-maria)))
    (assert (join-thread thread))))



(defun describe-system (system s)
  "Describes an ASDF system SYSTEM to stream S.

This is used  to obtain the title, description,  author, maintainer, and
license information, as  well as the contents of any  COPYING or LICENSE
file, in TeXinfo format for inclusion in the manual.

@subsection Example Output

@verbatim
@subsection System Tootsville

 The server software monolith for REST services of Tootsville.org

Author: Bruce-Robert Pocock <BRPocock@@interworldly.com>

License: AGPL v3+

@end verbatim
"
  (format s "~2%@subsection System ~:(~a~)" (double-@ (asdf:component-name system)))
  (when-let (description (asdf:system-description system))
    (format s "~2% ~a" (double-@ description)))
  (when-let (author (asdf:system-author system)) 
    (format s "~2%Author: ~a" (double-@ author)))
  (when-let (maintainer (asdf:system-maintainer system))
    (format s "~2%Maintainer: ~a" (double-@ maintainer)))
  (when-let (license (asdf:system-license system))
    (format s "~2%License: ~a" (double-@ license)))
  (ignore-errors
   (when-let (credits (or (probe-file (asdf:system-relative-pathname system "COPYING"))
                          (probe-file (asdf:system-relative-pathname system "COPYING.txt"))))
     (format s "~2%@verbatim~%~a~%@end verbatim~2%"
             (read-file-into-string credits))))
  (ignore-errors
   (when-let (credits (or (probe-file (asdf:system-relative-pathname system "CREDITS"))
                          (probe-file (asdf:system-relative-pathname system "CREDITS.txt"))))
     (format s "~2%@verbatim~%~a~%@end verbatim~2%"
             (read-file-into-string credits)))))

(defun all-credits ()
  "Obtain the credits for every system upon which Tootsville is dependant.

Obtains the information from `DESCRIBE-SYSTEM' while descending the tree
of dependancies from Tootsville through ASDF."
  (let ((systems-seen (list))
        (systems (list (asdf:find-system :Tootsville))))
    (with-output-to-string (s)
      (loop 
         (when (emptyp systems)
           (return-from all-credits (get-output-stream-string s)))
         (let ((system (pop systems)))
           (pushnew system systems-seen :test 'equalp)
           (describe-system system s)
           (dolist (other-system (mapcar #'asdf:find-system
                                         (remove-if #'consp
                                                    (asdf:component-sideway-dependencies system))))
             (unless (member other-system systems-seen)
               (pushnew other-system systems :test 'equalp))))))))



;;; Debugger hook

(defvar *verbose-bugs* nil
  "The client to whom to report bugs. See `TOOTSVILLE-USER::VERBOSEBUGS'")

(defvar *original-debugger-hook* nil
  "The value of `*DEBUGGER-HOOK*' saved by `HOOK-INTO-DEBUGGER'")

(defun hook-into-debugger ()
  "Hook the Tootsville `DEBUGGER-HOOK' into `*DEBUGGER-HOOK*'."
  (setf *original-debugger-hook* *debugger-hook*
        *debugger-hook* 'debugger-hook))

(defun debugger-hook (condition myself-ish)
  "Handle an unhandled error in Production.

If `*VERBOSE-BUGS*' is set, a private admin message is sent to the
client identified by it with the condition text and a stack backtrace.

The condition goes to the `VERBOSE:ERROR' logging.

The condition and backtrace go to `*ERROR-OUTPUT*'.

Calls `ROLLBAR:DEBUGGER-HOOK' to report the condition to Rollbar.

Falls through to any other `*DEBUGGER-HOOK*' that may exist.
"
  (declare (ignore myself-ish))
  (when *verbose-bugs*
    (private-admin-message "Debugger-Hook"
                           (format nil "Unhandled condition <Q>~/HTML/</Q>. <PRE>~/HTML/</PRE>"
                                   condition
                                   (trivial-backtrace:backtrace-string))
                           :user *client*))

  (v:error :error "Unhandled condition: ~a" condition)
  (format *error-output* "~&~|~%Unhandled condition: ~a~%" condition)
  (trivial-backtrace:print-backtrace-to-stream *error-output*)
  (format *error-output* "~2%~|~%")

  (rollbar:debugger-hook condition)

  ;; fall through to next hook
  (when *original-debugger-hook*
    (funcall *original-debugger-hook* condition *original-debugger-hook*)))
