(in-package :Tootsville)

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



(defun entry (&optional argv)
  "Top-level  entry-point  for  the  compiled  executable  binary  form.

Dispatches   based   upon   the   single  argument,   expected   to   be
a verb (case-insensitive) from the hard-coded table in this function."
  (case (intern (string-upcase (typecase argv
                                 (cons (if (< 1 (length argv))
                                           (second argv)
                                           "REPL"))
                                 (null "HELP")
                                 (t argv))) :keyword)
    (if (probe-file (default-config-file))
        (load-config)
        (error "No config file found"))
    (banner)
    (force-output)
    (connect-databases)
    (unless *endpoint-list*
      (cerror "try anyway" "All of my endpoints are missing"))
    ((:fcgi :fast-cgi) (or #-common-lisp (fastcgi-entry) (error 'unimplemented)))
    ((:devel :server) (start-hunchentoot :port (if (and (consp argv)
                                                        (< 2 (length argv)))
                                                   (parse-integer (third argv))
                                                   5000)
                                         :host (if (and (consp argv)
                                                        (< 3 (length argv)))
                                                   (fourth argv)
                                                   "127.0.0.1")))
    ((:prod :daemon) (start-production :port (if (and (consp argv)
                                                      (< 2 (length argv)))
                                                 (parse-integer (third argv))
                                                 5000)
                                       :host (if (and (consp argv)
                                                      (< 3 (length argv)))
                                                 (fourth argv)
                                                 "127.0.0.1")))
    ((:test :check) (power-on-self-test :exitp t))
    (:repl (start-repl))
    (:version (print *compiled*))
    (:swank (start-swank)
            (start-repl))
    (:write-docs (write-docs))
    (:version-info (version-info-report (nthcdr 2 argv)))
    (otherwise (print-help))))
