(in-package :tootsville)

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
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output *error-output*))

(defun greeting/daemon/log-output ()
  (v:info :logging
          "~%Log-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun greeting/daemon/standard-output ()
  (format t "~%Standard-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output))

(defun greeting/daemon/trace-output ()
  (format *trace-output*
          "~%Trace-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output *trace-output*))

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

(defun trace-output-heartbeat ()
  (format *trace-output* "~&//* Still Alive (~a)" (now))
  (format *trace-output* "~{~&//  ~a~}" (bt:all-threads))
  (format *trace-output* "~&/**")
  (force-output *trace-output*))

(defun banner/query-io ()
  (write-string (tootsville-v-banner) *query-io*)
  (finish-output *query-io*))

(defun banner/log ()
  (v:info :startup (tootsville-v-banner))
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
