(in-package :Tootsville)

(setf (config-env-var) "TOOTSVILLE")

(defparameter *application-root*
  (asdf:system-source-directory :Tootsville)
  "The location in which the application source code is installed.")

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
                 :on-error-mail (:from-name "Tootsville Support"
                                            :from-address "support@Tootsville.org"
                                            :to-address "support@Tootsville.org"
                                            :smtp-server "localhost"
                                            :subject-prefix "Error")
                 :run-dir '(:home "run")
                 :log-dir '(:home "logs" "Tootsville")))

(defun default-config-file ()
  "Returns the name of the default configuration file."
  (merge-pathnames
   (make-pathname
    :name "Tootsville.config"
    :type "lisp"
    :version :newest
    :directory '(:relative ".config" "Tootsville"))
   (user-homedir-pathname)))

(defvar *config-file* nil
  "Metadata about the configuration file last loaded")

(defun load-config (&optional (config-file (default-config-file)))
  "Load the configuration from CONFIG-FILE."
  (load config-file)
  ;; configure other packages
  (setf thread-pool-taskmaster:*developmentp* (config :devel :taskmaster)
        hunchentoot:*show-lisp-errors-p* (config :devel :hunchentoot :show-errors)
        hunchentoot:*show-lisp-backtraces-p* (config :devel :hunchentoot :show-backtraces))
  (apply #'rollbar:configure (config :rollbar))
  (setf *config-file* (list :path config-file
                            :truename (truename config-file)
                            :read (get-universal-time)
                            :host (machine-instance)
                            :file-write-date (file-write-date config-file)
                            :author (file-author config-file))))





(defun config (&rest keys)
  "Obtain the configuration value at the path KEY + SUB-KEYS"
  (cond
    (keys (apply #'extract (config) keys))
    (t (ecase (cluster)
         (:devel |devel|)
         (:test |test|)
         (:qa |qa|)
         (:prod |prod|)))))

(defvar |devel|) (defvar |test|) (defvar |qa|) (defvar |prod|)

(defun cluster-name ()
  "Get the name of the active cluster.

Currently one of:

@itemize
@item
test.tootsville.org
@item
qa.tootsville.org
@item
tootsville.org
@end itemize
"
  (or (uiop:getenv (config-env-var #.(package-name *package*)))
      (case (cluster)
        ((nil :test) "test.tootsville.org")
        (:qa "qa.tootsville.org")
        (:prod "tootsville.org")
        (otherwise (format nil "Cluster.~a" *cluster*)))))

(defun cluster-net-name ()
  (case (cluster)
    ((nil :test) "test.tootsville.net")
    (:qa "qa.tootsville.net")
    (:prod "tootsville.net")
    (:devel (format nil "devel.~a" *cluster*))))

(defvar *cluster* nil
  "Cache for `CLUSTER' (qv)")

(defun cluster ()
  "Get the identity of the current cluster.

Returns one of:
@itemize
@item
 :test
@item
:qa
@item
:prod
@end itemize"
  (or *cluster*
      (setf *cluster* (let ((hostname (string-downcase (machine-instance))))
                        (cond (((or (search "dev" hostname)
                                    (search "builder" hostname)
                                    ;; personal workstations, etc:
                                    (not (search "tootsville" hostname))) :devel)
                               ((search "test" hostname) :test)
                               ((search "qa" hostname) :qa)
                               ((search ".tootsville.net" hostname) :prod)
                               (t (warn "Could not identify the cluster")
                                  ())))))))

