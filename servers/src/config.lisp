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
  (when (developmentp)
    (setf thread-pool-taskmaster:*developmentp* t))
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
         (:test |test|)))))

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
  (or (uiop:getenv (config-env-var #.(package-name *package*)))
      (case (cluster)
        ((nil :test) "test.tootsville.net")
        (:qa "qa.tootsville.net")
        (:prod "tootsville.net")
        (otherwise (format nil "Cluster.~a" *cluster*)))))

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
      (let ((testp (let ((hostname (string-downcase (machine-instance))))
                     (or (search "test.tootsville.net" hostname)
                         (search "dev." hostname)
                         (search "-dev" hostname)
                         (search "builder" hostname)
                         ;; personal workstations, etc:
                         (not (search "tootsville" hostname)))))
            (qa-p (let ((hostname (string-downcase (machine-instance))))
                    (or (search "qa.tootsville.net" hostname)
                        (search "qa." hostname)
                        (search "-qa" hostname)))))
        (setf *cluster* (cond
                          (testp :test)
                          (qa-p :qa)
                          (t :prod))))))

(defun developmentp ()
  "Returns true if this is a Test cluster"
  (or (eql (cluster-name) :test)
      (null (cluster-name))))

(defun productionp ()
  "Returns true if this is the Production cluster"
  (not (developmentp)))
