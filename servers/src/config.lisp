(in-package :Tootsville)

(setf (config-env-var) "TOOTSVILLE")

(defparameter *application-root*
  (asdf:system-source-directory :Tootsville))

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
      :on-error-mail (:from-name "Tootsville Support"
                      :from-address "support@Tootsville.org"
                      :to-address "support@Tootsville.org"
                      :smtp-server "localhost"
                      :subject-prefix "Error")))

(defun default-config-file ()
  (merge-pathnames
   (make-pathname
    :name "Tootsville.config"
    :type "lisp"
    :version :newest
    :directory '(:relative ".config" "Tootsville"))
   (user-homedir-pathname)))

(defvar *config-file* nil
  "Metadata about the configuration file last loaded")

(defgeneric load-config (&optional config-file))
(defmethod load-config (&optional (config-file (default-config-file)))
  (load config-file)
  (setf *config-file* (list :path config-file
                            :truename (truename config-file)
                            :read (get-universal-time)
                            :host (machine-instance)
                            :file-write-date (file-write-date config-file)
                            :author (file-author config-file))))

(when (probe-file (default-config-file))
  (load-config))



(defgeneric extract-key-path% (collection key more-keys)
  (:method ((collection cons) (key symbol) more-keys)
    (declare (ignore more-keys))
    (getf collection key))
  (:method (collection (key integer) more-keys)
    (declare (ignore more-keys))
    (elt collection key))
  (:method ((collection hash-table) key more-keys)
    (declare (ignore more-keys))
    (gethash key collection))
  (:method ((collection array) (indices cons) more-keys)
    (declare (ignore more-keys))
    (apply #'aref collection indices))
  (:method ((collection null) key more-keys)
    (values))
  (:method :around (collection key (more-keys cons))
    (extract-key-path% (call-next-method)
                       (first more-keys)
                       (rest more-keys))))

(defun extract-key-path (collection key &rest more-keys)
  (extract-key-path% collection key more-keys))

(assert (= 4 (extract-key-path '(:a (:b (:c (:d 4))))
                               :a :b :c :d)))
(assert (= 4 (extract-key-path '(:a (1 2 3 4)) :a 3)))
(let ((h (make-hash-table :test 'equalp)))
  (setf (gethash "monkey" h) "George")
  (assert (string= "George" (extract-key-path `(:a (:b ,h))
                                              :a :b "monkey"))))



(defun config (&optional key &rest sub-keys)
  (if sub-keys
      (extract-key-path (config key) sub-keys)
      (envy:config #.(package-name *package*) key)))

(defun appenv ()
  (or (uiop:getenv (config-env-var #.(package-name *package*)))
      (if (developmentp)
          "development"
          "production")))

(defvar *developmentp* nil)

(defun developmentp ()
  (if *developmentp*
      (ecase *developmentp*
        (:devel t)
        (:prod nil))
      (let ((developmentp
              (let ((hostname (machine-instance)))
                (or (search hostname "Tootsville.ga")
                    (search hostname "dev.")
                    (search hostname "-dev")
                    (search hostname ".ga'")
                    (not (search hostname "Tootsville"))))))
        (setf *developmentp* (if developmentp
                                 :devel
                                 :prod))
        developmentp)))

(defun productionp ()
  (not (developmentp)))
