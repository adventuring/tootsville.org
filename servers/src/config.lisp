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
                                            :subject-prefix "Error")
                 :run-dir '(:home "run")
                 :log-dir '(:home "logs" "Tootsville")))

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



(defgeneric extract-key-path% (collection key)
  (:method ((collection cons) (key symbol))
    (getf collection key))
  (:method (collection (key integer))
    (elt collection key))
  (:method ((collection hash-table) key)
    (gethash key collection))
  (:method ((collection array) (indices cons))
    (apply #'aref collection indices))
  (:method ((collection null) key)
    (declare (ignore key))
    (values))
  (:method ((jso st-json:jso) (key string))
    (st-json:getjso key jso))
  (:method ((jso st-json:jso) (key symbol))
    (st-json:getjso (kebab:to-camel-case (string key)) jso))
  (:method ((jso st-json:jso) (key integer))
    (st-json:getjso (kebab:to-camel-case (stringify key)) jso)))

(defun extract (collection key &rest more-keys)
  "Extract the  item identified  by KEY  from COLLECTION.  If MORE-KEYS,
then extract an item from each subsequently nested collection.

 • For a list with a symbol key, uses GETF
 • For a sequence and integer, uses ELT
 • For a hash-table, uses GETHASH
 • For an array, uses AREF"
  (extract-key-path% collection key more-keys))

(assert (= 4 (extract '(:a (:b (:c (:d 4)))) :a :b :c :d)))
(assert (= 4 (extract '(:a (1 2 3 4)) :a 3)))
(let ((h (make-hash-table :test 'equalp)))
  (setf (gethash "monkey" h) "George")
  (assert (string= "George" (extract `(:a (:b ,h)) :a :b "monkey"))))



(defun config (&optional key &rest sub-keys)
  (if sub-keys
      (extract (config key) sub-keys)
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
             (let ((hostname (string-downcase (machine-instance))))
               (or (search "test.tootsville.org" hostname)
                   (search "qa.tootsville.org" hostname)
                   (search "dev." hostname)
                   (search "-dev" hostname)
                   (search "builder" hostname)
                   (not (search "tootsville" hostname))))))
        (setf *developmentp* (if developmentp
                                 :devel
                                 :prod))
        developmentp)))

(defun productionp ()
  (not (developmentp)))
