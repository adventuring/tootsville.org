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
  (if more-keys
      (apply #'extract (extract-key-path% collection key) more-keys)
      (extract-key-path% collection key)))

(assert (= 4 (extract '(:a (:b (:c (:d 4)))) :a :b :c :d))
        () "EXTRACT must be able to descend a sequence of dereferences")
(assert (= 4 (extract '(:a (1 2 3 4)) :a 3))
        () "EXTRACT must handle plists and regular lists")
(assert (= 4 (extract '(:a #(1 2 3 4)) :a 3))
        () "EXTRACT must handle plists and arrays")
(assert (= 4 (extract (st-json:read-json-from-string "{\"a\": 4}") "a"))
        () "EXTRACT must handle JSON objects")
(assert (= 4 (extract (st-json:read-json-from-string "{\"fooBar\": 4}") :foo-bar))
        () "EXTRACT must translate symbols to camelCase strings")
(assert (= 4 (extract (st-json:read-json-from-string "{\"a\": [1,2,4,8]}") "a" 2))
        () "EXTRACT must handle JSON objects and lists")
(let ((h (make-hash-table :test 'equalp)))
  (setf (gethash "monkey" h) "George")
  (assert (string= "George" (extract `(:a (:b ,h)) :a :b "monkey"))
          () "EXTRACT must handle plists and hash tables"))



(defun config (&optional key &rest sub-keys)
  "Obtain the configuration value at the path KEY + SUB-KEYS"
  (if sub-keys
      (extract (config key) sub-keys)
      (envy:config #.(package-name *package*) key)))

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
      (ecase *cluster*
        ((developmentp) "test.tootsville.org")
        ((qa-p) "qa.tootsville.org")
        ((productionp) "tootsville.org"))))

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
                     (or (search "test.tootsville.org" hostname)
                         (search "dev." hostname)
                         (search "-dev" hostname)
                         (search "builder" hostname)
                         ;; personal workstations, etc:
                         (not (search "tootsville" hostname)))))
            (qa-p (let ((hostname (string-downcase (machine-instance))))
                    (or (search "qa.tootsville.org" hostname)
                        (search "qa." hostname)
                        (search "-qa" hostname)))))
        (setf *cluster* (cond
                          (testp :test)
                          (qa-p :qa)
                          (t :prod))))))

(defun developmentp ()
  "Returns true if this is a Test or QA cluster"
  (member *cluster* '(:test :qa)))

(defun productionp ()
  "Returns true if this is the Production cluster"
  (not (developmentp)))

