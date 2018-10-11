;;;; version.lisp — version info
(in-package :tootsville)

(defun romance-ii-program-version ()
  "This program's version. Taken from ASDF."
  (asdf:component-version (asdf:find-system :tootsville)))

(defun romance-ii-program-name ()
  "This program's name. Taken from ASDF."
  (asdf:component-name (asdf:find-system :tootsville)))

(defun romance-ii-program-name/version ()
  "This program's name and version number, in name/version form, as used
in HTTP headers and such."
  (concatenate 'string
               (romance-ii-program-name)
               "/"
               (romance-ii-program-version)))

(defun ensure-site-name ()
  (unless (and (short-site-name)
               (long-site-name))
    #+sbcl
    (progn
      (setf sb-sys:*short-site-name* "Tootsville.org"
            sb-sys:*long-site-name*
            "Tootsville: A free, fun adventure game for the entire family"))))

(defun null-if-empty (string)
  (and (not (emptyp string)) string))


(defvar *romance-ii-copyright-latest*)

(eval-when (:compile-toplevel :load-toplevel)
  (defun romance-ii-copyright-latest ()
    (if (boundp '*romance-ii-copyright-latest*)
        *romance-ii-copyright-latest*
        (setf *romance-ii-copyright-latest*
              (apply #'max (map-asdf-files #'file-write-year
                                           (asdf:find-system :tootsville))))))
  (romance-ii-copyright-latest))

(defun tootsville-v-banner ()
  (format nil
          "~&~|
Tootsville Ⅴ, version ~a.

Copyright © 2016,2017, Bruce-Robert Pocock
Copyright  ©  2018~@[-~d~],  the  Corporation  for  Inter-World  Tourism
and Adventuring (ciwta.org).

Licensed  under the  terms of  the  GNU Affero  General Public  License,
version 3.~%~%"
          (romance-ii-version)
          (= 2018 *romance-ii-copyright-latest*)
          *romance-ii-copyright-latest*))

(defun version-info-list ()
  (ensure-site-name)
  (let ((basics
         (list :product (romance-ii-program-name)
               :version (romance-ii-program-version)
               :copyright (format nil "© ~d Bruce-Robert Pocock"
                                  (romance-ii-copyright-latest))
               :environment (list :configuration (cond ((developmentp) "development")
                                                       ((productionp) "production")
                                                       (t "unknown"))
                                  :developmentp (developmentp)
                                  :productionp (productionp))
               :machine (list :version (unembarassing (machine-version))
                              :type (machine-type)
                              :instance (string-capitalize (machine-instance)))
               :site (list :short-name (short-site-name)
                           :long-name (long-site-name))
               :lisp (list :type (lisp-implementation-type)
                           :version (lisp-implementation-version))
               :software (list :type (software-type)
                               :version (software-version))
               :copyright-latest (romance-ii-copyright-latest)
               :build-date tootsville::*build-date*
               :compiled tootsville::*compiled*)))
    (when (and (boundp 'hunchentoot:*request*) hunchentoot:*request*)
      (appendf basics
               (list :request (list :name (hunchentoot:local-addr*)
                                    :port (hunchentoot:local-port*)
                                    :protocol (hunchentoot:server-protocol*)))))

    (when (and (boundp 'hunchentoot:*acceptor*) hunchentoot:*acceptor*)
      (appendf
       basics
       (list :acceptor
             (list :class (string-capitalize
                           (class-name
                            (class-of hunchentoot:*acceptor*)))
                   :name (hunchentoot:acceptor-name hunchentoot:*acceptor*)
                   :port (hunchentoot:acceptor-port hunchentoot:*acceptor*)
                   :address (hunchentoot:acceptor-address hunchentoot:*acceptor*)))))
    basics))

(defun extract-plist-path (path plist &optional prefix)
  (labels ((prefixed (key)
             (if prefix
                 (concatenate 'string prefix "/"
                              (string key))
                 (string key))))
    (etypecase path
      (null (if (consp plist)
                (loop for (key . value) on plist by #'cddr
                   append (extract-plist-path nil (car value)
                                              (prefixed key)))
                (list prefix plist)))
      (cons (extract-plist-path (rest path) (getf plist (first path))
                                (prefixed (first path))))
      (symbol (let ((value (getf plist path)))
                (if (consp value)
                    (extract-plist-path nil value (prefixed path))
                    (list (prefixed path) value)))))))

(assert (equalp (extract-plist-path nil '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 "X" 0)))
(assert (equalp (extract-plist-path '(:a :b) '(:a (:b 42)))
                '("A/B" 42)))
(assert (equalp (extract-plist-path :a '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 )))
(assert (equalp (extract-plist-path '(:a) '(:a (:b 42 :c 99) :x 0))
                '("A/B" 42 "A/C" 99 )))

(defun version-info-for (args)
  (let ((keys (mapcar (lambda (name)
                        (intern (string-upcase (string name))
                                :keyword))
                      (or args '(:*))))
        (info (version-info-list)))
    (loop for key in keys
       appending
         (cond
           ((find #\/ (string key))
            (extract-plist-path (mapcar #'make-keyword
                                        (split-sequence #\/ (string key)))
                                info))
           ((equal :* key)
            (extract-plist-path nil info))
           (t (list key (getf info key)))))))

(defun version-info-report-string (args)
  (with-output-to-string (s)
    (let ((info (version-info-for args)))
      (cond ((and (= 2 (length info))
                  (= 1 (length args))
                  (string-equal (first args) (first info)))
             (princ (second info) s)
             (terpri s))
            (t (format s "~{~:(~a~):	~a~%~}" info))))))

(defun tootsville::version-info-report (&optional (args '(:*)))
  (format t (version-info-report-string args))
  (finish-output))
