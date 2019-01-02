;;;; -*- lisp -*-
;;;
;;;; ./servers/src/config.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
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
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)

(defparameter *application-root*
  (asdf:system-source-directory :Tootsville)
  "The location in which the application source code is installed.")

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

(defgeneric apply-config ()
  (:documentation "Whenever the configuration is loaded, these methods are called
 to allow “external” packages (which may not use this configuration mechanism)
 to apply settings.")
  (:method-combination progn))

(defmethod apply-config progn ()
  "Set up Hunchentoot and the taskmaster from configuration"
  (setf thread-pool-taskmaster:*developmentp* (config :taskmaster :devel)
        hunchentoot:*catch-errors-p* (config :hunchentoot :catch-errors)
        hunchentoot:*log-lisp-warnings-p* (config :hunchentoot :log-warnings)
        hunchentoot:*log-lisp-errors-p* (config :hunchentoot :log-errors)
        hunchentoot:*log-lisp-backtraces-p* (config :hunchentoot :log-backtraces)
        hunchentoot:*show-lisp-errors-p* (config :hunchentoot :show-errors)
        hunchentoot:*show-lisp-backtraces-p* (config :hunchentoot :show-backtraces)))

(defmethod apply-config progn ()
  "Apply configuration to Rollbar"
  (apply #'rollbar:configure (config :rollbar))
  (setf rollbar:*person-hook* #'get-rollbar-person)
  (rollbar:configure :environment (cluster-net-name)
                     :code-version #.(run-program "git rev-parse HEAD" :output :string)
                     :framework (romance-ii-program-name/version)))

(defmethod apply-config progn ()
  "Set site name from configuration"
  ;; TODO: Set site name from configuration
  )

(defun load-config (&optional (config-file (default-config-file)))
  "Load the configuration from CONFIG-FILE."
  (load config-file)
  (apply-config)
  (setf *config-file* (list :path config-file
                            :truename (truename config-file)
                            :read (now)
                            :host (machine-instance)
                            :file-write-date (universal-to-timestamp
                                              (file-write-date config-file))
                            :author (file-author config-file)))
  (v:info :config "Loaded config from (~{~:(~a~): ~s~^, ~}" *config-file*)
  (v:info :config "Cluster is the ~:(~a~) cluster ~a" (cluster) (cluster-name))
  (values *config-file*
          (cluster)
          (cluster-name)))



(defun config (&rest keys)
  "Obtain the configuration value at the path KEY + SUB-KEYS"
  (cond
    (keys (apply #'extract (config) keys))
    (t (ecase (cluster)
         (:devel |devel|)
         (:test |test|)
         (:qa |qa|)
         (:prod |prod|)))))

(defvar |devel| nil) (defvar |test| nil) (defvar |qa| nil) (defvar |prod| nil)

(defun cluster-name (&optional prefix)
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
  (case (cluster)
    (:test (format nil "~@[~a.~]test.tootsville.org" prefix))
    (:qa (format nil "~@[~a.~]qa.tootsville.org" prefix))
    (:prod (format nil "~@[~a.~]tootsville.org" prefix))
    (:devel (machine-instance))))

(defun cluster-net-name (&optional prefix)
  (case (cluster)
    (:test (format nil "~@[~a.~]test.tootsville.net" prefix))
    (:qa (format nil "~@[~a.~]qa.tootsville.net" prefix))
    (:prod (format nil "~@[~a.~]tootsville.net" prefix))
    (:devel (machine-instance))))

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
      (setf *cluster*
            (let ((hostname (string-downcase (machine-instance))))
              (cond ((or (search "dev" hostname)
                         (search "builder" hostname)
                         ;; personal workstations, etc:
                         (not (search "tootsville" hostname))) :devel)
                    ((search "test" hostname) :test)
                    ((search "qa" hostname) :qa)
                    ((search ".tootsville.net" hostname) :prod)
                    (t (warn "could not identify the cluster")
                       :devel))))))
