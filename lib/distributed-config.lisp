;;; Distributed Configuration System for Tootsville
;;; Copyright © 2025 Interworldly Adventuring, LLC
;;; 
;;; This program is Free Software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public
;;; License as published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.

(in-package :cl-user)

(defpackage :tootsville.distributed-config
  (:use :cl :cl-json :drakma :bordeaux-threads)
  (:export
   :*config*
   :get-config
   :set-config
   :watch-config
   :reload-config
   :config-value
   :config-path
   :config-environment
   :config-consul
   :config-local
   :config-default))

(in-package :tootsville.distributed-config)

;;; Configuration Sources
(defclass config-source ()
  ((priority :initform 0 :initarg :priority :accessor config-source-priority)
   (name :initform "unknown" :initarg :name :accessor config-source-name)
   (enabled :initform t :initarg :enabled :accessor config-source-enabled)
   (secure :initform nil :initarg :secure :accessor config-source-secure)))

(defclass config-consul (config-source)
  ((host :initform "localhost" :initarg :host :accessor config-consul-host)
   (port :initform 8500 :initarg :port :accessor config-consul-port)
   (token :initform nil :initarg :token :accessor config-consul-token)
   (datacenter :initform nil :initarg :datacenter :accessor config-consul-datacenter)
   (prefix :initform "tootsville/" :initarg :prefix :accessor config-consul-prefix)))

(defclass config-local (config-source)
  ((file-path :initform "config/local.lisp" :initarg :file-path :accessor config-local-file-path)
   (auto-reload :initform t :initarg :auto-reload :accessor config-local-auto-reload)
   (last-modified :initform 0 :accessor config-local-last-modified)))

(defclass config-environment (config-source)
  ((prefix :initform "TOOTSVILLE_" :initarg :prefix :accessor config-environment-prefix)))

(defclass config-default (config-source)
  ((values :initform (make-hash-table :test 'equal) :initarg :values :accessor config-default-values)))

;;; Main Configuration Manager
(defclass distributed-config ()
  ((sources :initform nil :accessor config-sources)
   (cache :initform (make-hash-table :test 'equal) :accessor config-cache)
   (watchers :initform (make-hash-table :test 'equal) :accessor config-watchers)
   (lock :initform (make-lock "config-lock") :accessor config-lock)
   (reload-interval :initform 30 :initarg :reload-interval :accessor config-reload-interval)))

(defvar *config* (make-instance 'distributed-config))

;;; Consul Integration
(defmethod initialize-consul ((config config-consul))
  (handler-case
      (let* ((url (format nil "http://~A:~A/v1/kv/~A" 
                          (config-consul-host config)
                          (config-consul-port config)
                          (config-consul-prefix config)))
             (response (drakma:http-request url :method :get)))
        (when (= (drakma:http-request-status response) 200)
          (log:info "Consul connection established to ~A" url)
          t))
    (error (e)
      (log:warn "Failed to connect to Consul: ~A" e)
      nil)))

(defmethod get-consul-value ((config config-consul) key)
  (handler-case
      (let* ((url (format nil "http://~A:~A/v1/kv/~A~A" 
                          (config-consul-host config)
                          (config-consul-port config)
                          (config-consul-prefix config)
                          key))
             (response (drakma:http-request url :method :get)))
        (when (= (drakma:http-request-status response) 200)
          (let ((data (json:decode-json-from-string (drakma:http-request-body response))))
            (when (and (listp data) (car data))
              (let ((value (cdr (assoc :value (car data)))))
                (when value
                  (base64:base64-string-to-string value)))))))
    (error (e)
      (log:warn "Failed to get Consul value for ~A: ~A" key e)
      nil)))

(defmethod set-consul-value ((config config-consul) key value)
  (handler-case
      (let* ((url (format nil "http://~A:~A/v1/kv/~A~A" 
                          (config-consul-host config)
                          (config-consul-port config)
                          (config-consul-prefix config)
                          key))
             (encoded-value (base64:base64-string-to-string value))
             (response (drakma:http-request url 
                                          :method :put
                                          :content encoded-value)))
        (= (drakma:http-request-status response) 200))
    (error (e)
      (log:warn "Failed to set Consul value for ~A: ~A" key e)
      nil)))

;;; Local File Configuration
(defmethod load-local-config ((config config-local))
  (handler-case
      (let ((file (config-local-file-path config)))
        (when (probe-file file)
          (let ((mod-time (file-write-date file)))
            (when (> mod-time (config-local-last-modified config))
              (setf (config-local-last-modified config) mod-time)
              (load file)
              (log:info "Loaded local config from ~A" file)
              t))))
    (error (e)
      (log:warn "Failed to load local config: ~A" e)
      nil)))

;;; Environment Variable Configuration
(defmethod get-env-value ((config config-environment) key)
  (let ((env-key (format nil "~A~A" 
                         (config-environment-prefix config)
                         (string-upcase key))))
    (uiop:getenv env-key)))

;;; Default Configuration
(defmethod get-default-value ((config config-default) key)
  (gethash key (config-default-values config)))

;;; Main Configuration Interface
(defmethod get-config-value ((config distributed-config) key &optional default)
  (with-lock-held ((config-lock config))
    ;; Check cache first
    (multiple-value-bind (cached found) (gethash key (config-cache config))
      (when found
        (return-from get-config-value cached)))
    
    ;; Try sources in priority order
    (dolist (source (sort (copy-list (config-sources config)) 
                          #'> :key #'config-source-priority))
      (when (config-source-enabled source)
        (let ((value (get-value-from-source source key)))
          (when value
            (setf (gethash key (config-cache config)) value)
            (return-from get-config-value value)))))
    
    ;; Return default if no value found
    default)))

(defmethod get-value-from-source (source key)
  (etypecase source
    (config-consul (get-consul-value source key))
    (config-local (get-local-value source key))
    (config-environment (get-env-value source key))
    (config-default (get-default-value source key))))

(defmethod set-config-value ((config distributed-config) key value)
  (with-lock-held ((config-lock config))
    ;; Update cache
    (setf (gethash key (config-cache config)) value)
    
    ;; Try to persist to highest priority source that supports writing
    (dolist (source (sort (copy-list (config-sources config)) 
                          #'> :key #'config-source-priority))
      (when (and (config-source-enabled source)
                 (typep source 'config-consul))
        (when (set-consul-value source key value)
          (return-from set-config-value t))))
    
    t))

;;; Configuration Watching
(defmethod watch-config ((config distributed-config) key callback)
  (with-lock-held ((config-lock config))
    (push callback (gethash key (config-watchers config) nil))))

(defmethod notify-watchers ((config distributed-config) key old-value new-value)
  (let ((watchers (gethash key (config-watchers config))))
    (dolist (callback watchers)
      (handler-case
          (funcall callback key old-value new-value)
        (error (e)
          (log:warn "Config watcher callback failed: ~A" e))))))

;;; Configuration Reloading
(defmethod reload-config ((config distributed-config))
  (with-lock-held ((config-lock config))
    ;; Clear cache
    (clrhash (config-cache config))
    
    ;; Reload local configs
    (dolist (source (config-sources config))
      (when (typep source 'config-local)
        (load-local-config source)))
    
    ;; Reinitialize Consul connections
    (dolist (source (config-sources config))
      (when (typep source 'config-consul)
        (initialize-consul source)))
    
    (log:info "Configuration reloaded")))

;;; Convenience Functions
(defun get-config (key &optional default)
  (get-config-value *config* key default))

(defun set-config (key value)
  (set-config-value *config* key value))

(defun watch-config (key callback)
  (watch-config *config* key callback))

(defun reload-config ()
  (reload-config *config*))

;;; Default Configuration Values
(defmethod initialize-default-config ((config distributed-config))
  (let ((default-source (make-instance 'config-default :priority 0)))
    (setf (config-default-values default-source)
          (let ((ht (make-hash-table :test 'equal)))
            ;; Database configuration
            (setf (gethash "database.host" ht) "localhost")
            (setf (gethash "database.port" ht) 5432)
            (setf (gethash "database.name" ht) "tootsville")
            (setf (gethash "database.user" ht) "tootsville")
            
            ;; Web server configuration
            (setf (gethash "server.port" ht) 8080)
            (setf (gethash "server.host" ht) "0.0.0.0")
            (setf (gethash "server.workers" ht) 4)
            
            ;; Game configuration
            (setf (gethash "game.max-players" ht) 100)
            (setf (gethash "game.world-size" ht) 1000)
            (setf (gethash "game.tick-rate" ht) 20)
            
            ;; Security configuration
            (setf (gethash "security.session-timeout" ht) 3600)
            (setf (gethash "security.max-login-attempts" ht) 5)
            
            ht))
    (push default-source (config-sources config))))

;;; Initialize the configuration system
(defmethod initialize-config ((config distributed-config))
  ;; Add default configuration
  (initialize-default-config config)
  
  ;; Add environment configuration
  (push (make-instance 'config-environment :priority 10) (config-sources config))
  
  ;; Add local file configuration
  (push (make-instance 'config-local :priority 20) (config-sources config))
  
  ;; Add Consul configuration if available
  (let ((consul-config (make-instance 'config-consul :priority 30)))
    (when (initialize-consul consul-config)
      (push consul-config (config-sources config))))
  
  ;; Initial reload
  (reload-config config)
  
  (log:info "Distributed configuration system initialized"))

;;; Auto-reload thread
(defmethod start-auto-reload ((config distributed-config))
  (let ((thread (make-thread 
                 (lambda ()
                   (loop
                     (sleep (config-reload-interval config))
                     (reload-config config)))
                 :name "config-auto-reload")))
    (log:info "Configuration auto-reload started")
    thread))

;;; Initialize on package load
(initialize-config *config*)
(start-auto-reload *config*)
