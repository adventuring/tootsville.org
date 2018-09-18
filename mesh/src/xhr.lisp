(in-package :xhr)

(defun network-error (condition)
  (#j:alert "A network error occurred. Is the network disconnected?"))

(defun xhr-report-errors (xhr)
  (setf (oget xhr "onerror")
        (lambda (condition)
          (network-error condition))))

(defun server-url (suffix)
  (concatenate 'string (getf (getf *game-state* :server-info) :url) suffix))

(defun xhr-set-up-post (xhr uri on-load)
  ((oget xhr "open") "PUT" (server-url uri))
  ((oget xhr "setRequestHeader") "Accept" "application/json")
  ((oget xhr "setRequestHeader") "Content-Type" "application/x-www-form-urlencoded")
  (setf (oget xhr "onload") on-load)
  (xhr-report-errors xhr))

(defun get (uri callback &optional headers)
  (error "No get"))

(defmacro with-get ((uri response &optional headers)
                    &body body)
  `(get ,uri (lambda (,response) ,@body nil) ,headers))

(defun post (uri callback &optional fields headers)
  (error "No post"))

(defmacro with-post ((uri response &optional fields headers)
                     &body body)
  `(post ,uri (lambda (,response) ,@body nil) ,fields ,headers))
