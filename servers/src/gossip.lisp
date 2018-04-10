(in-package :tootsville)

(defvar *gossip-users* nil)
(defvar *user*)

(defclass gossip-user ()
  ((google-token :type string :accessor user-google-token :initarg :google-token)
   (facebook-token :type string :accessor user-facebook-token :initarg :facebook-token)
   (id :type fixnum :accessor user-id :initarg :id)
   (remote-address :accessor user-remote-address :initarg :remote-address)
   (sdp-offer :accessor user-sdp-offer :initarg :sdp-offer :initform nil)
   (sdp-answer :accessor user-sdp-answer :initarg :sdp-answer :initform nil)))

(defun make-user (&rest args)
  (apply #'make-instance 'user args))

(defun user= (a b &rest rest)
  (and (equal (user-id a) (user-id b))
       (if rest (apply #'user= a rest)
           t)))

(defun find-user-by-sdp (sdp)
  (when-let (found (remove-if-not (lambda (user)
                                    (equal sdp (user-sdp-offer user)))
                                  *gossip-users*))
    (first found)))

(defun find-user-by-google-token (id-token)
  (let ((token (handler-case
                   (user<-google-token id-token)
                 (simple-error (c)
                   (declare (ignore c))
                   (when (member (hunchentoot:remote-addr*)
                                 '("localhost" "::1" "127.0.0.1") :test 'equal)
                     (warn "Falling back on special case for localhost testing")
                     (format nil "~a:~d"
                             (subseq id-token 0 (position #\. id-token))
                             (hunchentoot:remote-port*)))))))
    (if-let (found (remove-if-not (lambda (user)
                                    (equal token (user-google-token *user*)))
                                  *gossip-users*))
      (first found)
      (with-connection (:members)
        (or (datafly:retrieve-one (sxql:select (:*)
                                               :from :users
                                               :where '(:= :google-token token))
                          :as 'user)
            (progn
              (datafly:execute (sxql:insert-into :users
                                                 :google-token token))
              (datafly:retrieve-one (sxql:select (:*)
                                                 :from :users
                                                 :where '(:= :id (:last_insert_id)))
                            :as 'user)))))))

(defun request-param-value (param)
  (hunchentoot:parameter param))

(defun gossipnet-update-client (&optional (user *user*))
  (setf (user-remote-address user) (hunchentoot:remote-addr*))
  (when-let (sdp (request-param-value "sdp"))
    (when (not (equal sdp (user-sdp-offer user)))
      (setf (user-sdp-answer user) nil))
    (setf (user-sdp-offer user) sdp))
  (pushnew user *gossip-users* :test #'user=))

(defun find-user-from-session ()
  (when-let ((google-token (request-param-value "google-api-token")))
    (find-user-by-google-token google-token)))

(defun active-sdp-offers (user)
  (map 'list (rcurry #'drakma:url-encode :utf-8)
       (map 'list #'user-sdp-offer
            (remove-if #'user-sdp-answer
                       (remove-if-not #'user-sdp-offer
                                      (remove-if (curry #'user= *user*)
                                                 *gossip-users*))))))

(defun stringify (object)
  (format nil "~a" object))

(defun user-info (&optional (user *user*))
  (gossipnet-update-client user)
  (let ((partial 
         (list :id (user-id *user*)
               :toots (map 'list #'toot-info (player-toots))
               :offers (map 'list #'stringify (active-sdp-offers *user*)))))
    (if-let (answer (user-sdp-answer user))
      (append (list :answer answer) partial)
      partial)))

(defendpoint (:post "/gossip/answer" "application/json")
  (let ((answeror (find-user-from-session))
        (offeror (find-user-by-sdp (request-param-value "offeror"))))
    (declare (ignore answeror)) ; for now TODO
    (cond ((user-sdp-answer offeror)
           (list 409 nil '(:offeror "not-available")))
          (t
           (setf (user-sdp-answer offeror) (request-param-value "answer"))
           (list 202 nil (list :did "202 (TODO)"))))))

(defmacro with-user (() &body body)
  `(let ((*user* (find-user-from-session)))
     (unless *user*
       (return-from endpoint 
         (list 403 nil *403.JSON-BYTES*)))
     ,@body))

(defendpoint (:put "/gossip" "application/json")
  (with-user ()
    (list 201 '(:location "/gossip/fixme")
          (user-info *user*))))

(defendpoint (:get "/gossip" "application/json")
  (with-user ()
    (if (member user *gossip-users* :test #'user=)
        (list 200 nil (user-info))
        (list 403 nil *403.JSON-BYTES*))))
