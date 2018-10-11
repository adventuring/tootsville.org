(in-package :Tootsville)

(defvar *gossip-users* nil)
(defvar *user*)

(defclass gossip-user ()
  ((google-token :type string
                 :accessor user-google-token
                 :initarg :google-token)
   (facebook-token :type string
                   :accessor user-facebook-token
                   :initarg :facebook-token)
   (id :type fixnum
       :accessor user-id
       :initarg :id)
   (remote-address :type string
                   :accessor user-remote-address
                   :initarg :remote-address)
   (sdp-offer :type string
              :accessor user-sdp-offer
              :initarg :sdp-offer
              :initform nil)
   (sdp-answer :type string
               :accessor user-sdp-answer
               :initarg :sdp-answer
               :initform nil)))

(defun make-gossip-user (&rest args)
  (apply #'make-instance 'gossip-user args))

(defun user= (a b &rest rest)
  (and (equal (user-id a) (user-id b))
       (if rest (apply #'user= a rest)
           t)))

(defun find-user-by-sdp (sdp)
  (find-if (curry #'equal sdp) *gossip-users*
           :key #'user-sdp-offer))

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
    (or (find-if (curry #'equal token) *gossip-users*
                 :key #'user-google-token)
        (with-connection (:members)
          (or (datafly:retrieve-one
               (sxql:select (:*)
                            :from :users
                            :where '(:= :google-token token))
               :as 'gossip-user)
              (progn
                (datafly:execute (sxql:insert-into :users
                                                   :google-token token))
                (datafly:retrieve-one
                 (sxql:select (:*)
                              :from :users
                              :where '(:= :id (:last_insert_id)))
                 :as 'gossip-user)))))))

(defun request-param-value (param)
  (hunchentoot:parameter param))

(defun gossipnet-update-client (&optional (user *user*))
  (setf (user-remote-address user) (hunchentoot:remote-addr*))
  (when-let (sdp (hunchentoot:parameter "sdp"))
    (when (not (equal sdp (user-sdp-offer user)))
      (setf (user-sdp-answer user) nil))
    (setf (user-sdp-offer user) sdp))
  (pushnew user *gossip-users* :test #'user=))

(define-condition user-not-identified-error (error)
  ((source :initarg :source :reader user-not-identified-source)
   (value :initarg :value :reader user-not-identified-value))
  (:documentation "I could not identify any user by the credentials provided.")
  (:report (lambda (c s)
             (format s
                     "The credentials provided did not identify any user.~
~[ ~:*The credential source was: ~:(~a~)~]~
~[ ~:*The value provided was: ~:(~a~)~]"
                     (user-not-identified-source c)
                     (user-not-identified-value c)))))

(defmethod respond-to-error ((error user-not-identified-error))
  (setf (hunchentoot:return-code*) 401)
  (hunchentoot:abort-request-handler))

(defun find-user-from-session (&key (if-not-exists nil))
  (if-let ((google-token (hunchentoot:parameter "google-api-token")))
      (find-user-by-google-token google-token)
    (ecase if-not-exists
      (nil nil)
      (:error (error 'user-not-identified-error
                     :source :session
                     :value (list :google-api-token nil))))))

(defun active-sdp-offers (&optional (user *user*))
  (mapcar (rcurry #'drakma:url-encode :utf-8)
          (mapcar #'user-sdp-offer
                  (remove-if #'user-sdp-answer
                             (remove-if-not #'user-sdp-offer
                                            (remove-if (curry #'user= user)
                                                       *gossip-users*))))))

(defun user-info (&optional (user *user*))
  (gossipnet-update-client)
  (let ((partial
         (list :id (user-id user)
               :toots (mapcar #'toot-info (player-toots))
               :offers (mapcar #'stringify (active-sdp-offers user)))))
    (if-let (answer (user-sdp-answer user))
        (append (list :answer answer) partial)
      partial)))

(defendpoint (:post "/gossip/answer" "application/json")
    (let ((answeror (find-user-from-session))
          (offeror (find-user-by-sdp (hunchentoot:parameter "offeror"))))
      (declare (ignore answeror)) ; for now TODO
      (cond ((user-sdp-answer offeror)
             (list 409 nil '(:offeror "not-available")))
            (t
             (setf (user-sdp-answer offeror) (hunchentoot:parameter "answer"))
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
      (if (member *user* *gossip-users* :test #'user=)
          (list 200 nil (user-info))
          (list 402 nil (:to-authenticate )))))
