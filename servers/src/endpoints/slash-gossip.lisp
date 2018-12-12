(in-package :Tootsville)

(defun user= (a b &rest rest)
  (and (equal (user-id a) (user-id b))
       (if rest (apply #'user= a rest)
           t)))


(defun sdp-to-key (sdp)
  (concatenate 'string "sdp…#" (sha1-hex sdp)))

(defun find-user-by-sdp (sdp)
  (read-from-string (clouchdb:get-document (sdp-to-key sdp))))

(defun request-param-value (param)
  (hunchentoot:parameter param))

(defun update-session-details ()
  (let ((origin (concatenate 'string "http←" (hunchentoot:remote-addr*)))
        (login (find-record 'db.login
                            "player" (db.person-uuid *user*)
                            "credentials" *credentials*)))
    (if login 
        (unless (equal origin (db.login-origin login))
          (setf (db.login-origin login) origin)))
    (make-record 'db.login :player (db.person-uuid *user*) :origin origin
                 :credentials *credentials*)
    (setf (db.login-last-seen login) (now))
    (save-record login)))

(defun gossipnet-update-client ()
  (update-session-details)
  (when-let (sdp (hunchentoot:parameter "sdp"))
    (unless (find-user-by-sdp sdp)
      ())))


(defun user-info (&optional (user *user*))
  (gossipnet-update-client)
  (let ((partial
         (list :id (user-id user)
               :toots (mapcar #'toot-info (player-toots)))))))



(defendpoint (:post "/gossip/answer" "application/json")
  "Get an answer to a WebRTC initiation request.

This  is a  COMET type  endpoint;  you may  have  to wait  a moment  for
a reply.

@subsection{Status: 200 OK}

The response will contain the WebRTC session initiation data you need to
join  the  Gossipnet. Attempt  to  connect;  if  it  fails, try  to  PUT
a fresh request.

@subsection{Status: 204 It's always dark in the beginning}

This shouldn't be  returned; it means that there are  zero known players
in the universe.

@subsection{Status: 409 That won't work any more}

You've already gotten a response to  your most recently PUT request; See
PUT /gossip/request for details.

@subsection{Status: 429 Too many requests)

You are submitting requests too often. Wait before retrying.
"
  (let ((answeror *user*)
        (offeror (find-user-by-sdp (hunchentoot:parameter "offeror"))))
    (declare (ignore answeror)) ; for now TODO
    (cond ((user-sdp-answer offeror)
           (list 409 nil '(:offeror "not-available")))
          (t
           (setf (user-sdp-answer offeror) (hunchentoot:parameter "answer"))
           (list 202 nil (list :did "202 (TODO)"))))))



(defendpoint (put "/gossip/request" "application/json")
  "Request a random player to join you with WebRTC.

PUT a WebRTC session initiation request to this location. You'll receive
back the location from which to await your answer.

@subsection{Status: 202 Submitted request}

The response will be a JSON object with one key, \"location\". The value
will be an URI from which to request a response. Submit a GET request to
that URI and await a reply (COMET style).
"
  (with-user ()
    (list 202 '(:location "/gossip/answer")
          (user-info))))
