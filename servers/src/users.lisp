(in-package :Tootsville)



(defvar *user* nil
  "The currently-signed-in user, if any")

(defun user-display-name (person)
  (db.person-display-name (ensure-person person)))

(defun user-given-name (person)
  (db.person-given-name (ensure-person person)))

(defun user-surname (person)
  (db.person-surname (ensure-person person)))

(defun user-face (person)
  (let* ((uuid (db.person-uuid (ensure-person person)))
	 (portraits (find-records 'db.person-link "rel" :portrait)))
    (when portraits (first portrait))))

(defun user-id (person)
  (db.person-uuid (ensure-person person)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))

(defclass credentials ()
  ((user :type db.person
         :initarg :user
         :reader credentials-user)))

(defclass openid-credentials (credentials)
  ((issuer :type string
           :initarg :issuer
           :initarg :iss
           :reader credentials-issuer
           :reader openid-iss)
   (subject :type string
            :initarg :subject
            :initarg :sub
            :reader credentials-subject-code
            :reader openid-sub)
   (authorized-party :type string
                     :initarg :authorized-party
                     :initarg :azp
                     :reader openid-authorized-party
                     :reader openid-azp)
   (audience :type string
             :initarg :audience
             :initarg :aud
             :reader openid-audience
             :reader openid-aud)
   (issued-at :type timestamp
              :initarg :issued-at
              :initarg :iat
              :reader openid-issued-at
              :reader openid-iat)
   (expires :type timestamp
            :initarg :expires
            :initarg :exp
            :reader openid-expires
            :reader openid-exp)
   (user-id :type uuid
            :initarg :user
            :reader credentials-user)))

(defclass openid-token ()
  ((access-token :type string
                 :initarg :access-token
                 :accessor openid-access-token)
   (token-type :type string
               :initarg :token-type
               :initform "Bearer"
               :accessor openid-token-type)
   (refresh-token :type string
                  :initarg :token-type
                  :accessor openid-refresh-token)
   (token-expires :type timestamp
                  :initarg :token-expires
                  :accessor openid-token-expires)
   (id-token :type string
             :initarg :id-token
             :accessor openid-id-token)))



;;; Toot character data.

(defun find-toot-by-name (toot-name)
  (check-type toot-name toot-name)
  (find-record 'db.toot "name" toot-name))

(defun player-childp (&optional (player *user))
  (< (or (legal-age (db.person-date-of-birth player))
	 (db.person-age player))
     13))

(defun player-adultp (&optional (player *user))
  (>= (or (legal-age (db.person-date-of-birth player))
	  (db.person-age player))
     18))

(defun toot-childp (toot)
  (player-childp (find-reference toot :player)))

(defun toot-info (toot)
  (list :name (db.toot-name toot)
	:note "" ; TODO Toot notes by player/parent
	:avatar (db.avatar-name (find-reference toot :avatar))
	:base-color (color24-name (db.toot-base-color toot))
	:pattern (string-downcase (db.toot-pattern toot))
	:pattern-color (color24-name (db.toot-pattern-color toot))
	:pads-color (color24-name (db.toot-pads-color toot))
	:child-p (toot-childp toot)
	:sensitive-p (or (toot-childp toot)
			 (db.person-sensitivep (find-reference toot :player)))
	:last-seen (db.toot-last-active toot)))

(defun player-toots (&optional (player *user*))
  (find-records 'db.toot "player" (db.person-uuid player)))
  
(defun player-fake-toots (&optional (player *user*))
  (declare (ignore player))
  (list
   (list :name "Zap"
         :note "These are still fake Toots for testing"
         :avatar "UltraToot"
         :base-color "violet"
         :pattern "lightning"
         :pattern-color "yellow"
         :highlight-color "yellow"
         :child-p nil
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil (3-days-ago)))
   (list :name "Flora"
         :note "This an an example of a child's Toot
appearing on a parent's account."
         :avatar "UltraToot"
         :base-color "pink"
         :pattern "flowers"
         :pattern-color "white"
         :highlight-color "yellow"
         :child-p t
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil (2-days-ago)))
   (list :name "Moo"
         :note ""
         :avatar "UltraToot"
         :base-color "white"
         :pattern "moo"
         :pattern-color "black"
         :highlight-color "black"
         :child-p nil
         :sensitive-p nil
         :last-seen (local-time:format-timestring
                     nil
                     (yesterday)))))





(defun find-player-or-die ()
  "Ensure that a recognized player is connected."
  (find-user-from-session :if-not-exists :error))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/login/\"}"))

(defmacro with-player (() &body body)
  "Ensure that a recognized player is connected
using `FIND-PLAYER-OR-DIE' and bind *USER*"
  `(multiple-value-bind  (foundp *user*) (find-player-or-die)
     (cond (foundp
            ,@body)
           (t (throw 'endpoint (list 403 nil *403.json-bytes*))))))



(defun assert-my-character (toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type toot-name toot-name)
  (unless (find toot-name
                (mapcar (rcurry #'getf :name) (player-toots user))
                :test #'string-equal)
    (error 'not-your-toot-error :name toot-name)))



;;; TODO ensure that these time functions exist somewhere more
;;; appropriate and remove then

(defun days-ago (days)
  (local-time:timestamp- (local-time:now) days :day))

(defun yesterday ()
  (days-ago 1))

(defun 2-days-ago ()
  (days-ago 2))

(defun 3-days-ago ()
  (days-ago 3))

(defun header-time (&optional (time (get-universal-time)))
  (local-time:format-rfc1123-timestring
   nil
   (etypecase time
     (number (local-time:universal-to-timestamp time))
     (local-time:timestamp time))))
