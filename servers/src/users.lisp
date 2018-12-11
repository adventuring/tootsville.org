(in-package :Tootsville)



(defvar *user* nil
  "The currently-signed-in user, if any")

(defun user-display-name (&optional (person *user*))
  (db.person-display-name (ensure-person person)))

(defun user-given-name (&optional (person *user*))
  (db.person-given-name (ensure-person person)))

(defun user-surname (&optional (person *user*))
  (db.person-surname (ensure-person person)))

(defun user-face (&optional (person *user*))
  (let* ((uuid (db.person-uuid (ensure-person person)))
         (portraits (find-records 'db.person-link
                                  "person" uuid "rel" :portrait)))
    (when portraits (random-elt portraits))))

(defun user-id (&optional (person *user*))
  (db.person-uuid (ensure-person person)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))

#+ (or)
(defclass credentials ()
  ((user :type db.person
         :initarg :user
         :reader credentials-user)))
#+ (or)
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
#+ (or)
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

(defun find-Toot-by-name (Toot-name)
  (check-type Toot-name Toot-name)
  (find-record 'db.Toot "name" Toot-name))

(defun player-childp (&optional (player *user))
  (< (or (legal-age (db.person-date-of-birth player))
         (db.person-age player))
     13))

(defun player-adultp (&optional (player *user))
  (>= (or (legal-age (db.person-date-of-birth player))
	(db.person-age player))
      18))

(defun Toot-childp (Toot)
  (player-childp (find-reference Toot :player)))

(defun Toot-info (Toot)
  (list :name (db.Toot-name Toot)
        :note (db.Toot-note Toot)
        :avatar (db.avatar-name (find-reference Toot :avatar))
        :base-color (color24-name (db.Toot-base-color Toot))
        :pattern (string-downcase (db.Toot-pattern Toot))
        :pattern-color (color24-name (db.Toot-pattern-color Toot))
        :pads-color (color24-name (db.Toot-pads-color Toot))
        :child-p (Toot-childp Toot)
        :sensitive-p (or (Toot-childp Toot)
		     (db.person-sensitivep (find-reference Toot :player)))
        :last-seen (db.Toot-last-active Toot)))

(defun player-Toots (&optional (player *user*))
  (find-records 'db.Toot "player" (db.person-uuid player)))

(defun player-fake-Toots (&optional (player *user*))
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
  (unless *user* (error 'unidentified-player-error)))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/login/\"}"))

(defmacro with-player (() &body body) ; TODO: cv WITH-USER
  "Ensure that a recognized player is connected
using `FIND-PLAYER-OR-DIE' and bind *USER*"
  `(progn (find-player-or-die)
          ,@body))



(defun assert-my-character (Toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type Toot-name Toot-name)
  (unless (find Toot-name
                (mapcar (rcurry #'getf :name) (player-Toots user))
                :test #'string-equal)
    (error 'not-your-Toot-error :name Toot-name)))



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
