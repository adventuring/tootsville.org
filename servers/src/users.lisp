(in-package :Tootsville)



(defvar *user* nil
  "The currently-signed-in user, if any")

(defclass user ()
  ((display-name :accessor user-display-name
                 :initarg :display-name
                 :type string)
   (given-name :accessor user-given-name
               :initarg :given-name
               :type string)
   (middle-name :accessor user-middle-name
                :initarg :middle-name
                :type (or string null))
   (surname :accessor user-surname
            :initarg :surname
            :type (or string null))
   (face :accessor user-face
         :initarg :face
         :type (or puri:uri null))
   (uuid :accessor user-id
         :initarg :uuid
         :initform (uuid:make-v4-uuid)
         :type uuid:uuid)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|middleName|  (user-middle-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))

(defclass credentials ()
  ((user :type user
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

(defclass toot ()
  ((name :accessor toot-name 
         :initarg :name
         :type toot-name)
   (note :accessor toot-note
         :initarg :note
         :type string)
   (avatar :accessor toot-avatar
           :initarg :avatar
           :initform "UltraToot"
           :type string)
   (base-color :accessor toot-base-color
               :initarg :base-color
               :type toot-base-color-name)
   (pattern :accessor toot-pattern
            :initarg :pattern
            :type toot-pattern-name)
   (pattern-color :accessor toot-pattern-color
                  :initarg :pattern-color
                  :type toot-pattern-color-name)
   (pads-color :accessor toot-pads-color
               :initarg :pads-color
               :type toot-pads-color-name)
   (owner-id :accessor toot-owner-id
             :initarg :owner-id
             :type uuid-string)
   (childp :accessor toot-child-p
           :initarg :childp
           :type (member t nil))
   (sensitivep :accessor toot-sensitive-p
               :initarg :sensitivep
               :type (member t nil))
   (onlinep :accessor toot-online-p
            :initarg :onlinep
            :type (member t nil))
   (created :reader toot-created
            :initform (now)
            :type timestamp)
   (last-seen :accessor toot-last-seen
              :initarg :last-seen
              :initform (now)
              :type timestamp)))



;;; Toot character data.

(defun find-toot-by-name (toot-name)
  (check-type toot-name toot-name)
  (remove-if-not (lambda (toot)
                   (string-equal (getf toot :name) toot-name))
                 (player-toots)))

(defun toot-info (toot)
  (append toot (list :is-a "toot")))

(defun player-toots (&optional (player *user*))
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
  `(multiple-value-bind  (foundp *user*)
       (find-player-or-die)
     (cond (foundp
            ,@body)
           (t (return-from endpoint
                (list 403 nil *403.json-bytes*))))))



(defun assert-my-character (toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type toot-name toot-name)
  (unless (find toot-name
                (mapcar (rcurry #'getf :name) (player-toots user))
                :test #'string-equal)
    (error 'not-your-toot-error :name toot-name)))

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


