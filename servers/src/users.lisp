(in-package :Tootsville)



(defvar *user* nil
  "The currently-signed-in user, if any")

(defun email-lhs (address)
  (when address
    (subseq address 0 (position #\@ address))))

(defun ensure-user-for-plist (plist)
  "Find or create the user described by PLIST and return them.

PLIST  can  have  keys  that  align to  a  DB.PERSON  or  their  contact
infos (eg,  email) and is expected  to have been validated  already (eg,
come from a trusted authentication provider like Google Firebase)."
  (let ((by-mail (when-let (email (and (getf plist :email-verified-p)
                                       (getf plist :email)))
                   (when-let (link (find-record 'db.person-link
                                                "url"
                                                (concatenate 'string
                                                             "mailto:" email)))
                     (find-record 'db.person "uuid" (db.person-link-person link)))))
        (by-uid (when-let (cred (find-record 'db.credential
                                             "uid" (getf plist :uid)
                                             "provider" (getf plist :provider)))
                  (find-record 'db.person "uuid"
                               (db.credential-person cred)))))
    (cond
      ((and by-mail by-uid (uuid:uuid= (db.person-uuid by-mail)
                                       (db.person-uuid by-uid)))
       )
      ((and by-mail by-uid)
       )
      (by-mail)
      (by-uid)
      (t (let ((person (make-record
                        'db.person
                        :display-name (or (getf plist :name)
                                          (email-lhs (getf plist :email)))
                        :given-name (or (getf plist :given-name)
                                        (getf plist :name)
                                        (email-lhs (getf plist :email)))
                        :surname (getf plist :surname))))
           (make-record 'db.credential "person" (db.person-uuid person)
                        "provider" (getf plist :provider)
                        "uid" (getf plist :uid))
           (when (getf plist :email-verified-p)
             (make-record 'db.person-link
                          "person" (db.person-uuid person)
                          "rel" "CONTACT"
                          "url" (concatenate 'string "mailto:"
                                             (getf plist :email))))
           (when (getf plist :picture)
             (make-record 'db.person-link
                          "person" (db.person-uuid person)
                          "rel" "PORTRAIT"
                          "url" (getf plist :picture)))
           person)))))



;;; User details

(defun user-display-name (&optional (person *user*))
  (db.person-display-name (ensure-person person)))

(defun user-given-name (&optional (person *user*))
  (db.person-given-name (ensure-person person)))

(defun user-surname (&optional (person *user*))
  (db.person-surname (ensure-person person)))

(defun user-email (&optional (person *user*))
  "Finds an email address for PERSON of type CONTACT."
  (when-let (mails (remove-if-not
                    (lambda (record)
                      (string-begins "mailto:" (db.person-link-url record)))
                    (find-records 'db.person-link
                                  "person" (db.person-uuid
                                            (ensure-person person))
                                  "rel" "CONTACT")))
    (subseq (random-elt mails) 7)))

(defun user-face (&optional (person *user*))
  "Finds a portrait URI for PERSON"
  (when-let (portraits (find-records 'db.person-link
                                     "person" (db.person-uuid
                                               (ensure-person person))
                                     "rel" "PORTRAIT"))
    (random-elt portraits)))

(defun user-id (&optional (person *user*))
  (db.person-uuid (ensure-person person)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))



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
