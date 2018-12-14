(in-package :Tootsville)



(defvar *user* nil
  "The currently-signed-in user, if any")

(defun email-lhs (address)
  (when address
    (subseq address 0 (position #\@ address))))

(defmacro ignore-duplicates (&body body)
  `(restart-case 
       (handler-bind
           ((dbi.error:<dbi-database-error> 
             (lambda (c)
               (when (= 1062 (slot-value c 'DBI.ERROR::ERROR-CODE))
                 (invoke-restart 'continue)))))
         ,@body)
     (continue () nil)))

(defun associate-credentials (person credentials)
  (loop for (provider ids) on credentials by #'cddr
     do (dolist (id ids)
          (ensure-record 'db.credential
                         :person (db.person-uuid person)
                         :id-token id
                         :uid id 
                         :provider provider))))

(defun person-links-to-email (email)
  (find-records 'db.person-link
                :url
                (concatenate 'string
                             "mailto:" email)))

(defun all-links-to-same-person-p (links)
  (let ((first (db.person-link-person (first links))))
    (every (lambda (link) 
             (uuid:uuid= (db.person-link-person link) first))
           (rest links))))

(defun ensure-user-for-plist (plist)
  "Find or create the user described by PLIST and return them.

PLIST  can  have  keys  that  align to  a  DB.PERSON  or  their  contact
infos (eg,  email) and is expected  to have been validated  already (eg,
come from a trusted authentication provider like Google Firebase)."
  (let ((person 
         (or (when-let (email (and (getf plist :email-verified-p)
                                   (getf plist :email)))
               (when-let (links (person-links-to-email email))
                 (when-let (link (when (all-links-to-same-person-p links)
                                   (first links)))
                   (find-reference link :person))))
             (make-record
              'db.person
              :display-name (or (getf plist :name)
                                (email-lhs (getf plist :email)))
              :given-name (or (getf plist :given-name)
                              (getf plist :name)
                              (email-lhs (getf plist :email)))
              :surname (getf plist :surname)))))
    (ensure-record 'db.person-link
                   :person (db.person-uuid person)
                   :rel :contact
                   :url (concatenate 'string "mailto:"
                                     (getf plist :email))
                   :label "Provided by Firebase login")
    (associate-credentials person (getf plist :credentials)) 
    (when-let (picture (getf plist :picture))
      (ensure-record 'db.person-link
                     :person (db.person-uuid person)
                     :rel :portrait
                     :url picture
                     :label "Provided by Firebase login"))
    (when-let (email (and (getf plist :email-verified-p)
                          (getf plist :email)))
      (update-gravatar person email))
    person))

(defun update-gravatar (person email)
  (if-let ((gravatar (ignore-not-found
                       (find-record 'db.person-link
                                    :person (db.person-uuid person)
                                    :rel :portrait
                                    :label "Provided by Gravatar"))))
    (setf (db.person-link-url gravatar)
          (gravatar-image-url email
                              :size 256
                              :rating :pg
                              :default :identicon))
    (make-record 'db.person-link
                 :person (db.person-uuid person)
                 :rel :portrait
                 :label "Provided by Gravatar"
                 :url (gravatar-image-url email
                                          :size 256
                                          :rating :pg
                                          :default :identicon))))



(defmacro with-user (() &body body)
  `(progn (unless *user*
            (error 'unidentified-player-error))
          ,@body))



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
                                  :person (db.person-uuid
                                           (ensure-person person))
                                  :rel :CONTACT)))
    (subseq (random-elt mails) 7)))

(defun user-face (&optional (person *user*))
  "Finds a portrait URI for PERSON"
  (when-let (portraits (find-records 'db.person-link
                                     :person (db.person-uuid
                                              (ensure-person person))
                                     :rel :PORTRAIT))
    (random-elt portraits)))

(defun user-id (&optional (person *user*))
  (db.person-uuid (ensure-person person)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))


(defun player-childp (&optional (player *user*))
  (< (or (when-let (dob (db.person-date-of-birth player)) (legal-age dob))
         (db.person-age player)
         (db.person-child-code player)
         1)
     13))

(defun player-adultp (&optional (player *user*))
  (>= (or (legal-age (db.person-date-of-birth player))
          (db.person-age player))
      18))

(defun player-Toots (&optional (player *user*))
  (find-records 'db.Toot :player (db.person-uuid player)))



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
  (unless (find-record 'db.toot 
                       :player (db.player-uuid user)
                       :name Toot-name)
    (error 'not-your-Toot-error :name Toot-name)))



;;; Copied from CL-Gravatar, but fixed for my version of Drakma (newer?)

;;; TODO: post patch upstream

#| CL-Gravatar

Copyright 2011 Greg Pfeil <greg@technomadic.org>

Licensed under the Apache License,  Version 2.0 (the "License"); you may
not use this file except in  compliance with the License. You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless  required by  applicable law  or agreed  to in  writing, software
distributed  under the  License  is  distributed on  an  "AS IS"  BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See  the License  for the  specific language  governing permissions  and
limitations under the License. |#

(define-constant +gravatar-base-uri+ (puri:uri "https://secure.gravatar.com/")
  :test #'puri:uri=
  :documentation "Why would we ever _not_ use SSL?")

(defun gravatar-hash (email)
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-sequence
                                    (string-downcase (string-trim '(#\space)
                                                                  email)))
                                   'list))))

(defun gravatar-image-url (email &key size default force-default-p rating)
  "DEFAULT may be either a URL to your own image, or one of :404, :mm,
:identicon, :monsterid, :wavatar, or :retro. RATING may be one of :g, :pg,
:r, or :x."
  (let ((parameters ()))
    (when size (push `("s" . ,(format nil "~d" size)) parameters))
    (typecase default
      (keyword (push `("d" . ,(string-downcase default)) parameters))
      (string (push `("d" . ,default) parameters)))
    (when force-default-p (push '("f" . "y") parameters))
    (when rating (push `("r" . ,(string-downcase rating)) parameters))
    (puri:merge-uris (format nil "avatar/~a~@[?~a~]"
                             (gravatar-hash email)
                             (drakma::alist-to-url-encoded-string parameters
                                                                  :utf-8 #'drakma:url-encode))
                     +gravatar-base-uri+)))


