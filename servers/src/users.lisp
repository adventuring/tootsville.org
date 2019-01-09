;;;; -*- lisp -*-
;;;
;;;; ./servers/src/users.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

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
          (ensure-record 'credential
                         :person (person-uuid person)
                         :id-token id
                         :uid id
                         :provider provider))))

(defun find-person-by-url (url &optional more)
  (when-let (link (handler-case
                      (find-record 'person-link
                                   :url (reduce (curry #'concatenate 'string)
                                                (list url more)))
                    (not-found (c)
                      (declare (ignore c))
                      nil)))
    (find-record 'person :uuid (person-link-person link))))

(defun person-links-to-email (email)
  (find-records 'person-link
                :url
                (concatenate 'string
                             "mailto:" email)))

(defun all-links-to-same-person-p (links)
  (let ((first (person-link-person (first links))))
    (every (lambda (link)
             (uuid:uuid= (person-link-person link) first))
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
              'person
              :display-name (or (getf plist :name)
                                (email-lhs (getf plist :email)))
              :given-name (or (getf plist :given-name)
                              (getf plist :name)
                              (email-lhs (getf plist :email)))
              :surname (or (getf plist :surname) "")
              :gender :X
              :lang "en_US"))))
    (ensure-record 'person-link
                   :person (person-uuid person)
                   :rel :contact
                   :url (concatenate 'string "mailto:"
                                     (getf plist :email))
                   :label "email"
                   :provenance "Provided by Firebase login")
    (associate-credentials person (getf plist :credentials))
    (when-let (picture (getf plist :picture))
      (ensure-record 'person-link
                     :person (person-uuid person)
                     :rel :photo
                     :url picture
                     :label "portrait"
                     :provenance "Provided by Firebase login"))
    (when-let (email (and (getf plist :email-verified-p)
                          (getf plist :email)))
      (update-gravatar person email))
    person))

(defun update-gravatar (person email)
  (if-let ((gravatar (ignore-not-found
                       (find-record 'person-link
                                    :person (person-uuid person)
                                    :rel :photo
                                    :provenance "Provided by Gravatar"))))
    (setf (person-link-url gravatar)
          (gravatar-image-url email
                              :size 256
                              :rating :pg
                              :default :identicon))
    (make-record 'person-link
                 :person (person-uuid person)
                 :rel :photo
                 :label "Gravatar"
                 :provenance "Provided by Gravatar"
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
  (person-display-name (ensure-person person)))

(defun user-given-name (&optional (person *user*))
  (person-given-name (ensure-person person)))

(defun user-surname (&optional (person *user*))
  (person-surname (ensure-person person)))

(defun user-email (&optional (person *user*))
  "Finds an email address for PERSON of type CONTACT."
  (when-let (mails (remove-if-not
                    (lambda (record)
                      (string-begins "mailto:" (person-link-url record)))
                    (find-records 'person-link
                                  :person (person-uuid
                                           (ensure-person person))
                                  :rel :CONTACT)))
    (subseq (random-elt mails) 7)))

(defun user-face (&optional (person *user*))
  "Finds a portrait URI for PERSON"
  (when-let (portraits (find-records 'person-link
                                     :person (person-uuid
                                              (ensure-person person))
                                     :rel :photo))
    (random-elt portraits)))

(defun user-id (&optional (person *user*))
  (person-uuid (ensure-person person)))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))


(defun player-childp (&optional (player *user*))
  (< (or (when-let (dob (person-date-of-birth player)) (legal-age dob))
         (person-age player)
         (person-child-code player)
         1)
     13))

(defun player-adultp (&optional (player *user*))
  (>= (or (legal-age (person-date-of-birth player))
          (person-age player))
      18))

(defun player-Toots (&optional (player *user*))
  (find-records 'Toot :player (person-uuid player)))



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
  (unless (find-record 'toot
                       :player (person-uuid user)
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
                             (drakma::alist-to-url-encoded-string
                              parameters
                              :utf-8 #'drakma:url-encode))
                     +gravatar-base-uri+)))


(defun person-is-patron-p (person)
  ;; just me ☹
  (eql (uuid-to-uri (person-uuid person)) "SAsJFzx6TRO1W6pWEFxeAA=="))

(defun get-rollbar-person (person)
  (when *user*
    (list :|person|
          (list :|uid| (princ-to-string (person-uuid person))
                :|username| (format nil "~@[Toot: ~a, ~]Person: ~a"
                                    (when *Toot* (Toot-name *Toot*))
                                    (person-display-name *user*))
                :|email| (user-email)))))
