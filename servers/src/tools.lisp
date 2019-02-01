;;;; -*- lisp -*-
;;;
;;;; ./servers/src/tools.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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



(defmacro with-user-brp (() &body body)
  `(let ((*user* (find-record 'person :uuid
                              (uuid:make-uuid-from-string
                               "480B0917-3C7A-4D13-B55B-AA56105C5E00"))))
     (with-user ()
       ,@body)))



(defun check-first-line/reservations (file)
  (let ((first-line (split-sequence #\Tab (read-line file))))
    (assert (equalp first-line
                    '("Timestamp"	"Toot Name"
                      "Pick your Toot's base (skin) color"
                      "Pick your Toot's foot pads (palms) color"
                      "Pick your Toot's pattern color"
                      "Pick your Toot's Pattern. (Just the pattern, you already picked the color)"
                      "Which age range are you?"
                      "Your Google account's e-mail address"
                      "Your child's name or nickname. (Used only for labelling Toots in your collection.)"
                      "Your Google account's e-mail address")))))

(define-memo-function remove-parentheticals (string)
  (let ((s (copy-seq string)))
    (when (find #\( string)
      (setf s (subseq string 0 (position #\( string))))
    (when (find #\/ s)
      (setf s (subseq s 0 (position #\/ s))))
    (setf s (string-trim +whitespace+ s))
    (when (not (blank-string-p s))
      s)))

(defun parse-line/reservations (line)
  (destructuring-bind (created-at$ Toot-name
                                   base-color$ pads-color$ pattern-color$
                                   pattern-name$
                                   age-range
                                   gmail1
                                   child-name gmail2 &rest _)
      (mapcar (curry #'string-trim +whitespace+) (split-sequence #\Tab line))
    (declare (ignore _))
    (let* ((created-at (translate-american-ish-date created-at$))
           (base-color (or (remove-parentheticals base-color$)
                           (random-elt +Toot-base-color-names+)))
           (pads-color (or (remove-parentheticals pads-color$)
                           (random-elt +Toot-pad-color-names+)))
           (pattern-color (or (remove-parentheticals pattern-color$)
                              (random-elt (allowed-pattern-colors-on-base base-color))))
           (pattern-name (or (remove-parentheticals pattern-name$)
                             (random-elt +Toot-basic-pattern-names+)))
           (own-toot-p (or (blank-string-p age-range)
                           (search "13" age-range)))
           (gmail (if own-toot-p gmail2 gmail1)))
      (check-Toot-name Toot-name)
      (check-type pads-color Toot-pad-color-name)
      (check-type pattern-name Toot-pattern-name)
      (check-pattern-on-base-color pattern-color base-color
                                   :Toot-name Toot-name
                                   :pad-color pads-color
                                   :pattern pattern-name
                                   :address gmail)
      (list :created-at created-at
            :Toot-name Toot-name
            :base-color base-color
            :pads-color pads-color
            :pattern-color pattern-color
            :pattern-name pattern-name
            :own-toot-p own-toot-p
            :child-name (unless own-toot-p child-name)
            :gmail gmail))))

(defun import-toot-to-db (record)
  (let ((toot (find-record 'toot
                           :name (getf record :toot-name)))
        (email (ensure-record 'person-link
                              :rel :contact
                              :url (format nil "mailto:~a" (getf record :gmail))
                              :provenance "Tootsville V Pre-Registration")))
    (unless (ignore-errors (find-reference email :person))
      (format t "~& set owner toot ~a for ~a"
              (getf record :toot-name) (getf record :gmail))
      (let ((person (ensure-record 'person :given_name (getf record :gmail)
                                   :display_name (getf record :gmail)
                                   :surname ""
                                   :gender :X
                                   :language "en_US")))
        (setf (toot-player toot) (person-uuid person)
              (person-link-person email) (person-uuid person))
        (save-record toot))))
 ;;;(error (c) nil)

  (return-from import-toot-to-db)

  (unless (getf record :own-toot-p)
    (return-from import-toot-to-db))    ; TODO
  (let ((person (ensure-user-for-plist
                 (list :email (getf record :gmail)
                       :email-verified-p t))))
    (ignore-duplicates
      (ensure-record 'person-link
                     :person (person-uuid person)
                     :rel :CONTACT
                     :url (concatenate 'string "mailto:" (getf record :gmail))
                     :provenance "Toot Name Reservations GMail"))
    (unless (getf record :own-toot-p)
      (setf (person-child-code person) "*"
            (person-display-name person) (getf record :child-name)
            (person-given-name person) (getf record :child-name))
      ;; TODO - link to parent
      )
    (let ((toot (ensure-record
                 'toot
                 :name (getf record :toot-name)
                 :pattern (pattern-id (find-record 'pattern
                                                   :name (getf record :pattern-name)))
                 :base-color (parse-color24 (string (getf record :base-color)))
                 :pad-color (parse-color24 (string (getf record :pads-color)))
                 :pattern-color (parse-color24 (string (getf record :pattern-color)))
                 :avatar 1
                 :player (person-uuid person)
                 :last-active (or (getf record :created-at)
                                  (parse-timestring "2014-10-13T09:37:20"))
                 :note "Toot Name Pre-Registered"))
          (gifts (list (ensure-record 'item :template 1)
                       (ensure-record 'item :template 2)
                       (ensure-record 'item :template 3))))
      (dolist (gift gifts)
        (ignore-errors (ensure-record 'inventory-item
                                      :base-color (parse-color24 "periwinkle")
                                      :person (person-uuid person)
                                      :toot (toot-uuid toot)
                                      :item (item-uuid gift)
                                      :equipped "N"))))))

;; (make-record   'toot  :name   "Shade"   :pattern  13   :base-color
;; (make-color24  :red  #x90  :green  #x20  :blue  #x90)  :pattern-color
;; (make-color24   :red  #xff   :green  #xff   :blue  #x00)   :pad-color
;; (make-color24   :red   #xff   :green   #xff   :blue   #x00)   :avatar
;; 8     :last-active     (parse-timestring    "2013-01-01")     :player
;; (person-uuid ☠brp) :note "")

(defun import-toot-name-reservations
    (&optional (file (merge-pathnames (user-homedir-pathname)
                                      (make-pathname :name "Toots-Name-Reservations"))))
  (with-input-from-file (file file)
    (check-first-line/reservations file)
    (with-dbi (:friendly)
      (tagbody reading
         (let ((line (read-line file nil nil)))
           (unless line (go done))
           (when (blank-string-p line)
             (go reading))
           (restart-case
               (progn
                 (import-toot-to-db (parse-line/reservations line))
                 (format t "~&~:(~25a~)  for ~:[child of ~;~]~a"
                         (getf (parse-line/reservations line) :toot-name)
                         (getf (parse-line/reservations line) :own-toot-p)
                         (getf (parse-line/reservations line) :gmail)))
             (continue ()
               :report (lambda (s)
                         (princ "Skip this record and continue with the next" s))
               (format *error-output* "~&Skipping this record due to error:~%~a" line)
               (go reading)))
           (go reading))
       done))))
