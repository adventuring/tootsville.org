(in-package :Tootsville)



(defmacro with-user-brp (() &body body)
  `(let ((*user* (find-record 'db.person :uuid (uuid:make-uuid-from-string "480B0917-3C7A-4D13-B55B-AA56105C5E00")))) 
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
                           (random-elt +Toot-pads-color-names+)))
           (pattern-color (or (remove-parentheticals pattern-color$)
                              (random-elt (allowed-pattern-colors-on-base base-color))))
           (pattern-name (or (remove-parentheticals pattern-name$)
                             (random-elt +Toot-basic-pattern-names+)))
           (own-toot-p (or (blank-string-p age-range)
                           (search "13" age-range)))
           (gmail (if own-toot-p gmail2 gmail1)))
      (check-Toot-name Toot-name)
      (check-type pads-color Toot-pads-color-name)
      (check-type pattern-name Toot-pattern-name)
      (check-pattern-on-base-color pattern-color base-color
                                   :Toot-name Toot-name
                                   :pads-color pads-color
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
  (unless (getf record :own-toot-p)
    (return-from import-toot-to-db))    ; TODO
  (let ((person (make-record 'db.person :child-code nil
                             :display-name (getf record :gmail)
                             :given-name (subseq (getf record :gmail)
                                                 0
                                                 (position #\@ (getf record :gmail)))
                             :surname ""
                             :gender "X"
                             :lang "en")))
    (make-record 'db.person-link
                 :person (db.person-uuid person)
                 :rel :CONTACT
                 :url (concatenate 'string "mailto:" (getf record :gmail))
                 :label "Toot Name Reservations GMail")
    (unless (getf record :own-toot-p)
      (setf (db.person-child-code person) "*"
            (db.person-display-name person) (getf record :child-name)
            (db.person-given-name person) (getf record :child-name))
      ;; TODO
      )
    (let ((toot (make-record
                 'db.toot
                 :name (getf record :toot-name)
                 :pattern (db.pattern-id (find-record 'db.pattern "name" 
                                                      (getf record :pattern-name)))
                 :base-color (parse-color24 (string (getf record :base-color)))
                 :pad-color (parse-color24 (string (getf record :pads-color)))
                 :pattern-color (parse-color24 (string (getf record :pattern-color)))
                 :avatar 1
                 :player (db.person-uuid person)
                 :last-active (or (getf record :created-at) (parse-timestring "2013-01-01T00:00:00"))
                 :note "Toot Name Pre-Registered"))
          (gifts (list (make-record 'db.item :template 1)
                       (make-record 'db.item :template 2)
                       (make-record 'db.item :template 3))))
      (dolist (gift gifts)
        (make-record 'db.inventory-item
                     :person (db.person-uuid person)
                     :toot (db.toot-uuid toot)
                     :item (db.item-uuid gift)
                     :equipped "N")))))

;; (make-record   'db.toot  :name   "Shade"   :pattern  13   :base-color
;; (make-color24  :red  #x90  :green  #x20  :blue  #x90)  :pattern-color
;; (make-color24   :red  #xff   :green  #xff   :blue  #x00)   :pad-color
;; (make-color24   :red   #xff   :green   #xff   :blue   #x00)   :avatar
;; 8     :last-active     (parse-timestring    "2013-01-01")     :player
;; (db.person-uuid ☠brp) :note "")

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
