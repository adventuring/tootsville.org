(in-package :Tootsville)



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
                                   child-name gmail2)
      (mapcar (curry #'string-trim +whitespace+) (split-sequence #\Tab line))
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

(defun import-toot-name-reservations (&optional (file (merge-pathnames (user-homedir-pathname)
                                                                       (make-pathname :name "Toots-Name-Reservations"))))
  (with-input-from-file (file file)
    (check-first-line/reservations file)
    (let ((roster nil))
      (tagbody reading
         (let ((line (read-line file nil nil)))
           (unless line (go done))
           (when (blank-string-p line)
             (go reading))
           (restart-case
               (progn
                 (push (parse-line/reservations line) roster)
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
       done)
      roster)))
