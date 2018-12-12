(in-package :Tootsville)

(defun legal-age (date-of-birth &optional (reference-date (local-time:now)))
  "The age of  a person born on DATE-OF-BIRTH, as  of REFERENCE-DATE (or
right  now);  this uses  the  legal  definition  that the  person's  age
increments at  the midnight of their  date of birth each  year, with the
date 29 February treated as 1 March on non-leap-years.

The time  zone used for  this computation  is the not  defined, however,
yielding  rather  irregular  behaviour   depending  on  time  zones  and
the like.

TODO: Determine  in what  time zone  we should compute this  for legal
reasons, eg, COPPA."
  (check-type date-of-birth timestamp)
  (check-type reference-date timestamp)
  (unless (timestamp< date-of-birth reference-date)
    (return-from legal-age 0))
  (multiple-value-bind (msec sec min hour day month year)
      (decode-timestamp reference-date)
    (declare (ignore msec sec min hour))
    (multiple-value-bind (msec sec min hour
                               day-of-birth month-of-birth year-of-birth)
        (decode-timestamp date-of-birth)
      (declare (ignore msec sec min hour))
      (let ((had-birthday-p (or (< month-of-birth month)
                                (and (= month-of-birth month)
                                     (<= day-of-birth day)))))
        (+ (- year
              year-of-birth
              1)
           (if had-birthday-p 1 0))))))
