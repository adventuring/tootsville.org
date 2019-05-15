(defpackage Chœrogryllum
  (:nicknames Choerogryllum)
  (:use :cl :oliphaunt)
  (:export
   #:decode*-universal-time
   #:encode*-universal-time
   #:day-of-week*
   #:holiday-on
   #:date-string
   #:month*
   #:cal-month
   #:cal-year))

(in-package :Chœrogryllum)



(defun holiday-on (year month day)
  (multiple-value-bind
        (_sec _min _hour _day _month _year
              weekday other-month-day pink-month-day)
      (decode*-universal-time (encode*-universal-time 0 0 9 day month year))
    (declare (ignore _sec _min _hour _day _month _year))
    (cond
      ((and (= 15 day)
            (= 36 other-month-day)
            (= 26 pink-month-day)) "Trimestus")
      ((and (= 30 day) (= 10 month)) "Hallowe'en")
      ((and (= 1 day) (= 11 month)) "Hallowsday")
      ((and (or (= 3 month) (= 4 month) (= 5 month))
            (= 0 weekday) (< other-month-day 10)) "Easter")
      ((and (= 25 day) (= 12 month)) "Christmas")
      ((and (= 24 day) (= 12 month)) "Christmas Eve")
      ((and (= 13 day) (= 5 month)) "Hugaboo")
      ((and (= day 21) (= 1 month)) "Winter Solstice")
      ((and (= day 21) (= 4 month)) "Spring Solstice")
      ((and (= day 21) (= 7 month)) "Summer Solstice")
      ((and (= day 21) (= 10 month)) "Autumn Equinox")
      ((and (= 5 day) (= 11 month)) "Fawkesday")
      ((= 1 day month) "New Year's Day")
      ((and (= 30 day) (= 12 month)) "New Year's Eve")
      ((and (<= 17 day 20)
            (= 7 month) (/= 8 weekday)) "Summer Arts Festival")
      ((and (= 36 other-month-day)
            (= 26 pink-month-day)) "Duomestus")
      ((= 36 other-month-day) "Full Other Moon")
      ((= 26 pink-month-day) "Full Pink Moon")
      ((= 15 day) "Full Moon")
      (t nil))))

(defun exponent-digit (number)
  (check-type number (integer 0 10))
  (coerce (list (elt "⁰¹²³⁴⁵⁶⁷⁸⁹†" number)) 'string))

(defun cal-month/print-holiday-footnotes (year month holidays stream)
  (when holidays
    (loop for holiday in (nreverse holidays)
       for i from 1
       do (format stream "~% *~a: ~a on ~a" (exponent-digit i)
                  (holiday-on year month holiday)
                  (date-string (encode*-universal-time 0 0 9 holiday month year))))
    (terpri stream)))

(defun first-weekday-of-month (year month)
  (nth-value 6 (decode*-universal-time (encode*-universal-time 0 0 9 1 month year))))

(defun cal-month-header (year month stream)
  (declare (ignore year))
  (format stream "~45@<~:@r. ~a  ~;---------------------------------------------~>

~{~a.~^ ~}~%"
          month (month* month)
          (mapcar (rcurry #'day-of-week* :form :abbrev) (range 0 8))))

(defun cal-month (year month)
  "Pretty-prints a one-month mini-calendar."
  (let ((first-weekday-of-month (first-weekday-of-month year month)))
    (with-output-to-string (s)
      (cal-month-header year month s)
      (loop for pad-day below first-weekday-of-month
         do (princ "     " s))
      (let (holidays)
        (loop for day from 1 upto 30
           for holiday = (holiday-on year month day)
           do (format s " ~2d~2a" day (if holiday
                                          (progn
                                            (push day holidays)
                                            (concatenate 'string "*" (exponent-digit (length holidays))))
                                          "  "))
           when (zerop (mod (+ day first-weekday-of-month) 9))
           do (terpri s))
        (terpri s)
        (cal-month/print-holiday-footnotes year month holidays s)
        (princ "---------------------------------------------" s)
        (terpri s)))))

(defun cal-year (year)
  (with-output-to-string (s)
    (dotimes (month 12)
      (princ (cal-month year (1+ month)) s))))

(defun date-string (time &key (form :long))
  "Returns the pretty-printed Chœrogryllum date string describing Universal time TIME."
  (multiple-value-bind  (sec min hour day month year weekday)
      (decode*-universal-time time)
    (declare (ignore hour min sec))
    (ecase form
      (:long (format nil "~a, the ~:r of ~a, ~d"
                     (day-of-week* weekday) day (month* month) year))
      (:abbrev (format nil "~a ~2,'0d ~5a ~d"
                       (day-of-week* weekday :form :abbrev) day (month* month :form :abbrev) year)))))

(defun encode*-universal-time (sec min hour day month year)
  "Encodes a Chœrogryllum date & time into a Universal Time."
  (check-type year (integer 0 *))
  (check-type month (integer 1 12))
  (check-type day (integer 1 30))
  (check-type hour (integer 0 17))
  (check-type min (integer 0 59))
  (check-type sec (integer 0 59))
  (round (+
          (* (+ year 10) 60 60 18 360)
          (* (1- month) 60 60 18 30)
          (* (1- day) 60 60 18)
          (* hour 60 60)
          (* min 60)
          sec)))

(defun decode*-universal-time (&optional (time (get-universal-time)))
  "Returns multiple values with date and time decoded.

Returns:
\(sec min hour day month year weekday other-month-day pink-month-day julian)
"
  (let* ((year (- (floor time (* 60 60 18 360)) 10))
         (month (1+ (mod (floor time (* 60 60 18 30)) 12)))
         (day (1+ (mod (floor time (* 60 60 18)) 30) ))
         (hour (mod (floor time (* 60 60)) 18))
         (min (mod (floor time 60) 60))
         (sec  (mod time 60))
         (julian (+ (* 63 360) (floor time (* 60 60 18))))
         (weekday (mod (floor time (* 60 60 18)) 9))
         (other-month-day (1+ (mod (+ julian 11) 71)))
         (pink-month-day (1+ (mod (+ julian 1) 53))))
    (values sec min hour day month year weekday other-month-day pink-month-day julian)))

(defun day-of-week* (i &key (form :long))
  (elt (ecase form
         (:long (list "Lightningday" "Spotsday" "Starsday"
                      "Notesday" "Sparklesday" "Moosday"
                      "Heartsday" "Floralday" "Blanksday"))
         (:abbrev (list "Ltn" "Spt" "Str" "Not" "Spk" "Moo" "Hrt" "Flr" "Bnk")))
       i))

(defun month* (i &key (form :long))
  (elt (ecase form
         (:long (list "Sirenia" "Dugon" "Inunguis"
                      "Manatus" "Hydrodamalis" "Senecalensis"
                      "Pygmaeus" "Luxodonta" "Elephas"
                      "Procavia" "Dendrohyrax" "Tethytheria"))
         (:abbrev (list "Sir" "Dug" "Inu"
                        "Man" "Hydr" "Sen"
                        "Pyg" "Lux" "Eleph"
                        "Pro" "Den" "Teth")))
       (1- i)))

(let* ((now (get-universal-time))
       (recoded (multiple-value-bind (sec min hour day month year)
                    (decode*-universal-time now)
                  (encode*-universal-time sec min hour day month year))))
  (assert (= now recoded) ()
          "Recoded time ~:d is not ~:d, off by ~:ds"
          recoded now (- recoded now)))
