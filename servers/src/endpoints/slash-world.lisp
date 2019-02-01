;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-world.lisp is part of Tootsville
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

(defendpoint (GET "/world" "application/json")
  "Get world-related info in general. Not implemented."
  (error 'unimplemented))

(defendpoint (get "/world/tootanga/:x-coord/:y-coord/:z-coord" "application/json")
  "Get the information about the area near (X-COORD,Y-COORD,Z-COORD)

The terrain and objects in that area, characters, &c. will be returned.

Your character must be able to observe that general area. No peeking!
"
  (if-let (doc (clouchdb:get-document
                (format nil "T:~36R,~36R,~36R" x-coord y-coord z-coord) ))
    doc
    (spawn-terrain :tootanga x-coord y-coord z-coord)))



(defendpoint (GET "/world/clock/date" "text/plain")
  "Get the date on Chœrogryllum (pretty-printed date string)"
  (choerogryllum:date-string (get-universal-time)))

(defendpoint (GET "/world/clock/date/long" "text/plain")
  "Get the date on Chœrogryllum (pretty-printed date string)"
  (choerogryllum:date-string (get-universal-time)))

(defendpoint (GET "/world/clock/date/abbrev" "text/plain")
  "Get the date on Chœrogryllum (abbreviated date string)"
  (choerogryllum:date-string (get-universal-time) :form :abbrev))

(defendpoint (GET "/world/clock/time" "application/json")
  "Get the date & time on Chœrogryllum as a JSON structure"
  (multiple-value-bind (sec min hour day month year weekday
                            other-month-day pink-month-day julian)
      (choerogryllum:decode*-universal-time (get-universal-time))
    (list :sec sec
          :min min
          :hour hour
          :day day
          :month month
          :year year
          :weekday weekday
          :other-month-day other-month-day
          :pink-month-day pink-month-day
          :julian julian
          :julian-360 (mod julian 360)
          :holiday (choerogryllum:holiday-on year month day))))

(defendpoint (GET "/world/clock/time" "text/plain")
  "Get the current time on Chœrogryllum (time string with seconds)"
  (multiple-value-bind (sec min hour day month year weekday
                            other-month-day pink-month-day julian)
      (choerogryllum:decode*-universal-time (get-universal-time))
    (declare (ignore day month year weekday other-month-day pink-month-day julian))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(defun detailed-time (&optional (now (get-universal-time)))
  "Get a long string explaining the date, time, and other info."
  (multiple-value-bind (sec min hour day month year weekday
                            other-month-day pink-month-day julian)
      (choerogryllum:decode*-universal-time now)
    (format nil "~
Currently the universal time code is ~:d.

On the planet Chœorgryllum, it is ~2,'0d:~2,'0d:~2,'0d in the ~
~[wee hours of the morning~;morning~;afternoon~;evening~] on
~a.

That's the ~:r day of the nine-day week, and the ~:r month
of the twelve months of the year.

~[It is new moon for The Moon.~:;~
It is the ~:*~:r day of The Moon's 30-day month.~]
~[It is new moon for The Other Moon.~:;~
It is the ~:*~:r day of The Other Moon's 71-day month.~]
~[It is new moon for The Pink Moon.~:;~
It is the ~:*~:r day of The Pink Moon's 53-day month.~]
It is the ~:d~[th~;st~;nd~;rd~:;th~] day of the 360-day calendar year.
~@[

It is ~a.~]
"
            now
            hour min sec
            (floor hour 6)
            (choerogryllum:date-string now :form :long)
            (1+ weekday) month
            day
            other-month-day
            pink-month-day
            (mod julian 360)
            (let ((n (mod (mod julian 360) 10)))
              (if (= 12 (mod julian 360)) 9 n))
            (choerogryllum:holiday-on year month day))))

(defendpoint (GET "/world/clock/calendar/year/:year/month/:month/fragment"
                  "text/html")
  "Get a calendar fragment in HTML for MONTH of YEAR."
  (list 200 () (chœrogryllum::cal-month.html
                (parse-integer year) (parse-integer month))))

(defendpoint (GET "/world/clock/calendar/now/fragment" "text/html")
  "Get a calendar fragment in HTML for MONTH of YEAR."
  (list 200 () (chœrogryllum::cal-month.html)))

(defendpoint (GET "/world/clock/calendar/year/:year/fragment" "text/html")
  "Get a calendar fragment in HTML for 12 months of YEAR."
  (list 200 () 
        (format nil "~{~a~%~^<br>~%~}"
                (loop for month from 1 upto 12
                   collecting (chœrogryllum::cal-month.html 
                               (parse-integer year) month)))))

(defendpoint (GET "/world/clock/calendar/year/:year/month/:month" "text/html")
  "Get a calendar as an HTML page for MONTH of YEAR."
  (list 200 () 
        (format nil "<html>
<head><meta charset=\"utf-8\">
<title> Month ~d of Year ~d — Chœrogryllum Calendar </title></head>
<body>~a</body></html>"
                month year
                (chœrogryllum::cal-month.html 
                 (parse-integer year) (parse-integer month)))))

(defendpoint (GET "/world/clock/time/detailed" "text/plain")
  "Get a long string explaining the date, time, and other info."
  (detailed-time))

(defconstant +moon-year+ 3600000)
(defconstant +other-moon-year+ 583243)
(defconstant +pink-moon-year+ 452398723)

(defconstant +moon-day+ (* 18 60 59))
(defconstant +other-moon-day+ (* 2 18 60 58))
(defconstant +pink-moon-day+ (* 8 18 60 56))

(defun sinus (x range)
  (sin (/ (mod x range) (/ range pi))))

(defun sky-contents (x y z &optional (now (get-universal-time)))
  (let ((day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
    (list :|sun| (list :|azimuth| (sinus (- now (* 21 18 60 60)) (* 360 18 60 60))
                       :|elevation| (sinus now (* 18 60 60)))
          :|moon| (list :|azimuth| (sinus now +moon-year+)
                        :|elevation| (sinus now +moon-day+)
                        :|fullness| (sinus day 30)
                        :|phase| (if (plusp (- 30/2 day))
                                     "waxing" "waning"))
          :|other-moon| (let* ((now (+ now (* 11 18 60 60)))
                               (day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
                          (list :|azimuth| (sinus now +other-moon-year+)
                                :|elevation| (sinus now +other-moon-day+)
                                :|fullness| (sinus day 71)
                                :|phase| (if (plusp (- 71/2 day))
                                             "waxing" "waning")))
          :|pink-moon| (let* ((now (+ now (* 18 60 60)))
                              (day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
                         (list :|azimuth| (sinus now +pink-moon-year+)
                               :|elevation| (sinus now +pink-moon-day+)
                               :|fullness| (sinus day 53)
                               :|phase| (if (plusp (- 53/2 day))
                                            "waxing" "waning")))
          :|clouds| (clouds x y z)
          :|precipitation| (multiple-value-list (precipitation x y z)))))

(defendpoint (GET "/world/sky/tootanga/:x-coord/:y-coord/:z-coord")
  "Get the contents of the sky visible over (X-COORD, Y-COORD, Z-COORD).

This data  includes the position  of the Sun  (which could be  below the
horizon), the position of each moon,  and the (fractional) phase of that
moon. It  may also include  an array  of clouds, precipitation  (rain or
snow), lightning patterns, &c.

This will @emph{not} include things that are flying in the sky.
"
  (list 200
        nil
        (sky-contents x-coord y-coord z-coord)))
