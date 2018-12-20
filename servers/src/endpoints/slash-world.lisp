;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-world.lisp is part of Tootsville
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

(defendpoint (GET "/world" "application/json")
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
  (choerogryllum:date-string (get-universal-time)))

(defendpoint (GET "/world/clock/date/long" "text/plain")
  (choerogryllum:date-string (get-universal-time)))

(defendpoint (GET "/world/clock/date/abbrev" "text/plain")
  (choerogryllum:date-string (get-universal-time) :form :abbrev))

(defendpoint (GET "/world/clock/time" "application/json")
  (multiple-value-bind (sec min hour day month year weekday other-month-day pink-month-day julian)
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
          :julian-270 (mod julian 270)
          :holiday (choerogryllum:holiday-on year month day))))

(defendpoint (GET "/world/clock/time" "text/plain")
  (multiple-value-bind (sec min hour day month year weekday other-month-day pink-month-day julian)
      (choerogryllum:decode*-universal-time (get-universal-time))
    (declare (ignore day month year weekday other-month-day pink-month-day julian))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(defun detailed-time (&optional (now (get-universal-time)))
  (multiple-value-bind (sec min hour day month year weekday other-month-day pink-month-day julian)
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
It is the ~:d~[th~;st~;nd~;rd~:;th~] day of the 270-day solar year.
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
            (mod julian 270)
            (mod (mod julian 270) 10)
            (choerogryllum:holiday-on year month day))))

(defendpoint (GET "/world/clock/time/detailed" "text/plain")
  (detailed-time))
