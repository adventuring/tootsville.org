(in-package :Tootsville)

 ;;; Tootangan Climatology:
 ;;;
 ;;; * Each day is a sine curve from  10:00 (high an hour after noon) to
 ;;;   an  hour before  sunrise as  the low  temp (varying  with sunrise
 ;;;   times)
 ;;;
 ;;; * The year is a sine curve of high temperatures
 ;;;
 ;;; * The  height of  winter is  21 Sirenia,  the Winter  Solstice; the
 ;;;   height of summer is 21 Pygmaeus (7/21), the Summer Solstice
 ;;;
 ;;; * Daylight hours change  with the solstices, also as  a sine curve,
 ;;;   with the same high and low points
 ;;;
 ;;; * The delta between high and  low temperatures is determined by the
 ;;;   daylight  hours,  so there  is  very  little cooling-off  in  the
 ;;;   summer, but in the winter, the  low temperature can be much lower
 ;;;   (relatively).
 ;;;
 ;;; * The basic reference point is Tootsville itself, at just above sea
 ;;;   level. Higher altitudes have a relatively lower temperature, with
 ;;;   the  higher peaks  never rising  above 0,  while Tootsville  will
 ;;;   always   rise  above   0  at   least   at  noon,   even  on   the
 ;;;   Winter Solstice.
 ;;;
 ;;; * The seasonal, general  patterns are subject to  the actual, daily
 ;;;   weather generation.
 ;;;
 ;;; * Cloud patterns  are generated hourly  at the edges of  the world.
 ;;;   These  patterns  are  pushed around  by  invisible-to-the-players
 ;;;   fields of  relative wind strengths and  directions, and humidity.
 ;;;   The humidity  fields affect the likelihood  of precipitation, the
 ;;;   wind fields alter the direction of movement of the clouds.
 ;;;

(defun current-temp (x y z) ;; TODO
  (declare (ignore x y z))
  (let ((low 15) (high 20))
    (+ low (* (- high low) (sinus (get-universal-time) (* 18 60 60))))))

(defun precipitation (x y z) ;; TODO
  (declare (ignore x y z))
  (values 0 nil))

(defun clouds (x y z) ;; TODO
  (declare (ignore x y z))
  (list nil))

(defstruct wind-vector x-magnitude y-magnitude)

(defun wind-x (wind-vector) (wind-vector-x-magnitude wind-vector))
(defun wind-y (wind-vector) (wind-vector-y-magnitude wind-vector))
(declaim (inline wind-x wind-y))

(defun make-wind-vector-field ()
  (make-array '(800 600) 
              :element-type 'wind-vector 
              :initial-element (make-wind-vector)))

(defvar *wind-vector-field* (make-wind-vector-field))

(defvar *humidity-field* 
  (make-array '(800 600)
              :element-type 'float
              :initial-element 0))

(defun tick-weather-day ()
  "@itemize

@item

Precipitation  chances are  highest  in the  third  months --  Inunguis,
Senecalensis, Elephas,  and Tethytheria --  peaking at the 15th  of each
third month. Thus,  the least chance of precipitation is  around the 1st
of the second month of each  quarter -- 1 Dugon, Hyrodamalis, Luxodonta,
and Dendrohyrax
 
@item 

Winter precipitation  (from about 1  Tehytheria to about 30  Dugon) will
tend to be  coming from the mountains  to the sea, and  bring snow, with
a  coresponding drop  in temperature.  Summer precipitation  (from about
1 Senecalensis to  30 Luxodonta) will tend to come  from the south seas,
and  bring  warmer  temperatures.  The temperature  won't  be  generally
affected at all by precipitation during the spring and autumn months.

@end itemize"
  (multiple-value-bind (_sec _min _hour day month _year
                             _weekday _other _pink _julian)
      (choerogryllum:decode*-universal-time)
    (declare (ignore _sec _min _hour _weekday _other _pink _julian _year))
    (let* ((yday (+ (* 30 month) day))
           (chance-new-precip (+ 0.05 (sinus yday 90)))
           (precip-magnitude (round (* 100 (random chance-new-precip))))
           (prevailing-wind-θ (sinus (- yday 21) 360))
           (prevailing-wind-x (cos prevailing-wind-θ))
           (prevailing-wind-y (sin prevailing-wind-θ)))
      (loop for n from 1 upto precip-magnitude
         for x = (* prevailing-wind-x (random 800))
         for y = (* prevailing-wind-y (random 600))
         do (incf (aref *humidity-field* x y) (random chance-new-precip))))))

(defvar *weather-kernel*)

(defun ensure-weather-kernel ()
  (or *weather-kernel*
      (setf *weather-kernel* (lparallel:make-kernel 
                              (min 1 (1- (processor-count)))))))

(defun tick-weather-minute ()
  (let ((lparallel:*kernel* (ensure-weather-kernel)))
    (lparallel:pdotimes (x 800)
      (dotimes (y 600)
        (let* ((wind-x (wind-x (aref *wind-vector-field* x y)))
               (wind-y (wind-y (aref *wind-vector-field* x y)))
               (dest-x (+ x (* 2 wind-x)))
               (dest-y (+ y (* 2 wind-y)))
               (Δx (nth-value 1 (round dest-x)))
               (Δy (nth-value 1 (round dest-y)))
               (Δ (+ Δx Δy))
               (take (min (aref *humidity-field* x y) Δ)))
          (decf (aref *humidity-field* x y) take)
          (when (and (< 0 dest-x 800)
                     (< 0 dest-y 600))
            (incf (aref *humidity-field* dest-x dest-y) take)))))))
