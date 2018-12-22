(in-package :Tootsville)

 ;;; Tootangan Climatology:
 ;;;
 ;;; * Each day is a sine curve from 10:00 (high an hour after noon) to an
 ;;;   hour before sunrise as the low temp (varying with sunrise times)
 ;;;
 ;;; * The year is a sine curve of high temperatures
 ;;;
 ;;; * The height of winter is 21 Sirenia, the Winter Solstice; the height
 ;;;   of summer is 21 Pygmaeus (7/21), the Summer Solstice
 ;;;
 ;;; * Daylight hours change with the solstices, also as a sine curve, with the
 ;;;   same high and low points
 ;;;
 ;;; * The delta between high and low temperatures is determined by the
 ;;;   daylight hours, so there is very little cooling-off in the summer,
 ;;;   but in the winter, the low temperature can be much lower
 ;;;   (relatively).
 ;;;
 ;;; * The basic reference point is Tootsville itself, at just above sea
 ;;;   level.  Higher altitudes have a relatively lower temperature, with
 ;;;   the higher peaks never rising above 0, while Tootsville will always
 ;;;   rise above 0 at least at noon, even on the Winter Solstice.
 ;;;
 ;;; * Precipitation chances are highest in the third months -- Inunguis,
 ;;;   Senecalensis, Elephas, and Tethytheria -- peaking at the 15th of each
 ;;;   third month.  Thus, the least chance of precipitation is around the
 ;;;   1st of the second month of each quarter -- 1 Dugon, Hyrodamalis,
 ;;;   Luxodonta, and Dendrohyrax
 ;;;
 ;;; * Winter precipitation (from about 1 Tehytheria to about 30 Dugon) will
 ;;;   tend to be coming from the mountains to the sea, and bring snow, with
 ;;;   a coresponding drop in temperature.  Summer precipitation (from about
 ;;;   1 Senecalensis to 30 Luxodonta) will tend to come from the south
 ;;;   seas, and bring warmer temperatures.  The temperature won't be
 ;;;   generally affected at all by precipitation during the spring and
 ;;;   autumn months.
 ;;;
 ;;; * The seasonal, general patterns are subject to the actual, daily
 ;;;   weather generation.
 ;;;
 ;;; * Cloud patterns are generated hourly at the edges of the world. These
 ;;;   patterns are pushed around by invisible-to-the-players fields of
 ;;;   relative wind strengths and directions, and humidity.  The humidity
 ;;;   fields affect the likelihood of precipitation, the wind fields alter
 ;;;   the direction of movement of the clouds.
 ;;;


