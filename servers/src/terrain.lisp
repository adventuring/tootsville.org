(in-package :Tootsville)

(defgeneric spawn-terrain (place x-coord y-coord z-coord))

(defmethod spawn-terrain ((place (eql :tootanga)) x y z)
  (assert (<= -3000 x 3000))
  (assert (<= -3000 y 3000))
  (assert (<= 0 z 255))
  
  ;; sample height and terrain for (/ x 10) (/ y 10) on the map files
  ;; grab surrounding 4 as well
  ;; map terrain color code to terrain type
  ;; generate contour map based on height map
  ;; apply sea level
  ;; apply above-sea-level water
  ;; add noise appropriate to terrain
  ;; save to map database 
  
  (error 'unimplemented)
  
  )

(defvar *terrain-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                    "data/Tootanga-map"
                                                    :type "png" )))
(defvar *elevation-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                    "data/Tootanga-elevation"
                                                    :type "png" )))
