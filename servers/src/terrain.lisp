;;;; -*- lisp -*-
;;;
;;;; ./servers/src/terrain.lisp is part of Tootsville
;;;
;;;; Copyright © 2016,2017 Bruce-Robert Pocock; © 2018,2019 The Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This program is Free Software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
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

(define-constant +habitat-colors+
    '(((color24-rgb 65 0 145) . :shaddow)
      ((color24-rgb 150 150 150) . :rocky)
      ((color24-rgb 239 14 78) . :swamp)
      ((color24-rgb 38 152 65) . :grassland)
      ((color24-rgb 236 237 138) . :desert)
      ((color24-rgb 145 82 0) . :savannah)
      ((color24-rgb 35 239 14) . :forest)
      ((color24-rgb 150 138 237) . :ocean)
      ((color24-rgb 255 255 255) .  :ice))
   :test 'equalp
   :documentation "The color triplets which represent each type of habitat
  in the PNG habitat map.")

(defvar *global-heightmap%)
(defvar *global-heightmap-x%)
(defvar *global-heightmap-y%)
(defvar *features%)

(deftype kind-of-habitat ()
  '(member :shaddow :rocky :swamp :grassland
    :desert :savannah :forest :ocean :ice
    :moon :pink-moon :moon-base :city :farm
    :manatee-city :beachside :space :asteroid-field))

(deftype map-places ()
  '(member :tootanga
    :moon :other-moon :pink-moon
    :orbit))



;; Reading map data from images


(defvar *habitat-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                      "data/Tootanga-map"
                                                      :type "png"))
  "The Tootanga map contains color-coded pixels representing the various
habitat areas of the game. Each pixel represents a 200m by 200m area; thus,
the entire map area (800 by 600 pixels) represents a playable game area of
160 by 120 km.")

(defvar *elevation-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                    "data/Tootanga-elevation"
                                                    :type "png"))
  "The Tootangan elevation map provides a logarithmic altitude map of the
approximate/net altitude of each 200 by 200 meter area of the game.")

(defpost check-map-heights ()
  "Ensure that both maps are 600px high."
  (= 600 (pngload:height *habitat-map*) (pngload:height *elevation-map*)))

(defpost check-map-widths ()
  "Ensure that both maps are 800px wide."
  (= 800 (pngload:width *habitat-map*) (pngload:width *elevation-map*)))

(defun habitat<-pixel (color)
  "Which habitat type does the given color triplet represent?"
  (let ((c (or (assoc color +habitat-colors+ :test #'color24=)
               (error "palette mismatch in habitat map on ~a"
                      (color24-name color)))))
    (cdr c)))

(defun get-9-terrain-tiles (x y)
  "Returns 9 tiles of terrain centered on X Y as a 3 by 3 array"
  (let ((x-offset (1- (+ 400 (floor x 200))))
        (y-offset (1- (+ 300 (floor y 200))))
        (elevation (make-array '(3 3) :element-type '(unsigned-byte 8)))
        (habitat (make-array '(3 3) :element-type 'symbol :initial-element :ocean)))
    (when (or (< x-offset 1) (< y-offset 1)
              (> x-offset 799) (> y-offset 599))
      (error 'unimplemented))
    (dotimes (ix 3)
      (dotimes (iy 3)
        (setf (aref elevation ix iy)
              (aref (pngload:data *elevation-map*) (+ x-offset ix) (+ y-offset iy) 2)

              (aref habitat ix iy)
              (habitat<-pixel
               (color24-rgb 
                 (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 0)
                 (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 1)
                 (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 2))))))
    (list elevation habitat)))



;;; Adding features

(defun terrain/connect-streams ()
  "Connect up to any stream in an adjacent tile.

If no adjacent tile has yet been spawned, small chance of creating a new
 stream.  If streams  enter  from more  than one  side,  connect up  as
 a branching stream. "
  (error 'unimplemented))

(defun terrain/add-cactus ()
  "Add a cactus"
  (error 'unimplemented))

(defun terrain/add-tree ()
  "Add a random tree or bush."
  (error 'unimplemented))

(defun terrain/add-mushrooms ()
  "Add a cluster of mushrooms or similar."
  (error 'unimplemented))

(defun terrain/add-log ()
  "Adds a fallen log or similar feature."
  (error 'unimplemented))

(defun terrain/add-flowers ()
  "Add a random cluster of appropriate flowers or herbs."
  (error 'unimplemented))

(defun terrain/stream-present-p ()
  "Does a stream bisect the currently-active space?

Should return true  if a body of water exists  which enters the space
 from any  side and  bisects the  space into  two disjoint  land areas.
 Terminus of  a stream  or completely underwater  are not  “streams” by
 this definition."
  (error 'unimplemented))
  
(defun point-underwater-p (x y)
  "Is the point underwater? TODO"
  (error 'unimplemented))
  
(defun find-random-point-if (function)
  "Find a random point within the space for which FUNCTION is true.

Returns (LIST X Y)"
  (loop
     for x = (/ (random 20000) 100)
     for y = (/ (random 20000) 100)
     until (funcall function x y)
     finally (return (list x y))))

(defun terrain/add-small-pond ()
  "Create a pool of water smaller than the tile and contained within it. TODO"
  (if (terrain/stream-present-p)
      (destructuring-bind (x y)
          (find-random-point-if #'point-underwater-p)
        (error 'unimplemented))
      (error 'unimplemented)))

(defun terrain/add-shaddow-bush ()
  "Add a Shaddow bush to the area"
  (error 'unimplemented))

(defun terrain/add-shaddow-stalagmite ()
  "Add a Shaddow stalagmite to the area"
  (error 'unimplemented))

(defun terrain/add-shaddow-pit ()
  "Add a Shaddow pit to the area"
  (error 'unimplemented))


;;; Per-habitat generation rules.

(defgeneric generate-terrain-features (contour habitat)
  (:documentation "Generate the terrain features based upon the contour map
  and habitat type.  Methods of this function specialize upon the habitat
  type."))

(defmethod generate-terrain-features :before (contour habitat)
  "Connect streams, regardless of the habitat type."
  (terrain/connect-streams))

(defmethod generate-terrain-features (contour (habitat (eql :shaddow)))
  (loop repeat (random 100) do (terrain/add-shaddow-stalagmite))
  (loop repeat (random 100) do (terrain/add-shaddow-bush))
  (loop repeat (random 25) do (terrain/add-shaddow-pit)))

(defmethod generate-terrain-features (contour (habitat (eql :swamp)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :ocean)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :grassland)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :forest)))
  (loop repeat (random 200) do (terrain/add-tree))
  (loop repeat (random 10) do (terrain/add-mushrooms))
  (loop repeat (random 10) do (terrain/add-log))
  (loop repeat (random 5) do (terrain/add-flowers))
  (loop repeat (random 5) do (terrain/add-small-pond)))

(defmethod generate-terrain-features (contour (habitat (eql :desert)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :savannah)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :rocky)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :ice)))
  (error 'unimplemented))



(defgeneric generate-terrain-contour (9-elevations habitat x y scale)
 (:documentation "Generate the contour for a tile area"))

(defun copy-terrain-edge-horz (start-x y end-x dest-x dest-y)
  (loop for xi from start-x to end-x
     for yi = y
     for xj from dest-x to (+ dest-x (- start-x end-x))
     for yj = dest-y
     do (setf (global-heightmap-corner xj yj) (global-heightmap-corner xi yi))))

(defun copy-terrain-edge-vert (x start-y end-y dest-x dest-y)
  (loop for yi from start-y to end-y
     for xi = x
     for yj from dest-y to (+ dest-y (- start-y end-y))
     for xj = dest-x
     do (setf (global-heightmap-corner xj yj) (global-heightmap-corner xi yi))))

(defun generate-terrain-blank-edge-horz (start-x y end-x base-elevation)
  (loop for xi from start-x to end-x
     for yi = y
     do (setf (global-heightmap-corner xi yi) base-elevation)))

(defun generate-terrain-blank-edge-vert (x start-y end-y base-elevation)
  (loop for yi from start-y to end-y
     for xi = x
     do (setf (global-heightmap-corner xi yi) base-elevation)))

(defun fill-blank-contour (x y base-elevation)
  (dotimes (xi 200)
    (dotimes (yi 200)
      (setf (global-heightmap-corner (+ xi x) (+ yi y)) (+ base-elevation (- (random 5) 2))))))

(defun smoothe-contour-200×200 (x y &optional (repeats 3))
  (dotimes (i repeats)
    (dotimes (xi 200)
      (dotimes (yi 200)
        (setf (global-heightmap-corner (+ xi x) (+ yi y))
              (floor (+ (* 4 (global-heightmap-corner (+ xi x) (+ yi y)))
                        (global-heightmap-corner (+ xi x -1) (+ yi y))
                        (global-heightmap-corner (+ xi x 1) (+ yi y))
                        (global-heightmap-corner (+ xi x) (+ yi y -1))
                        (global-heightmap-corner (+ xi x) (+ yi y 1)))
                     8))))))

(defun generate-blank-contour (9-elevations x y)
  (if (terrain-exists-p :Tootanga (1- x) y)
      (copy-terrain-edge-vert (1- x) y (+ y 200) x y)
      (generate-terrain-blank-edge-vert (1- x) (1- y) (+ y 200)
                                        (aref 9-elevations 0 1)))
  (if (terrain-exists-p :Tootanga (+ x 200) y)
      (copy-terrain-edge-vert (+ x 200) y (+ y 200) x y)
      (generate-terrain-blank-edge-vert (+ x 200) (1- y) (+ y 200)
                                        (aref 9-elevations 2 1)))
  (if (terrain-exists-p :Tootanga x (1- y))
      (copy-terrain-edge-horz (1- x) (1- y) (+ x 200) x y)
      (generate-terrain-blank-edge-horz (1- x) (1- y) (+ x 200)
                                        (aref 9-elevations 1 0)))
  (if (terrain-exists-p :Tootanga x (+ y 200))
      (copy-terrain-edge-horz (1- x) (+ y 200) (+ x 200) x y)
      (generate-terrain-blank-edge-horz (1- x) (+ y 200) (+ x 200)
                                        (aref 9-elevations 1 2)))
  (fill-blank-contour x y (aref 9-elevations 1 1))
  (smoothe-contour-200×200 x y 30))


(defun (setf global-heightmap-corner) (elevation x y)
  (setf (aref *global-heightmap%
              (- x *global-heightmap-x%)
              (- y *global-heightmap-y%))
        elevation))

(defun global-heightmap-corner (x y)
  (aref *global-heightmap%
        (- x *global-heightmap-x%)
        (- y *global-heightmap-y%)))

(defmethod generate-terrain-contour (9-elevations habitat x y (scale (eql 0)))
  ;; (format *trace-output* "~& Initial blank slate:")
  ;; (dump-global-heightmap x y)
  (generate-blank-contour 9-elevations x y)
  ;; (format *trace-output* "~& With neighbouring elevations:")
  ;; (dump-global-heightmap x y)
  (call-next-method 9-elevations habitat x y 8))

(defmethod generate-terrain-contour (9-elevations habitat x y (scale (eql 1)))
  t)

(defgeneric habitat-elevation-roughness (habitat)
  (:documentation "How much relative roughness to the contour for this habitat?")
  (:method (habitat) 1)
  (:method ((habitat (eql :desert))) 1/100)
  (:method ((habitat (eql :grasslands))) 1/6)
  (:method ((habitat (eql :savannah))) 1/12)
  (:method ((habitat (eql :ocean))) 1/2))

(defun shift-contour-point (x y shift)
  "Shift a point on the contour map vertically"
  (setf (global-heightmap-corner x y)
        (min #xff (max 0 (+ (global-heightmap-corner x y)
                            shift)))))

(defmethod generate-terrain-contour (9-elevations habitat x y step)
  (let ((scale (elt '(0 1 2 5 10 25 50 100 200)
                    step)))
    (dotimes (xi (floor 200 scale))
      (dotimes (yi (floor 200 scale))
        (let ((shift (round (* (sqrt (* (random (let ((r (floor scale 5)))
                                                  (if (zerop r) scale r)))
                                        (habitat-elevation-roughness habitat)))
                               (signum (1- (random 3)))))))
          (dotimes (xj scale)
            (dotimes (yj scale)
              (shift-contour-point (+ x (* xi scale) xj)
                                   (+ y (* yi scale) yj)
                                   shift))))))
    (smoothe-contour-200×200 x y)

    ;; (format  *trace-output*   "~&  After  contour   randomization  on
    ;; ~D×~:*~D     square~p:"     scale      (floor     200     scale))
    ;; (dump-global-heightmap x y)
    )

  (generate-terrain-contour 9-elevations habitat x y (1- step)))

(defun dump-global-heightmap (x y)
  (format *trace-output* "~& ┎────────────────────────────────────────────────────────────────┒~
~{~% ┃ ~{~2,'0x~^ ~} ┃~}~
~% ┖────────────────────────────────────────────────────────────────┚"
          (loop for yi from y upto (+ y 200) by 5
             collect (loop for xi from x upto (+ x 200) by 10
                        collect (global-heightmap-corner xi yi)))))



;;; Spawn new, never-before-seen terrain block
;;
;;; Terrain blocks are 200m×200m and  can potentially be on Chœrogryllum
;;; (:Tootanga), in the  near-Chœrogryllum orbit (:Oribt), or  on one of
;;; the moons (:Moon, :Other-Moon, :Pink-Moon).

(defgeneric spawn-terrain (place x-coord y-coord))

(defmethod spawn-terrain ((place (eql :tootanga)) (x integer) (y integer))
  (assert (<= -80000 x 80000))
  (assert (<= -60000 y 60000))
  (let ((*global-heightmap% (make-array (list 202 202) :element-type '(unsigned-byte 8)))
        (*global-heightmap-x% (1- x))
        (*global-heightmap-y% (1- y))
        (*features%))
    (destructuring-bind (elevation habitat) (get-9-terrain-tiles x y)
      (verbose:info :terrain "~& Generating map at (~:d,~:d) ~
in habitat ~:(~A~) with elevations ~S"
                    x y (aref habitat 1 1) elevation)
      (let* ((contour (generate-terrain-contour elevation habitat x y 0))
             (features (generate-terrain-features contour (aref habitat 1 1)))))
      ;; (format *trace-output* "~& Final rough map:")
      ;; (dump-global-heightmap x y)
      ))
  ;; apply sea level
  ;; save to map database

  )



(defun terrain-db-key (place x y)
  (check-type place map-places)
  (check-type x integer)
  (check-type y integer)
  (format nil "World/~:(~a~)/~x×~x" place (floor x 200) (floor y 200)))

(defun terrain-exists-p (place x y)
  "If terrain has been previously defined at the tile given, return it.

Use `TERRAIN' generally instead."
  (clouchdb:get-document (terrain-db-key place x y)))

(defun terrain (place x y)
  "Obtain the terrain tile in PLACE at X,Y

PLACE is one of :Tootanga, :Moon, :Other-Moon, :Pink-Moon.

X and Y must be aligned to 200m increments."
  (or (terrain-exists-p place x y)
      (spawn-terrain place x y)))



