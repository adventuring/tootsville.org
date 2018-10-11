(in-package :babylon)

(defvar *canvas* nil)
(defvar *engine* nil)

(defun init ()
  (setf *canvas* (#j:document:getElementById "renderCanvas")
        *engine* (make-new #j:BABYLON:Engine *canvas* true))
  (assert (and *canvas* *engine*))
  (#j:window:addEventListener "resize" (lambda () ((oget *engine* "resize")))))

(defun color (red green blue)
  (cond
    ((and (integerp red) (integerp green) (intergerp )
          (<= 0 red #xff) (<= 0 green #xff) (<= 0 blue #xff))
     (make-new #j:BABYLON:Color3 (/ red #xff) (/ green #xff) (/ blue #xff)))
    ((and (<= 0 red 1) (<= 0 green 1) (<= 0 blue 1))
     (make-new #j:BABYLON:Color3 red green blue))
    (t (error "Color triple not in byte nor float form: ~s" (list red green blue)))))

(defun vector3 (x y z)
  (if (and (zerop x) (zerop y) (zerop z))
      (#j:BABYLON:Vector3:Zero)
      (make-new #j:BABYLON:Vector3 x y z)))

(defvar *scene* nil)
(defvar *camera* nil)

(defun make-camera (name position)
  (let ((new-camera (make-new #j:BABYLON:FreeCamera name position *scene*)))
    (unless *camera*
      (setf *camera* new-camera))
    new-camera))

(defun make-scene ()
  (unless (and *canvas* *engine*)
    (init))
  (let* ((scene (make-new #j:BABYLON:Scene *engine*))
         (camera (make-camera "Camera 1" (vector3 0 5 -10))))
    (setf (oget scene "clearColor") (color 0.7 0.3 0.8))
    ((oget camera "setTarget") (vector3 0 0 0))
    ((oget camera "attachControl") *canvas*)))

(defun check-scene ()
  (unless (and *scene* *camera*)
    (make-scene)))

(defun make-sample-scene ()
  (check-scene)
  (#j:BABYLON:CreateGround "Ground Plane" 6 6 2 *scene*)
  (make-new #j:BABYLON:HemisphericLight "Sky Light" (vector3 0 1 0) *scene*))

(defvar *renderer* nil)

;;; If no GPU, may need to set mesh.computeBonesUsingShaders = false for
;;; each  skeletal  mesh to  get  reasonable  performance. This  is  the
;;; global default.

;;; TODO: Profile the  playback frame rate. If  quality is unacceptable,
;;; have  a Parrot  offer to  “try some  things.” Alternate  the various
;;; settings to try to locate  a configuration that does have acceptable
;;; performance on the device.

(defvar *compute-bones-using-shaders-p* t)

(defun make-render-process ()
  (check-scene)
  ((oget *engine* "runRenderLoop") (lambda () ((oget *scene* "render")))))
