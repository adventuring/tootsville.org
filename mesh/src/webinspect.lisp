(in-package :webinspect)

(defun inspector (object)
  (warn "Temporary implementation of INSPECTOR")
  (format t "~& Object: ~s~%Aesthetic: ~:*~a" object)
  (describe object)
  object)
