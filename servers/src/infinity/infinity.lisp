(in-package :Tootsville)

(defvar *infinity-ops* nil)

(defun call-infinity-from-rest (method)
  (let* ((json$ (hunchentoot:post-parameters*))
         (json (jonathan.decode:parse json$)))
    (with-user ()
      (list 200 () (funcall method json *user* (user-plan *user*))))))

(defmacro definfinity (name (lambda-list user-var world-var) &body body)
  (let ((legacy-name (symbol-munger:lisp->camel-case (string name)))
        (docstring (when (stringp (first body)) (first body)))
        (infinity-name (intern (concatenate 'string "INFINITY-" (string name))))
        (λ-list (if (eql '&rest (first lambda-list))
                    lambda-list
                    (cons '&key lambda-list))))
    (push (cons (string name) infinity-name) *infinity-ops*)
    (push (cons legacy-name infinity-name) *infinity-ops*)
    `(progn
       (defun ,infinity-name (d u r)
         ,docstring
         (destructuring-bind (,@λ-list) d
           ,@body))
       (defendpoint (POST ,(concatenate 'string "/world/infinity/" name))
         (call-infinity-from-rest  ',infinity-name )))))
