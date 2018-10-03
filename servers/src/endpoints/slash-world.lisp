(in-package :tootsville)
(syntax:use-syntax :annot)

(defendpoint (:get "/world" "application/json")
  (setf (getf (response-headers *response*) :content-type) "application/json")
<<<<<<< HEAD:servers/src/endpoints/slash-world.lisp
  (error 'unimplemented))
=======
  (list 501 nil '(:error)))
>>>>>>> squishy-locally:servers/src/endpoints/world.lisp
