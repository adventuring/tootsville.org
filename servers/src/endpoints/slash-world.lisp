(in-package :tootsville)
(syntax:use-syntax :annot)

(defendpoint (:get "/world" "application/json")
    (setf (getf (response-headers *response*) :content-type) "application/json")
  (error 'unimplemented))
