(in-package :Tootsville)

(defun fetch-json (uri)
  "Fetch URI  as an application/json file  and parse it with  Yason into
a property list tree."
  (yason:parse
   (map 'string #'code-char
        (drakma:http-request uri :method :get :accept "application/json;charset=utf-8"))
   :object-as :plist
   :object-key-fn (compose #'make-keyword #'string-upcase)
   :json-arrays-as-vectors t))
