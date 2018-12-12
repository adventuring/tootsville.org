(in-package :Tootsville)

(defun fetch-json (uri)
  "Fetch URI  as an application/json file  and parse it with  Yason into
a property list tree."
  (jonathan.decode:parse
   (map 'string #'code-char
        (drakma:http-request uri :method :get :accept "application/json;charset=utf-8"))
   :as :plist))
