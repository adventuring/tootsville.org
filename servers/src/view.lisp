(in-package :Tootsville)

(defun render-json (object)
  (setf (hunchentoot:header-out :content-type)
        "application/json;charset=utf-8")
  (flexi-streams:string-to-octets (encode-json object)
                                  :external-format :utf-8))
