;;;; endpoints /version/*
(in-package :tootsville)

(defendpoint (:get "/version" "application/json")
    (list 200 nil (plist-alist (version-info-list))))

(defendpoint (:get "/version" "text/plain")
    (list 200 nil (version-info-report-string '(:*))))

(defendpoint (:get "/version/:param" "text/plain")
    (list 200 nil
          (version-info-report-string
           (uiop:split-string param :separator "/"))))
