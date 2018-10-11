;;;; endpoints /version/*
(in-package :Tootsville)

(defendpoint (:get "/version/about" "application/json")
    (list 200 nil (plist-alist (version-info-list))))

(defendpoint (:get "/version/about" "text/plain")
    (list 200 nil (version-info-report-string '(:*))))

(defendpoint (:get "/version/about/:param" "text/plain")
    (if param
        (list 200 nil
              (version-info-report-string
               (uiop:split-string param :separator "/")))
        (list 400 nil
              "You forgot to ask anything.")))

(defendpoint (:get "/version/about/:param" "application/json")
    (list 200 nil
          (list param
                (version-info-report-string
                 (uiop:split-string param :separator "/")))))
