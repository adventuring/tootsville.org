(in-package :Tootsville)

(defendpoint (get "/meta-game/headers" "application/json")
  (list 200 ()
        (list :headers-in
              (alist-plist (hunchentoot::headers-in*)))))
