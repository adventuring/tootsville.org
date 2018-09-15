;;; -*- lisp  -*- LOAD this  file to start  a local loopback  server for
;;; testing

(format t "~|~3%TOOTSTEST local start

Grabbing Tootstest ASDF…")
(asdf:load-asd (merge-pathnames #p"./Tootsville.asd" *load-pathname*))
(format t "~&~|~%Loading system…")
(ql:quickload :Tootsville)
(format t "~&~|~%Starting local server…")
(eval (read-from-string "(Tootsville:start)"))
(format t "~2&Opening in browser…")
(uiop:run-program "xdg-open http://localhost:5000/Tootsville/")
(format t "~2&Ready.")
