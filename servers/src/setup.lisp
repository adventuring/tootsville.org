(format t "~3& Tootsville Ⅴ Setup~3&")
(ignore-errors (require 'sb-introspect))
(unless (find-package :sb-introspect)
  (load #p"SYS:CONTRIB;**;sb-introspect.fasl.NEWEST"))
(ignore-errors (require 'sb-rotate-byte))
(unless (find-package :sb-rotate-byte)
  (load #p"SYS:CONTRIB;**;sb-rotate-byte.fasl.NEWEST"))
(when (and (find-package :ql)
           (not (find-package "SWANK")))
  (funcall (find-symbol "QUICKLOAD" :ql) :swank))
(pushnew :verbose-no-init *features*)
