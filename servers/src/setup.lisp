(cl:in-package :cl-user)
(require :asdf)
(unless (find-package :Tootsville-ASD)
  (defpackage Tootsville-ASD
    (:use :cl :asdf)))
(in-package :Tootsville-ASD)

(defvar *setup* nil)

(format t "~3& Tootsville Ⅴ Setup~3&")



;;; Ensure Quicklisp is installed.

#.(load (merge-pathnames "ensure-quicklisp.lisp"
                         *load-truename*))



;;; Bits that sometimes get lost in SBCL image dumping madness

(ignore-errors (require 'sb-introspect))
(unless (find-package :sb-introspect)
  (load #p"SYS:CONTRIB;**;sb-introspect.fasl.NEWEST"))
(ignore-errors (require 'sb-rotate-byte))
(unless (find-package :sb-rotate-byte)
  (load #p"SYS:CONTRIB;**;sb-rotate-byte.fasl.NEWEST"))

;;; Ensure  Swank  is  loaded.  (Does   Buildapp  try  to  blacklist  it
;;; or something?

(when (not (find-package "SWANK"))
  (ql:quickload :swank))

;;; The Verbose (logging) library wants to  start a new thread when it's
;;; loaded by ASDF/Quicklisp, which is  bad news during Buildapp and not
;;; super-useful  in   debugging.  This  feature  flag   keeps  it  from
;;; doing that.

(pushnew :verbose-no-init *features*)

;;; Ensure  that   the  ASD   files  of   any  submodules   are  loaded.
;;; By convention, we load submodules into lib/

(let* ((src-dir (make-pathname
                 :directory
                 (pathname-directory (or *load-pathname*
                                         *compile-file-pathname*
                                         *default-pathname-defaults*))))
       (lib-dirs (merge-pathnames (make-pathname :directory '(:relative "lib")
                                                 :name :wild :type :wild)
                                  src-dir)))
  (let ((*setup* t))
    (asdf:load-asd (merge-pathnames (make-pathname :directory '(:relative :up)
                                                   :name "tootsville"
                                                   :type "asd")
                                    src-dir)))
  (dolist (lib-dir (directory lib-dirs))
    (let ((asdf (merge-pathnames (make-pathname :name :wild
                                                :type "asd")
                                 lib-dir)))
      (dolist (another-system-definition (directory asdf))
        (format *trace-output* "~&Found system definition ~S"
                (uiop/pathname:enough-pathname another-system-definition src-dir))
        (pushnew lib-dir asdf:*central-registry*)))))

(format *trace-output*
        "~2& System Definitions registry:
~{~& • ~a~}"
        asdf:*central-registry*)

;;; misc
#+sbcl
(setf sb-impl::*default-external-format* :utf-8)



(format *trace-output* "~&Setup script completed; ready to load.~4%")

(setf *setup* t)
