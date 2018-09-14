(format t "~3& Tootsville Ⅴ Setup~3&")



;;; Ensure Quicklisp is installed.  Checks the GPG signature of the
;;; bootloader against a copy downloaded 2018-9-13; if this starts failing,
;;; it may be that Xach has uploaded a new bootstrap script.

#.(unless (find-package :ql)
    (let ((quicklisp.lisp (merge-pathnames 
                           (make-pathname :directory '(:relative "quicklisp")
                                          :name "setup" :type "lisp")
                           (user-homedir-pathname))))
      (when (probe-file quicklisp.lisp)
        (load quicklisp.lisp)))
    (unless (find-package :ql)
      (uiop/run-program:run-program
       "cd; curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp")
      (uiop/run-program:run-program "gpg --recv-keys 028B5FF7")
      (with-output-to-file (s "~/quicklisp.sign.asc" :if-exists :supersede)
        (princ "-----BEGIN PGP SIGNATURE-----
Version: GnuPG/MacGPG2 v2.0.22 (Darwin)
Comment: GPGTools - https://gpgtools.org

iQIcBAABCgAGBQJUyVD2AAoJEDB5ZasCi1/3EYAQAIPm+dXrTCRgHA6aFZTc8VeB
DoBFsQPlFsLeKjixy3wSalPhI751ItWyy3DOx1tRYpFN0aYGSRbiMuVUF9DVMsSB
ROeNCg+f6lvtHuSokHKiZ95y8GarBqs4qguHi1Ir+n+inno4ZfE+8JvrxxQ9Lo4Z
KGgRoyJceaVcku3lmQK131i2eOf8RZaHmXv24tHprXpnle0d74etXO0TAdmpk5R2
OaeduaitR7o3cGT0JCS39rC5qH/H19jz9XkTjuLOGNqiKcX7XePTejdSLbT7FphN
4G2uTN8Z5HiYy56OpcuLdjHlPFiThwmIId6NMO3PHNlVAeL5PMKzw9tZTQlw4C5E
XKgL7gqC4L6Ys64/70IIBPo+L8TG74VkEbcWkNr3TCzAdz8x1Sa40YHRoVpRFK8M
T+v/qMJ8sgHb9o1OsEniv8eCFyFNPd7AlrRYehWQqRaVCuFG//95jEGNI/ig7vjK
v/z6tIyqi2e6zJPsJxAPJ8Y23jkTH4u5dvFct+k1fkZOCIpA/jLUT1RuWqLKTpOF
pSlDVmrqjrhM+jVnFEyWOg/mkSmQsY0jfC2sVDpx4XEGq3PdBnKqpPBTL0L8s5sg
YmqsGjMVk9IMCtylwuBlP5VGgoB8GmHGdhbeSKNxQJb75voDOiIS/sOP5+ACOxcA
Yg8wz616XOOHpLNUki3j
=yTd1
-----END PGP SIGNATURE-----"
               s))
      (uiop/run-program:run-program
       "cd; gpg --verify quicklisp.sign.asc quicklisp.lisp")
      (load "~/quicklisp.lisp")
      (handler-case
          (funcall (intern "INSTALL" :quicklisp-quickstart))
        (error (c)
          (declare (ignore c))
          (when (find-restart :load-setup)
            (invoke-restart :load-setup))))))

;;; Bits that sometimes get lost in SBCL image dumping madness

(ignore-errors (require 'sb-introspect))
(unless (find-package :sb-introspect)
  (load #p"SYS:CONTRIB;**;sb-introspect.fasl.NEWEST"))
(ignore-errors (require 'sb-rotate-byte))
(unless (find-package :sb-rotate-byte)
  (load #p"SYS:CONTRIB;**;sb-rotate-byte.fasl.NEWEST"))

;;; Ensure Swank is loaded.  (Does Buildapp try to blacklist it or
;;; something?

(when (not (find-package "SWANK"))
  (ql:quickload :swank))

;;; The Verbose (logging) library wants to start a new thread when it's
;;; loaded by ASDF/Quicklisp, which is bad news during Buildapp and not
;;; super-useful in debugging.  This feature flag keeps it from doing that.

(pushnew :verbose-no-init *features*)

;;; Ensure that the ASD files of any submodules are loaded.  By convention,
;;; we load submodules into lib/

(let* ((src-dir (make-pathname
                   :directory
                   (pathname-directory (or *load-pathname*
                                           *compile-file-pathname*
                                           *default-pathname-defaults*))))
       (lib-dirs (merge-pathnames (make-pathname :directory '(:relative "lib")
                                                 :name :wild)
                                  src-dir)))
  (dolist (lib-dir (directory lib-dirs))
    (let ((asdf (merge-pathnames (make-pathname :name :wild
                                                :type "asd")
                                 lib-dir)))
      (dolist (another-system-definition (directory asdf))
         (format *trace-output* "~&Found system definition ~S"
                 another-system-definition)
         (asdf:load-asd another-system-definition)))))
