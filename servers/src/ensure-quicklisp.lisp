;;;; -*- lisp -*-
;;;
;;;; ./servers/src/ensure-quicklisp.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :CL-USER)



(unless (find-package :ql)
  (let ((quicklisp.lisp (merge-pathnames
                         (make-pathname :directory '(:relative "quicklisp")
                                        :name "setup" :type "lisp")
                         (user-homedir-pathname))))
    (when (probe-file quicklisp.lisp)
      (load quicklisp.lisp)))
  (unless (find-package :ql)
    (flet ((check-installed (command
                             &optional (package command))
             (handler-case (uiop/run-program:run-program
                            (format nil "which ~a" command))
               (uiop/run-program:subprocess-error (c)
                 (declare (ignore c))
                 (format *error-output*
                         "~2&Please install “~a”; perhaps using:
 • pkcon install ~1@*~a
 • sudo dnf install ~1@*~a
 • sudo apt install ~1@*~a"
                         package)
                 (quit)))))
      (check-installed "curl")
      (check-installed "gpg" "gnupg"))
    (format t "~2& Quicklisp is not installed; ~
downloading bootstrap script … ~2&")
    (uiop/run-program:run-program
     "cd; curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp")
    (format t "~2& … downloading Quicklisp release-signing key … ~2&")
    (uiop/run-program:run-program
     "cd;curl -o quicklisp.release.key https://beta.quicklisp.org/release-key.txt")
    (format t "~2& … importing signing key … ~2&")
    (uiop/run-program:run-program
     "cd;gpg --import quicklisp.release.key")
    (format t "~2& … verifying ID and signature of signing key … ~2&")
    (uiop/run-program:run-program
     "gpg --fingerprint 028B5FF7 | grep 'Key fingerprint = D7A3 489D DEFE 32B7 D0E7  CC61 3079 65AB 028B 5FF7'")
    (format t "~2& … downloading signature of bootstrap … ~2&")
    (uiop/run-program:run-program
     "cd;curl -o quicklisp.sign.asc https://beta.quicklisp.org/quicklisp.lisp.asc")
    (format t "~2& … verifying signature against key …~2&")
    (uiop/run-program:run-program
     "cd; gpg --verify quicklisp.sign.asc quicklisp.lisp")
    (format t "~2& … All ready. Run installer …")
    (load "~/quicklisp.lisp")
    (handler-case
        (funcall (intern "INSTALL" :quicklisp-quickstart))
      (error (c)
        (declare (ignore c))
        (when (find-restart :load-setup)
          (invoke-restart :load-setup))))
    (format t "~2& Cleaning up.~2&")
    (delete-file "~/quicklisp.lisp")
    (delete-file "~/quicklisp.release.key")
    (delete-file "~/quicklisp.sign.asc")))
