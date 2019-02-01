;;;; -*- lisp -*-
;;;
;;;; ./servers/src/script.lisp is part of Tootsville
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

#+jscl
(eval-when (:compile-toplevel :load-toplevel)
  (unless (find-package :jscl)
    (load (asdf:system-relative-pathname :Tootsville
                                         "src/lib/jscl/jscl" :type "lisp"))
    (funcall (intern "BOOTSTRAP-CORE" (find-package "JSCL")))))
#+jscl(defpackage :Tootsville.js (:use :jscl :cl))
(in-package :Tootsville)
(syntax:use-syntax :annot)

(defparameter *mesh-dir* (asdf:system-relative-pathname :Tootsville "js/"))

(defun mesh.js-pathname ()
  (make-pathname :name "mesh" :type "js" :version :newest
                 :directory (pathname-directory *mesh-dir*)))

(defun join-lines (a b)
  (concatenate 'string a #(#\Newline) b))

(defparameter *uglifyp* nil)
(defparameter *prettyp* t)

(defun uglify-with-parameters (js)
  (if (or *uglifyp*
          *prettyp*)
      (cl-uglify-js:ast-gen-code
       (funcall
        (if *uglifyp*
            #'cl-uglify-js:ast-mangle
            #'identity)
        (cl-uglify-js:ast-squeeze
         (parse-js:parse-js js :ecma-version 5)
         :sequences t :dead-code t))
       :beautify *prettyp*)
      js))

(defun uglify (js)
  (let ((js (etypecase js
              (stream (read-file-into-string js :external-format :utf-8))
              (string js))))
    (format *terminal-io* "~&Uglifying: source is ~:d chars;" (length js))
    (let ((outcome (uglify-with-parameters js)))
      (progn
        (format *terminal-io* "~&Uglifying: outcome is ~:d chars;" (length outcome))
        outcome))))

(defun print-error-as-js-comment (c source-path)
  (format nil "

/* —————————— */
console.log(\"Compile-time error: ~a in …>jscl>~a\");
/*
~a
 * —————————— */

"
          (type-of c) (pathname-name source-path) c))
