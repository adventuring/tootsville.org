#+jscl
(eval-when (:compile-toplevel :load-toplevel)
  (unless (find-package :jscl)
    (load (asdf:system-relative-pathname :tootsville
                                         "src/lib/jscl/jscl" :type "lisp"))
    (funcall (intern "BOOTSTRAP-CORE" (find-package "JSCL")))))
#+jscl(defpackage :tootsville.js (:use :jscl :cl))
(in-package :tootsville)
(syntax:use-syntax :annot)

(defparameter *mesh-dir* (asdf:system-relative-pathname :tootsville "js/"))

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
