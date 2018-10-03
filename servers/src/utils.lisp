(cl:in-package :cl-user)
(defpackage org.star-hope.utils
  (:use :cl :alexandria)
  (:export
   
   #:stringify
   #:extract
   #:extreme
   #:human-duration
   #:range-size
   #:year<-universal-time
   #:file-write-year
   #:map-asdf-files
   #:two-chars-in-a-row-p
   #:three-chars-in-a-row-p
   
   ))
(in-package :org.star-hope.utils)



;;; Painfully simple function-compositions

(defun stringify (object)
  (format nil "~a" object))

(defun extreme (function list)
  (reduce (lambda (a b)
            (if (funcall function a b) a b))
          list))


;;; Human-friendly formats in and output

(defun range-size (numeric-range-string)
  "Count the length of a range of numbers separated by -"
  (if (find #\- numeric-range-string)
      (destructuring-bind (start end) 
          (uiop:split-string numeric-range-string
                             :separator "-")
        (1+ (- (parse-integer end) (parse-integer start))))
      1))

(defun human-duration (seconds)
  (cond
    ((< seconds 90)
     (format nil "~d second~:p" seconds))
    ((< seconds (* 90 60))
     (format nil "~d minutes" (round seconds 60)))
    ((< seconds (* 3 24 60 60))
     (format nil "~d hours" (round seconds (* 60 60))))
    ((< seconds (* 6 7 24 60 60))
     (format nil "~d days" (round seconds (* 24 60 60))))
    ((< seconds (* 75 7 24 60 60))
     (format nil "~d weeks" (round seconds (* 7 24 60 60))))
    (t (format nil "~d years" (round seconds (* 365.2489 24 60 60))))))



;;; Time handling.

(defun days-ago (days)
  "Return a time that is DAYS days in the past."
  (local-time:timestamp- (local-time:now) days :day))

(defun yesterday ()
  "Get the same time as now, but yesterday.

See: `DAYS-AGO'"
  (days-ago 1))

(defun 2-days-ago ()
  "Two days ago.

See: `DAYS-AGO'"
  (days-ago 2))

(defun 3-days-ago ()
  "Three days ago.

See: `DAYS-AGO'"
  (days-ago 3))

(defun header-time (&optional (time (get-universal-time)))
  "Format TIME (or now) as an RFC-1123 timestring for HTTP headers.

Accepts either a LOCAL-TIME:TIMESTAMP or NUMBER of Universal Time."
  (local-time:format-rfc1123-timestring
   nil
   (etypecase time
     (number (local-time:universal-to-timestamp time))
     (local-time:timestamp time))))

(defun year<-universal-time (time)
  (nth-value 5 (decode-universal-time time)))

(defun file-write-year (file)
  (or (year<-universal-time (file-write-date file))
      0))



(defun map-asdf-files (function module)
  (check-type function function)
  (check-type module asdf/component:module)
  (mapcan (lambda (child)
            (etypecase child
              (asdf/component:module (map-asdf-files function child))
              (asdf/component:file-component
               (list (funcall function
                              (slot-value child 'asdf/component::absolute-pathname))))))
          (asdf:component-children module)))



;;; Obscure compiler features?

#+sbcl
(sb-alien:define-alien-routine ("disable_lossage_handler" disable-sbcl-ldb)
    sb-alien:void)
#+sbcl
(sb-alien:define-alien-routine ("enable_lossage_handler" enable-sbcl-ldb)
                               sb-alien:void)

#-sbcl
(defun disable-sbcl-ldb ())
#-sbcl
(defun enable-sbcl-ldb ())
