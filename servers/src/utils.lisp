(cl:in-package :cl-user)
(defpackage org.star-hope.utils
  (:use :cl :alexandria)
  (:export
   
   #:stringify
   #:extreme
   #:human-duration
   #:range-size
   #:year<-universal-time
   #:file-write-year
   #:map-asdf-files
   #:dns-name
   #:www-uri
   
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




;;; Types


(defun host-name-char-p (char)
  (check-type char character)
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (char<= #\0 char #\9)
      (char= #\. char)
      (char= #\- char)))

(defun two-chars-in-a-row (string char-bag)
  "Do any two characters in CHAR-BAG occur together in STRING?"
  (check-type string string)
  (check-type char-bag sequence)
  (loop for i from 1 below (length string)
     when (and (find (char string i) char-bag)
               (find (char string (1- i)) char-bag))
     do (return-from two-chars-in-a-row t))
  nil)

(defun host-name-like-p (name)
  (check-type name string)
  (and (every #'host-name-char-p name)
       (not (char= #\- (char name 0)))
       (not (char= #\- (char name (1- (length name)))))
       (not (two-chars-in-a-row name ".-"))
       (let ((parts (uiop:split-string name ".")))
         (every #'alpha-char-p (last parts))
         (<= 2 (length (last parts))))))

(defun www-uri-like-p (uri)
  (check-type uri string)
  (and (<= 3 (count #\/ uri))
       (destructuring-bind (method _ host+port)
           (uiop:split-string uri :separator "/" :max 3)
         (and (member method '("http:" "https:") :test #'string=)
              (emptyp _)
              (host-name-like-p (subseq host+port
                                        0
                                        (position #\: host+port)))))))

(deftype dns-name ()
  '(and string (satisfies host-name-like-p)))

(deftype www-uri ()
  '(and string (satisfies www-uri-like-p)))
