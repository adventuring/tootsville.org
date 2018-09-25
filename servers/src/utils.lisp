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




;;; Types


(defun two-chars-in-a-row-p (string char-bag)
  "Do any two characters in CHAR-BAG occur together in STRING?"
  (check-type string string)
  (check-type char-bag sequence)
  (loop for i from 1 below (length string)
     when (and (find (char string i) char-bag)
               (find (char string (1- i)) char-bag))
     do (return-from two-chars-in-a-row-p i))
  nil)

(defun three-chars-in-a-row-p (string &optional char-bag)
  "Do any three characters in CHAR-BAG occur together in STRING?

If CHAR-BAG is NIL, then any  character that occurs three times matching
itself returns true."
  (check-type string string)
  (check-type char-bag (or null sequence))
  (unless (<= 3 (length string))
    (return-from three-chars-in-a-row-p nil))
  (if char-bag
      (progn
        (assert (every #'characterp char-bag))
        (loop for i from 2 below (length string)
           when (and (find (char string i) char-bag)
                     (find (char string (1- i)) char-bag)
                     (find (char string (- i 2)) char-bag))
           do (return-from three-chars-in-a-row-p i)))
      (loop for i from 2 below (length string)
         when (char= (char string i) 
                     (char string (- i 1))
                     (char string (- i 2)))
         do (return-from three-chars-in-a-row-p i)))
  nil)




;;; UUID  interation with  the database.  We handle  them internally  as
;;; (UNSIGNED-BYTE 128), and into MariaDB  as a BINARY(16), but may want
;;; to also sometimes use UUID objects directly (eg, to generate them).

(defun binary<-uuid (uuid)
  "Return a single (UNSIGNED-BYTE 128) representing UUID"
  (check-type uuid uuid:uuid)
  (let ((binary 0))
    (loop with byte-array = (uuid:uuid-to-byte-array uuid)
       for index from 0 upto 15
       for byte = (aref byte-array index)
       do (setf binary (dpb byte (byte 8 (* 8 index)) binary)))
    binary))

(defun uuid<-binary (integer)
  "Convert an (UNSIGNED-BYTE 128) into a UUID"
  (check-type integer (unsigned-byte 128))
  (let ((byte-array (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for index from 0 upto 15
       for byte = (ldb (byte 8 (* 8 index)) integer)
       do (setf (aref byte-array index) byte))
    (uuid:byte-array-to-uuid byte-array)))

(defun uuid-string (uuid)
  (etypecase uuid
    ((unsigned-byte 128)
     (format nil "{~8,0x-~4,0x-~4,0x-~4,0x-~12,0x}"
             (ldb (byte (* 8 4) 0) uuid)
             (ldb (byte (* 4 4) (* 8 4)) uuid)
             (ldb (byte (* 4 4) (* 12 4)) uuid)
             (ldb (byte (* 4 4) (* 16 4)) uuid)
             (ldb (byte (* 12 4) (* 20 4)) uuid)))
    (uuid:uuid (uuid-string (binary<-uuid uuid)))))

(assert
 (let ((uuid (uuid:make-v4-uuid)))
   (uuid:uuid= uuid (uuid<-binary (binary<-uuid uuid)))))

