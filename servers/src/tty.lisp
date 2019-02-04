;;;; -*- lisp -*-
;;;
;;;; ./servers/src/tty.lisp is part of Tootsville
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

(defpackage org.tootsville.tty
  (:use :cl)
  (:export
   #:set-ansi-color
   #:set-xterm-icon
   #:set-xterm-title
   #:xtermp
   #:with-ansi-color
   #:with-xterm-icon
   #:with-xterm-title
   ))

(in-package #:org.tootsville.tty)

(defvar colors-by-name
  '(:black :red :green :yellow :blue :magenta :cyan :light-gray
    :dark-gray :light-red :light-green :light-yellow
    :light-blue :light-magenta :light-cyan :white))

(deftype color-designator ()
  '(or number (member colors-by-name)))

;; from https://stackoverflow.com/a/41645393/475150

(defun rbgi-to-rgb (rgbi)
  (declare (type (bit-vector 4) rgbi))
  (let ((r (bit rgbi 0))
        (g (bit rgbi 1))
        (b (bit rgbi 2))
        (i (bit rgbi 3))
        (rgb (make-array 3 :element-type 'single-float)))
    (setf (elt rgb 0) (* #.(float 1/3) (+ i (* 2 r)))
          (elt rgb 1) (* #.(float 1/3) (+ i (* 2 g)))
          (elt rgb 2) (* #.(float 1/3) (+ i (* 2 b))))
    rgb))

(defun rgbx-approx (rgb &key lightp)
  (destructuring-bind (red green blue) rgb
    (declare (type single-float red green blue))
    (let ((threshold (if lightp
                         (/ 2.0 3.0)
                         (/ 1.0 3.0))))
      (mapcar (lambda (channel)
                (if (> (the single-float channel) threshold)
                    1.0 0.0))
              (list red green blue)))))

(defun squared-color-distance (rgb-a rgb-b)
  (reduce #'+
          (mapcar (lambda (x)
                    (declare (type single-float x))
                    (float (* x x)))
                  (mapcar (lambda (a b)
                            (declare (type single-float a b))
                            (float (- a b)))
                          rgb-a rgb-b))))

(defun rgbi-integer (rgbi)
  (destructuring-bind (r g b i) rgbi
    (logior (the bit r)
            (ash 1 (the bit g))
            (ash 2 (the bit b))
            (ash 3 (the bit i)))))

(defun rgb-to-rgbi (rgb)
  (rgbi-integer
   (let ((dark (rgbx-approx rgb :lightp nil))
         (light (rgbx-approx rgb :lightp t)))
     (if (< (squared-color-distance rgb (rgbi-to-rgb dark))
            (squared-color-distance rgb (rgbi-to-rgb light)))
         dark
         light))))

(defun color-to-ansi (color)
  (cond
    ((or (symbolp color) (stringp color))
     (let ((i (position color (the list colors-by-name)
                        :test #'string-equal)))
       (if i
           (+ 30 i)
           (error "color not found by name: ~a" color))))
    ((and (or (listp color) (arrayp color))
          (= 3 (length color)))
     (+ 30 (rgb-to-rgbi color)))))

(defun set-ansi-color (&key (stream *standard-output*)
                            (color nil color-kp)
                            (background-color nil background-kp)
                            (boldp nil boldp-kp)
                            (dimp nil dimp-kp)
                            (underlinep nil underlinep-kp)
                            (italicp nil italicp-kp)
                            (reversep nil reversep-kp)
                            (hiddenp nil hiddenp-kp)
                            (strekiethroughp nil strikethroughp-kp)
                            (ansi-sequence nil ansi-sequence-kp))
  (check-type color (or null color-designator))
  (check-type background-color (or null color-designator))
  (check-type ansi-sequence (or null string))
  (when (and ansi-sequence-kp
             (or color-kp background-kp
                 boldp-kp dimp
                 underlinep-kp italicp-kp strikethroughp-kp
                 reversep-kp hiddenp-kp))
    (warn "ANSI-Sequence provided alongside ~:(~{~a~^, ~}~)"
          (mapcar (lambda (symbol)
                    (let ((name (symbol-name symbol)))
                      (subseq name
                              0 (position #\- name :from-end t))))
                  (remove-if #'null
                             '(color-kp background-kp
                               boldp-kp underlinep-kp italicp-kp)
                             :key #'symbol-value))))
  (format stream "[~{~a~^;~}m"
          (remove-if #'null
                     (when color-kp
                       (color-to-ansi color))
                     (when background-kp
                       (+ 10 (color-to-ansi color)))
                     (when boldp-kp
                       (princ "[~:[2~;~]1m" boldp))
                     (when dimp-kp
                       (princ "[~:[2~;~]2m" dimp))
                     (when underlinep-kp
                       (princ "[~:[2~;~]4m" underlinep))
                     (when reversep-kp
                       (princ "[~:[2~;~]7m" reversep))
                     (when hiddenp-kp
                       (princ "[~:[2~;~]8m" hiddenp))))
  (finish-output stream))

(defmacro with-ansi-color ((&rest keys
                                  &key stream
                                  color background-color
                                  boldp underlinep italicp)
                           &body body)
  (let ((saved-color (gensym "SAVED-COLOR-")))
    `(let (,saved-color)
       (unwind-protect
            (progn (when (xtermp)
                     (setf ,saved-color (get-ansi-color))
                     (set-ansi-color ,@keys))
                   ,@body
                   (when (xtermp)
                     (set-ansi-color ,saved-color)))))))

(defun set-xterm-title (title &optional (stream *standard-output*))
  (when (xtermp)
    (assert (not (find #\Bell title)))
    (format stream "1;~a")))
