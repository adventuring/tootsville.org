;;;; -*- lisp -*-
;;;
;;;; ./servers/src/types/binary.lisp is part of Tootsville
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

(in-package :Tootsville)

(defun integer-to-byte-vector
    (integer
     &optional
       (vector (make-array (ceiling (integer-length integer) 8)
                           :element-type '(unsigned-byte 8))))
  "Convert INTEGER into VECTOR of (UNSIGNED-BYTE 8)

If VECTOR is  supplied, it must be lon enough  to accept INTEGER without
growing. Otherwise,  the vector  of the minimum  length to  hold INTEGER
will be constructed.

The byte vector will be in big-endian (aka \"network\") order."
  (assert (<= (ceiling (integer-length integer) 8)
              (length vector))
          (integer vector)
          "INTEGER-TO-BYTE-VECTOR: ~
integer ~x is longer than vector length ~:d byte~p"
          integer (length vector))
  (let ((i8 (* 8 (1- (length vector)))))
    (dotimes (i (length vector))
      (setf (aref vector i) (ldb (byte 8 i8) integer))
      (decf i8 8)))
  vector)

(defun byte-vector-to-integer (vector)
  "Convert VECTOR of (UNSIGNED-BYTE 8) into an integer.

The VECTOR should be in big-endian (aka \"network\") order."
  (let ((i8 (* 8 (1- (length vector)))) (integer 0))
    (dotimes (i (length vector))
      (setf (ldb (byte 8 i8) integer) (aref vector i))
      (decf i8 8))
    integer))



(defun uri-to-uuid (uuid)
  "Extract a UUID encoded in Base64 in URI form"
  (uuid:byte-array-to-uuid (cl-base64:base64-string-to-usb8-array
                            (substitute #\/ #\- uuid))))

(defun uuid-to-uri (uuid)
  "Encode UUID in Base64 and escape for URIs.

Swaps / characters for - characters to be more polite in an URI."
  (substitute #\- #\/ (cl-base64:usb8-array-to-base64-string
                       (uuid:uuid-to-byte-array uuid))))



(defun sha1-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1
                             (trivial-utf-8:string-to-utf-8-bytes string))))

(defun ensure-integer (value)
  (etypecase value
    (integer value)
    (real (round value))
    (string (parse-integer value))))

(defun ensure-number (value)
  (etypecase value
    (number value)
    (string (parse-number value))))
