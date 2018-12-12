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
