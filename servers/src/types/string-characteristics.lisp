(in-package :Tootsville)

(defun string-length-2-p (s)
  (check-type s string)
  (equal 2 (length s)))

(defun string-all-alpha-chars-p (s)
  (check-type s string)
  (every #'alpha-char-p s))

(deftype two-letter-string ()
  '(and string
    (satisfies string-length-2-p)
    (satisfies string-all-alpha-chars-p)))



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
