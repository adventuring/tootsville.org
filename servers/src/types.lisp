(in-package :tootsville)

(defun potential-toot-name-character-p (c)
  (and (characterp c)
       (or (alphanumericp c)
           (char= #\- c)
           (char= #\' c)
           (char= #\space c))))

(defun potential-toot-name-p (toot-name)
  (and (stringp toot-name)
       (<= 3 (length toot-name) 64)
       (every #'potential-toot-name-character-p
              toot-name)
       (alpha-char-p (char toot-name 0))))

(deftype toot-name ()
  `(and string (satisfies potential-toot-name-p)))
