;;;; -*- lisp -*-
;;;
;;;; ./servers/src/types/toot-names.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2016,2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

;;; Toot names

(defun remove-repeats-for-Toot-name (string)
  (regex-replace-all "--+"
                     (regex-replace-all "(.)\\1\\1+" string "\\1")
                     "-"))

(defun check-Toot-name (name)
  "Check if NAME is allowed as a Toot name; offering restarts to correct it,
 if not.

This  is  generally  intended  for  accepting  new  Toot  names,  versus
validating REST calls, for example."
  (tagbody do-over
     (restart-bind
         ((auto-rename
           (lambda ()
             (let ((try (remove-repeats-for-Toot-name
                         (substitute-if #\- (complement #'alpha-char-p) name))))
               (when (char= #\- (first-elt try))
                 (setf try (subseq try 1)))
               (when (char= #\- (last-elt try))
                 (setf try (subseq try 0 (- (length try) 2))))
               (when (< (length try) 3)
                 (setf try (concatenate 'string try "-a")))
               (when (< 32 (length try))
                 (setf try (subseq try 0 32)))
               try))
            :report-function
            (lambda (s)
              (format s "Find a name similar to ~a"
                      name)))
          (provide-new-name
           (lambda (new-name)
             (setf name new-name)
             (go do-over))
            :report-function
            (lambda (s) (format s "Supply a new name"))))
       (check-type name toot-name))))

(define-memo-function potential-Toot-name-character-p (character)
  "Is CHARACTER allowed in a Toot name at all?

Allowed characters are alphanumerics, apostrophe, hyphen, or space, but
there are additional rules in `POTENTIAL-TOOT-NAME-P' which limit
the string as a whole."
  (and (characterp character)
       (or (alphanumericp character)
           (char= #\- character)
           (char= #\' character)
           (char= #\space character))))

(defun potential-Toot-name-p (Toot-name)
  "Could TOOT-NAME be allowed as a Toot name?

Toot names must be:

@itemize

@item
From three to 32 characters in length, inclusive.

@item
Characters must be  `POTENTIAL-TOOT-NAME-CHARACTER-P', ie, alphanumeric,
a space, apostrophe, or hyphen.

@item
The first character must be alphabetic

@item
There can not be two punctuation marks (or spaces) in a row

@item
There can not be three of the same character in a row

@item
There can not be more than three digits

@item
Digits must appear only at the end -- i.e., if there are any digits, the
leftmost digit must be after the rightmost non-digit character.

@end itemize"
  (and (stringp Toot-name)
       (<= 3 (length Toot-name) 32)
       (every #'potential-Toot-name-character-p
              Toot-name)
       (alpha-char-p (char Toot-name 0))
       (not (three-chars-in-a-row-p Toot-name))
       (not (two-chars-in-a-row-p Toot-name #(#\Space #\Apostrophe
                                              #\Hyphen-Minus)))
       (or (notany #'digit-char-p Toot-name)
           (< (position-if (complement #'digit-char-p) Toot-name :from-end t)
              (position-if #'digit-char-p Toot-name)))
       (< (count-if #'digit-char-p Toot-name) 4)))

(deftype Toot-name ()
  `(and string (satisfies potential-Toot-name-p)))
