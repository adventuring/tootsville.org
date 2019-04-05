;;;; -*- lisp -*-
;;;
;;;; ./servers/src/errors.lisp is part of Tootsville
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


;;; Convenience functions for examining conditions/errors

(defun condition-name (condition)
  "Returns the capitalized name of the class of CONDITION."
  (string-capitalize (symbol-name (class-name (class-of condition)))))

(defun condition-slots (object)
  "Enumerates the name of every slot on OBJECT"
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of object))))

(defun slot-values (object)
  "For any OBJECT, this returns a list; each element is a PList with a slot
 name and value, encoded in JSON."
  (loop for slot in (condition-slots object)
     collecting
       (list :slot (symbol-name slot)
             :value (%to-json (slot-value object slot)))))


;;; Backtrace handling

(defparameter +backtrace-regex+ "\\n\\w*\\d+:"
  "A regular expression to split backtraces")

(defun split-backtrace (str)
  "Split a string backtrace into parts"
  (ppcre:split +backtrace-regex+ str))

(defun parse-backtrace (bt)
  "Break lines of a backtrace into error messag, date/time, and call frames
 (stack)"
  (destructuring-bind (header &rest frames) (split-backtrace bt)
    (let ((error-msg (subseq header
                             (position #\: header :from-end t)))
          (date-time (subseq header
                             (1+ (position #\: header))
                             (position #\A header))))
      (list error-msg date-time frames))))
