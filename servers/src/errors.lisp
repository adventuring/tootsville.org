;;;; -*- lisp -*-
;;;
;;;; ./servers/src/errors.lisp is part of Tootsville
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



;;; Rendering a backtrace  … who defines this generic  function? XXX XXX
;;; how is this not CLIM:PRESENT or so?

(defmethod render (bt condition env)
  "☠ deprecated"
  (break "☠deprecated RENDER BACKTRACE CONDITION ENVIRONMENT")
  (let* ((backtrace (parse-backtrace bt)))
    (%to-json
     `(:|error| ,(princ-to-string condition)
        :|condition| ,(condition-name condition)
        :|location| ,(if hunchentoot:*show-lisp-backtraces-p*
                         backtrace
                         (nth 0 backtrace))
        :|slots| ,(slot-values condition)
        :|timestamp| ,(nth 1 backtrace)
        :|env| ,env))))

;;; XXX make sure we don't use this

(defun present-error-to-client (condition env)
  "☠ deprecated, do not use"
  (break "☠deprecated PRESENT-ERROR-TO-CLIENT")
  (let ((backtrace (with-output-to-string (stream)
                     (write-string
                      (print-backtrace condition
                                       :output nil)
                      stream))))
    (list 500 '(:content-type "application/json;charset=utf-8")
          (render-json (list
                        :backtrace backtrace
                        :condition condition
                        :env env)))))

;;; this might actually be TRTTD with RESTAS — or some variation XXX

(defun middleware (app)
  "☠ deprecated"
  (break "☠deprecated MIDDLEWARE")
  (lambda (env)
    (tagbody do-over
       (restart-bind
           ((retry-request
             (lambda ()
               (go do-over))))
         (handler-bind
             ((error (lambda (c) (present-error-to-client c env))))
           (funcall app))))))
