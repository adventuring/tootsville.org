;;;; -*- lisp -*-
;;;
;;;; ./servers/src/to-json.lisp is part of Tootsville
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



(defmethod %to-json ((uuid uuid:uuid))
  (jonathan.encode:%write-string (format nil "\"~a\"" uuid)))

(defmethod %to-json ((color24 color24))
  (jonathan.encode:%write-string (format nil "\"~a\"" (color24-name color24))))

(defmethod %to-json ((timestamp timestamp))
  (jonathan.encode:%write-string (princ-to-string (timestamp-to-unix timestamp))))

(defmethod %to-json ((object t))
  "Return a JSON object that represents the state of OBJECT"
  (%to-json
   `(:|isA| ,(string-capitalize (type-of object))
      :t ,(format nil "~s" object))))

(defmethod %to-json ((object structure-object))
  "Return a JSON object that represents the state of structure-object OBJECT"
  (%to-json
   `(:|isA| ,(string-capitalize (class-name (class-of object)))
      ,@(loop for slot in (mapcar #'closer-mop:slot-definition-name
                                  (closer-mop:class-slots (class-of object)))
           collect (make-keyword (symbol-munger:lisp->camel-case slot))
           collect (slot-value object slot)))))

(defmethod %to-json ((object standard-object))
  "Return a JSON object that represents the state of standard-object OBJECT"
  (%to-json
   `(:|isA| ,(string-capitalize (class-name (class-of object)))
      ,@(loop for slot in (mapcar #'closer-mop:slot-definition-name
                                  (closer-mop:class-slots (class-of object)))
           collect (make-keyword (symbol-munger:lisp->camel-case slot))
           collect (slot-value object slot)))))

(defmethod %to-json ((function function))
  "Encode FUNCTION as a JSON object."
  ;; TODO try to extract its source tree and pass it along, as well.
  (let ((name (nth-value 2 (function-lambda-expression function))))
    (%to-json
     `(:|isA| "function"
        :|package| ,(string-upcase (package-name (symbol-package name)))
        :|name| ,(string-upcase (symbol-name name))
        :|source| ,(function-lambda-expression function)))))

(defmethod %to-json ((pathname pathname))
  "Encode PATHNAME as a JSON object"
  (%to-json
   `(:|isA| "pathname"
      :|host| ,(typecase (pathname-host pathname)
                 #+sbcl (sb-impl::unix-host (machine-instance))
                 (t (princ-to-string (pathname-host pathname))))
      :|device| ,(pathname-device pathname)
      :|directory| ,(uiop:split-string (pathname-directory pathname)
                                       :separator "/")
      :|name| ,(pathname-name pathname)
      :|version| ,(pathname-version pathname)
      :|type| ,(pathname-type pathname))))

(defmethod jonathan::%to-json ((symbol symbol))
  "Supply a Lisp symbol in JavaScirpt string form"
  (jonathan.encode::string-to-json (string (symbol-munger:lisp->camel-case symbol))))
