;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoint.lisp is part of Tootsville
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

(defun parse-uri-as-template (uri)
  (let ((parts (split-sequence #\/ uri :remove-empty-subseqs t)))
    (loop for part in parts
       collecting (if (and (< 2 (length part))
                           (char= #\: (char part 0)))
                      (make-keyword (string-upcase (subseq part 1)))
                      part))))

(defclass endpoint ()
  ((function :type symbol
             :initarg :function
             :reader endpoint-function)
   (method :type http-method
           :initarg :method
           :reader endpoint-method)
   (template :type proper-list
             :initarg :template
             :reader endpoint-template)
   (template-arity :type (integer 0 100)
                   :reader endpoint-template-arity)
   (content-type :type symbol
                 :initarg :content-type
                 :reader endpoint-content-type)))

(defvar *endpoints* (make-hash-table))
(defvar *endpoint-list* nil)

(defmethod initialize-instance :after ((endpoint endpoint)
                                       &key template
                                            &allow-other-keys)
  (setf (slot-value endpoint 'template-arity) (length template)))

(defmethod initialize-instance :before ((endpoint endpoint)
                                        &key method uri template content-type)
  (declare (ignore method template content-type))
  (when (stringp uri)
    (setf (slot-value endpoint 'template)
          (parse-uri-as-template uri))))

(defgeneric endpoint-template-string (endpoint)
  (:method ((endpoint endpoint))
    (endpoint-template-string (endpoint-template endpoint)))
  (:method ((template list))
    (format nil "~{/~a~}"
            (or (mapcar (lambda (el) (etypecase el
                                       (symbol (concatenate 'string ":" (symbol-name el)))
                                       (string el)))
                        template)
                '(""))))
  (:method ((uri string))
    (endpoint-template-string (parse-uri-as-template uri))))

(defgeneric endpoint-hash (endpoint-identifier)
  (:method ((endpoint endpoint))
    (sxhash (format nil "~a ~a → ~a"
                    (endpoint-method endpoint)
                    (endpoint-template-string endpoint)
                    (endpoint-content-type endpoint))))
  (:method ((description list))
    (sxhash (format nil "~a ~a → ~a"
                    (string-upcase (first description))
                    (endpoint-template-string (second description))
                    (when-let (content-type (third description))
                      (string-upcase content-type))))))

(defun endpoints-equal (a b)
  (and (eql (endpoint-method a)
            (endpoint-method b))
       (= (endpoint-template-arity a)
          (endpoint-template-arity b))
       (eql (endpoint-content-type a)
            (endpoint-content-type b))
       (equalp (endpoint-template a)
               (endpoint-template b))))

(defmethod add-or-replace-endpoint (function method (uri string) &optional content-type)
  (let ((instance (make-instance 'endpoint
                                 :function function
                                 :method method
                                 :uri uri
                                 :content-type (make-keyword
                                                (etypecase content-type
                                                  (string (string-upcase content-type))
                                                  (symbol (symbol-name content-type)))))))
    (setf (gethash (endpoint-hash instance) *endpoints*) instance)
    (remap-endpoints)))

(defmethod add-or-replace-endpoint (function method (template list) &optional content-type)
  (let ((instance (make-instance 'endpoint
                                 :function function
                                 :method method
                                 :template template
                                 :content-type (make-keyword
                                                (etypecase content-type
                                                  (string (string-upcase content-type))
                                                  (symbol (symbol-name content-type)))))))
    (setf (gethash (endpoint-hash instance) *endpoints*) instance)
    (remap-endpoints)))

(defun remap-endpoints ()
  (setf *endpoint-list*
        (sort (sort (sort (sort (stdutils:hash-values *endpoints*)
                                #'string-lessp
                                :key #'endpoint-method)
                          #'string-lessp
                          :key #'endpoint-content-type)
                    #'<
                    :key #'endpoint-template-arity)
              #'string<
              :key #'endpoint-template-string)))

(defun enumerate-endpoints ()
  (copy-list *endpoint-list*))

(defun clear-all-endpoints ()
  (setf *endpoint-list* nil)
  (clrhash *endpoints*))

(defun endpoint-close (endpoint method arity ua-accept)
  (and (eql (endpoint-method endpoint) method)
       (= (endpoint-template-arity endpoint) arity)
       (member (endpoint-content-type endpoint) ua-accept)))

(defun endpoint-close-key (endpoint)
  (list (endpoint-method endpoint)
        (endpoint-template-arity endpoint)
        (endpoint-content-type endpoint)))

(defun endpoint-kinda-key (endpoint)
  (list (endpoint-method endpoint)
        (endpoint-template-arity endpoint)))

(defun endpoint-template-match (endpoint uri-parts)
  (let ((string-match '#:string-match))
    (block nil
      (cons
       endpoint
       (remove string-match
               (mapcar
                (lambda (a b)
                  (etypecase a
                    (string (if (string= a b)
                                string-match
                                (return)))
                    (symbol b)))
                (endpoint-template endpoint) uri-parts))))))

(defun find-exact-endpoint (method uri-parts ua-accept)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0))
           (type list uri-parts ua-accept)
           (type symbol method))
  #+ (or) (v:info :endpoint "{~a} Look for exact match ~a ~{/~a~} accepting ~s"
                  (current-thread) method uri-parts ua-accept)
  (let* ((arity (length uri-parts))
         (maybes (the proper-list
                      (remove
                       (list method arity ua-accept)
                       *endpoint-list*
                       :test (complement #'equalp)
                       :key #'endpoint-close-key))))
    (dolist (maybe maybes)
      #+ (or) (v:info :endpoint "Looking closer at ~s" maybe)
      (when-let (match (endpoint-template-match maybe uri-parts))
        #+ (or) (v:info :endpoint "Match! Return ~s" match)
        (return-from find-exact-endpoint match)))))

(defun find-kinda-endpoint (method uri-parts)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0))
           (type list uri-parts)
           (type symbol method))
  #+ (or) (v:info :endpoint "{~a} Look for acceptable match ~a ~{/~a~}"
                  (current-thread) method uri-parts)
  (let* ((arity (length uri-parts))
         (maybes (the proper-list
                      (remove
                       (list method arity)
                       *endpoint-list*
                       :test (complement #'equalp)
                       :key #'endpoint-kinda-key))))
    (dolist (maybe maybes)
      #+ (or) (v:info :endpoint "Looking closer at ~s" maybe)
      (when-let (match (endpoint-template-match maybe uri-parts))
        #+ (or) (v:info :endpoint "Match! Return ~s" match)
        (return-from find-kinda-endpoint match)))))

(defun find-best-endpoint (method uri-parts ua-accept)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0))
           (type list uri-parts ua-accept)
           (type symbol method))
  (let ((tail (lastcar uri-parts)))
    (or (when (and (stringp tail)
                   (find #\. tail))
          (v:info :endpoint "Remap extension and reconsider ~s" uri-parts)
          (find-best-endpoint method (append (butlast uri-parts)
                                             (split-sequence #\. tail))
                              ua-accept))
        (find-exact-endpoint method uri-parts ua-accept)
        (find-kinda-endpoint method uri-parts))))

(defmethod print-object ((endpoint endpoint) s)
  (print-unreadable-object (endpoint s :type t)
    (princ (endpoint-method endpoint) s)
    (princ " " s)
    (princ (endpoint-template-string endpoint) s)
    (princ " → " s)
    (princ (endpoint-content-type endpoint) s)
    (princ " ← " s)
    (princ (endpoint-function endpoint) s)))
