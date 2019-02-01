;;;; -*- lisp -*-
;;;
;;;; ./mesh/src/XHR.lisp is part of Tootsville
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

(in-package :xhr)

(defun network-error (condition)
  (#j:alert "A network error occurred. Is the network disconnected?"))

(defun xhr-report-errors (xhr)
  (setf (oget xhr "onerror")
        (lambda (condition)
          (network-error condition))))

(defun server-url (suffix)
  (concatenate 'string (getf (getf *game-state* :server-info) :url) suffix))

(defun xhr-set-up-post (xhr uri on-load)
  ((oget xhr "open") "PUT" (server-url uri))
  ((oget xhr "setRequestHeader") "Accept" "application/json")
  ((oget xhr "setRequestHeader") "Content-Type" "application/x-www-form-urlencoded")
  (setf (oget xhr "onload") on-load)
  (xhr-report-errors xhr))

(defun get (uri callback &optional headers)
  (error "No get"))

(defmacro with-get ((uri response &optional headers)
                    &body body)
  `(get ,uri (lambda (,response) ,@body nil) ,headers))

(defun post (uri callback &optional fields headers)
  (error "No post"))

(defmacro with-post ((uri response &optional fields headers)
                     &body body)
  `(post ,uri (lambda (,response) ,@body nil) ,fields ,headers))
