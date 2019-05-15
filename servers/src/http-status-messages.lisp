;;;; -*- lisp -*-
;;;
;;;; ./servers/src/http-status-messages.lisp is part of Tootsville
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

(defvar *http-status-message* (make-hash-table))
(macrolet ((def-http-status (code phrase)
             `(setf (gethash ,code *http-status-message*) ,phrase)))
  (def-http-status 100 "Continue")
  (def-http-status 101 "Switching Protocols")
  (def-http-status 200 "OK")
  (def-http-status 201 "Created")
  (def-http-status 202 "Accepted")
  (def-http-status 203 "Non-Authoritative Information")
  (def-http-status 204 "No Content")
  (def-http-status 205 "Reset Content")
  (def-http-status 206 "Partial Content")
  (def-http-status 207 "Multi-Status")
  (def-http-status 300 "Multiple Choices")
  (def-http-status 301 "Moved Permanently")
  (def-http-status 302 "Moved Temporarily")
  (def-http-status 303 "See Other")
  (def-http-status 304 "Not Modified")
  (def-http-status 305 "Use Proxy")
  (def-http-status 307 "Temporary Redirect")
  (def-http-status 400 "Bad Request")
  (def-http-status 401 "Authorization Required")
  (def-http-status 402 "Payment Required")
  (def-http-status 403 "Forbidden")
  (def-http-status 404 "Not Found")
  (def-http-status 405 "Method Not Allowed")
  (def-http-status 406 "Not Acceptable")
  (def-http-status 407 "Proxy Authentication Required")
  (def-http-status 408 "Request Time-out")
  (def-http-status 409 "Conflict")
  (def-http-status 410 "Gone")
  (def-http-status 411 "Length Required")
  (def-http-status 412 "Precondition Failed")
  (def-http-status 413 "Request Entity Too Large")
  (def-http-status 414 "Request-URI Too Large")
  (def-http-status 415 "Unsupported Media Type")
  (def-http-status 416 "Requested range not satisfiable")
  (def-http-status 417 "Expectation Failed")
  (def-http-status 424 "Failed Dependency")
  (def-http-status 500 "Internal Server Error")
  (def-http-status 501 "Not Implemented")
  (def-http-status 502 "Bad Gateway")
  (def-http-status 503 "Service Unavailable")
  (def-http-status 504 "Gateway Time-out")
  (def-http-status 505 "Version not supported"))
