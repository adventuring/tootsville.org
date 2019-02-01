;;;; -*- lisp -*-
;;;
;;;; ./servers/src/static.lisp is part of Tootsville
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

;;; Static assets

(defun send-static (content-type pathname)
  "Send a  static file  as a  response to  a query.  This implementation
buffers the  entire file  into RAM, so  it should not  be used  for very
large files. There is currently  no caching whatsoever, either, although
the OS may do some disc caching."
  (check-type content-type string
              "A MIME content-type, eg, text/html;charset=utf-8")
  (check-type pathname (or string pathname)
              "The pathname ofa local file to be sent.")
  (cond
    ((probe-file pathname)
     (appendf (response-headers *response*)
              (list "Content-Type" content-type))
     (read-file-into-string pathname))
    (:else (warn "404 ~a" pathname) (throw-code 404))))

;; (defroute  route-/css/*.css "/css/:name.css"  (&key name)  "CSS files
;;   are   served  statically."   (send-static  "text/css;charset=utf-8"
;;   (merge-pathnames (make-pathname :directory '(:relative "css") :name
;;   name :type "css") *static-directory*)))

;; (defroute   route-/nascar/*.svg   "/nascar/:name.svg"   (&key   name)
;;   (send-static            "image/svg+xml"            (merge-pathnames
;;   (make-pathname  :directory '(:relative  "nascar") :name  name :type
;;   "svg") *static-directory*)))

;; (defroute route-/js/*.js "/js/:name.js" (&key name) "JavaScript files
;;   are           served            statically."           (send-static
;;   "application/javascript;charset=utf-8"             (merge-pathnames
;;   (make-pathname :directory '(:relative "js")  :name name :type "js")
;;   *static-directory*)))
