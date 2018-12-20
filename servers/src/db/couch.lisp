 ;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/couch.lisp is part of Tootsville
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

(sdefun connect-mixer ()
        (setf clouchdb:*couchdb*
              (ignore-errors
                (clouchdb:set-connection :host (or (config :mixer :host))
                                         :port (or (config :mixer :port) "5984")
                                         :user (config :mixer :admin :name)
                                         :password (config :mixer :admin :password)
                                         :name "tootsville/5")))
        (v:info :mixer "MOTD from Mixer: ~a"
                (cdr (assoc :|motd| (clouchdb:get-document "motd")))))


(defun find-active-Toot-for-user (&optional (user *user*))
  nil)

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (error 'unimplemented))
