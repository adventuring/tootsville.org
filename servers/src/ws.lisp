;;;; -*- lisp -*-
;;;
;;;; ./servers/src/ws.lisp is part of Tootsville
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

(defclass gossip-channel (hunchensocket:websocket-resource)
  ((offer-id :initarg :offer-id
             :initform (uuid:make-v4-uuid)
             :reader gossip-offer-id)))

#+ (or) (defvar *gossip-channels*
          (queues:make-queue :simple-cqueue :minimum-size 16))

#+ (or) (defun ws-get-gossip-channel (request)
          (queues:qpop *gossip-channels*))

(defun ws-broadcast (channel sender message)
  (dolist (peer (hunchensocket:clients channel))
    (unless (eql peer sender)
      (hunchensocket:send-text-message channel message))))

(defmethod hunchensocket:text-message-received ((channel gossip-channel)
                                                sender message)
  (ws-broadcast channel sender message))

(defparameter *ws-server*
  (make-instance 'hunchensocket:websocket-acceptor :port :2774))
