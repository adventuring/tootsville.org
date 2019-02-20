;;;; -*- lisp -*-
;;;
;;;; ./servers/src/gossip.lisp is part of Tootsville
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



(defun gossip-initiation-uri (initiation)
  (etypecase initiation
    (uuid:uuid (format nil "/tootsville/gossip-exchange/~a" (uuid-to-uri initiation)))
    (gossip-initiation (gossip-initiation-uri
                        (gossip-initiation-uuid initiation)))))


(defun gossip-offer (sdp &optional (user *user*))
  (let ((init (make-gossip-initiation :uuid (uuid-to-uri (uuid:make-v1-uuid))
                                      :offeror user :offer sdp)))
    (save-record init)))

(defun gossip-pop-offer (&optional (user *user*))
  (let ((offers (find-records 'gossip-initiation :answeror nil)))
    (when offers
      (let ((offer (first offers)))
        (setf (gossip-initiation-answeror offer) (uuid-to-uri (person-uuid user)))
        (save-record offer)))))

(defun gossip-answer-offer (offer sdp &optional (user *user*))
  (assert (or (and (null (gossip-initiation-answeror offer))
                   (setf (gossip-initiation-answeror offer) (person-uuid user)))
              (equal (gossip-initiation-answeror offer)
                     (person-uuid user))))
  (setf (gossip-initiation-answer offer) sdp))

(defvar *ice-credentials* nil)

(defun ice-credentials ()
  (or (unless (zerop (random 100))
        *ice-credentials*)
      (setf *ice-credentials*
            (fetch-ice-credentials/xirsys))))

(defun fetch-ice-credentials/xirsys ()
  (let ((s (map 'string #'code-char
                (drakma:http-request 
                 (format nil "https://global.xirsys.net/_turn/~a"
                         (config :xirsys :channel))
                 :method :put
                 :basic-authorization 
                 (list (config :xirsys :username)
                       (config :xirsys :password))))))
    (let ((json (jonathan.decode:parse s)))
      (unless (equal "ok" (extract json :|s|))
        (sleep (random 5))
        (return-from fetch-ice-credentials/xirsys (fetch-ice-credentials/xirsys)))
      (extract json :|v| :|iceServers|))))
