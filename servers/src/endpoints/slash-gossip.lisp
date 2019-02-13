;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/slash-gossip.lisp is part of Tootsville
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

(defun make-offers-from-json (json)
  (let (offers)
    (dolist (offer (coerce (getf json :|offers|) 'list))
      (let* ((offer-object (make-record 'gossip-initiation
                                        :offeror *user*
                                        :offer offer)))
        (save-record offer-object)
        (v:info '(:gossip :gossip-new) 
                "New SDP offer ~a" (gossip-initiation-uuid offer-object))
        (push (gossip-initiation-uuid offer-object) offers)))
    offers))

(defendpoint (get "/gossip/ice-servers" "application/json")
  "Obtain STUN/TURN server credentials for ICE"
  (with-user ()
    (list 200 ()
          (ice-credentials))))

(defendpoint (post "/gossip/offers" "application/json")
  "Provide a new offer. Body is an SDP offer. Reply will be an offer URI."
  (with-user ()
    (let* ((json$ (map 'string 'code-char (hunchentoot:raw-post-data)))
           (json (jonathan.decode:parse json$))
           (offers (make-offers-from-json json)))
      (list 202 (list :location "/gossip/offers")
            (list :|offers| (mapcar #'uuid-to-uri offers))))))

(defendpoint (get "/gossip/offers/any" "application/sdp")
  "Ask for any, arbitrary offer to potentially accept."
  (let ((offer (gossip-pop-offer)))
    (if offer
        (list 200
              (list :location (format nil "/gossip/offers/~a"
                                      (uuid-to-uri (gossip-initiation-uuid offer))))
              (gossip-initiation-offer offer))
        (error 'not-found :the "Gossipnet initiation offer"))))

(defendpoint (put "/gossip/offers/:uuid64" "application/sdp")
  "Answer a particular offer with ID UUID64"
  (let ((offer (find-record 'gossip-initiation :uuid (uri-to-uuid uuid64)))
        (body (hunchentoot:raw-post-data)))
    (gossip-answer-offer offer body)))
