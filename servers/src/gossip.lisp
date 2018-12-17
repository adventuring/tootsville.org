(in-package :Tootsville)

(defvar *gossip-offer-queue* (make-instance 'queues:queue))
(defvar *gossip-ringing* (make-hash-table :test 'equal))

(defun gossip-signal-offers (offers)
  (dolist (offer offers)
    (gossip-signal-offer offer)))

(defun gossip-name-for-sdp (sdp)
  (format nil "/gossip/offers/sha="(sha1-hex offer-sdp))) 

(defun gossip-signal-offer (player offer-sdp)
  (queues:qpush (list :player player :sdp offer-sdp :tendered (get-universal-time))
                *gossip-offer-queue*))

(defun gossip-accept-offer ()
  (let* ((ringing (queues:qpop *gossip-offer-queue*))
         (sdp (getf ringing :sdp)))
    (setf (gethash *gossip-ringing* sdp) ringing)
    sdp))

(defun gossip-post-answer (offer-sdp answer-sdp)
  (if-let (offer (gethash *gossip-ringing* offer-sdp))
    (let ((offeror (getf offer :player)))
      (player-alert offeror :gossip :get answer-sdp)
      (remhash *gossip-ringing* offer-sdp))
    (error 'not-found :the "Gossipnet SDP offer")))

