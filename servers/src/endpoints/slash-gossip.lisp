(in-package :Tootsville)

(defendpoint (post "/gossip/offers" "application/sdp")
  "Provide a new offer. Body is an SDP offer. Reply will be an offer URI."
  (let ((body (hunchentoot:raw-post-data)))
    (let ((uuid (gossip-offer body)))
      (v:info '(:gossip :gossip-new) "New SDP offer ~a" uuid)
      (list 202 (list :location (format nil "/gossip/offers/~a" 
                                        (uuid-to-uri uuid)))))))

(defendpoint (get "/gossip/offers" "application/sdp")
  "Ask for any, arbitrary offer to potentially accept."
  (let ((offer (gossip-pop-offer)))
    (if offer
        (list 200
              (list :location (format nil "/gossip/offers/~a"
                                      (uuid-to-uri (gossip-initiation-uuid offer))))
              (getf offer :sdp))
        (error 'not-found :the "Gossipnet initiation offer"))))

(defendpoint (put "/gossip/offers/:uuid64" "application/sdp")
  "Answer a particular offer with ID UUID64"
  (let ((offer (gossip-get-offer (uri-to-uuid uuid64)))
        (body (hunchentoot:raw-post-data)))
    (gossip-answer-offer offer body)))

