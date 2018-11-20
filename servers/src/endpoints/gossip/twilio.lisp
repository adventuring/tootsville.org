(in-package :Tootsville)

(defmacro with-twilio-params (() &body body)
  `(let ((message-sid (hunchentoot:parameter "MessageSid"))
         (account-sid (hunchentoot:parameter "AccountSid"))
         (messaging-service-sid (hunchentoot:parameter "MessagingServiceSid"))
         (message-from (hunchentoot:parameter "From"))
         (message-to (hunchentoot:parameter "To"))
         (message-body (hunchentoot:parameter "Body"))
         (message-from-place (list :city (hunchentoot:parameter "FromCity")
                                   :state (hunchentoot:parameter "FromState")
                                   :postal-code (hunchentoot:parameter "FromZip")
                                   :country (hunchentoot:parameter "FromCountry")))
         (message-to-place (list :city (hunchentoot:parameter "ToCity")
                                 :state (hunchentoot:parameter "ToState")
                                 :postal-code (hunchentoot:parameter "ToZip")
                                 :country (hunchentoot:parameter "ToCountry")))
         message-media-type 
         message-media-url)
     (let ((num-media (parse-integer (hunchentoot:parameter "NumMedia"))))
       (when (< 0 num-media)
         (loop for i below num-media
            do (push (hunchentoot:parameter (format nil "MediaContentType~D" i))
                     message-media-type)
            do (push (hunchentoot:parameter (format nil "MediaUrl~D" i))
                     message-media-url))))))



(defendpoint (post "/gossip/twilio/incoming/call" "text/xml")
    "Respond to a phone call to NUMBER at Twilio.

Someone has called us at NUMBER, and  Twilio needs to know how to reply.
Send an XML (TwiML) response."
  (with-twilio-params ()
    (cxml:with-xml-output (cxml:make-octet-vector-sink)
      (cxml:with-element "Response"
                         (cxml:with-element "Message" 
                                            
                                            (cxml:text 
                                             
                                             "Hello,   you   have   reached   the
Corporation for Inter-World Tourism and Adventuring, the not-for-profit company
that maintains Tootsville."))))))



(defendpoint (post "/gossip/twilio/incoming/fax" "text/xml")
    "Respond to a fax call to NUMBER at Twilio.

Someone has faxxed us at NUMBER, and  Twilio needs to know how to reply.
Send an XML (TwiML) response."
  (with-twilio-params ()
    (error 'unimplemented)))



(defendpoint (post "/gossip/twilio/incoming/sms" "text/xml")
    "Respond to an SMS or MMS message to NUMBER at Twilio.

Someone  has messaged  us at  NUMBER, and  Twilio needs  to know  how to
reply. Send an XML (TwiML) response."
  (with-twilio-params ()
    (error 'unimplemented)))



(defendpoint (post "/gossip/twilio/incoming/whatsapp" "text/xml")
    "Respond to a WhatsApp message to NUMBER at Twilio.

Someone  has messaged  us at  NUMBER, and  Twilio needs  to know  how to
reply. Send an XML (TwiML) response."
  (with-twilio-params ()
    (error 'unimplemented)))



(defendpoint (post "/gossip/twilio/incoming/verify" "text/xml")
    "Check a Verify code from a user's phone.

We have sent a Verify code  to someone through Twilio. They have replied
by entering  that code, which  we now need  to verify through  the Authy
Verify endpoint."
  (with-twilio-params ()
    (error 'unimplemented)))


