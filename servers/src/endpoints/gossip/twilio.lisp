(in-package :Tootsville)

(defmacro with-twilio-params (() &body body)
  `(let* ((message-sid (hunchentoot:parameter "MessageSid"))
          (account-sid (hunchentoot:parameter "AccountSid"))
          (messaging-service-sid (hunchentoot:parameter "MessagingServiceSid"))
          (message-from (hunchentoot:parameter "From"))
          (message-from-user (find-person-by-url "tel://" message-from))
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
        (cxml:with-element "Say"
          (cxml:with-element "Body"
            (cxml:text
             (format nil "Hello~@[, ~a~], ~
this is the Tootsville telephone operator. ~
~@[Thank you for being a patron!~] ~
How can I help you? ~
This service is not available right now." ; TODO
                     (when message-from-user
                       (or (when-let (surname (person-surname message-from-user))
                             (ecase (person-gender message-from-user)
                               (:m (format nil "Mister ~a" surname))
                               (:f (format nil "Mizz ~a" surname))
                               (:x nil)))
                           (person-display-name message-from-user)
                           (person-given-name message-from-user)))
                     (when message-from-user
                       (person-is-patron-p message-from-user))))))))))



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
    (cxml:with-element "Response"
      (cxml:with-element "Message"
        (cxml:with-element "Body"
          (cxml:text
           (format nil "Hello~@[, ~a~], ~
this is Tootsville. ~
This service is not available now." ; TODO
                   (when message-from-user
                     (or (when-let (surname (person-surname message-from-user))
                           (ecase (person-gender message-from-user)
                             (:m (format nil "Mister ~a" surname))
                             (:f (format nil "Mizz ~a" surname))
                             (:x nil)))
                         (person-display-name message-from-user)
                         (person-given-name message-from-user))))))))))



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
