(in-package :Tootsville)





(defendpoint (post "/gossip/twilio/incoming/call" "text/xml")
  "Respond to a phone call to NUMBER at Twilio.

Someone has called us at NUMBER, and  Twilio needs to know how to reply.
Send an XML (TwiML) response."
  (with-twilio-params ()
    (twilio:as-response 
     (twilio:say 
      (format nil "Hello~@[, ~a~], ~
this is the Tootsville telephone operator. ~
~@[Thank you for being a patron!~]"
              (when message-from-user
                (or (when-let (surname (person-surname message-from-user))
                      (ecase (person-gender message-from-user)
                        (:m (format nil "Mister ~a" surname))
                        (:f (format nil "Mizz ~a" surname))
                        (:x nil)))
                    (person-display-name message-from-user)
                    (person-given-name message-from-user)))
              (when message-from-user
                (person-is-patron-p message-from-user))))
     (twilio:with-gather ()
                         (twilio:say "This service is not available yet.")))))



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
    (twilio:as-response
     (twilio:message
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
                    (person-given-name message-from-user))))))))



(defendpoint (post "/gossip/twilio/incoming/whatsapp" "text/xml")
  "Respond to a WhatsApp message to NUMBER at Twilio.

Someone  has messaged  us at  NUMBER, and  Twilio needs  to know  how to
reply. Send an XML (TwiML) response."
  (with-twilio-params ()
    (twilio:as-response
     (twilio:message
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
                    (person-given-name message-from-user))))))))



(defendpoint (post "/gossip/twilio/incoming/verify" "text/xml")
  "Check a Verify code from a user's phone.

We have sent a Verify code  to someone through Twilio. They have replied
by entering  that code, which  we now need  to verify through  the Authy
Verify endpoint."
  (with-twilio-params ()
    (error 'unimplemented)))
