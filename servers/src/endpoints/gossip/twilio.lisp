(in-package :Tootsville)


(defendpoint (post "/gossip/twilio/incoming/:number/call" "text/xml")
    "Respond to a phone call to NUMBER at Twilio.

Someone has called us at NUMBER, and  Twilio needs to know how to reply.
Send an XML (TwiML) response."
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/incoming/:number/fax" "text/xml")
    "Respond to a fax call to NUMBER at Twilio.

Someone has faxxed us at NUMBER, and  Twilio needs to know how to reply.
Send an XML (TwiML) response."
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/incoming/:number/sms" "text/xml")
    "Respond to an SMS or MMS message to NUMBER at Twilio.

Someone  has messaged  us at  NUMBER, and  Twilio needs  to know  how to
reply. Send an XML (TwiML) response."
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/incoming/:number/whatsapp" "text/xml")
    "Respond to a WhatsApp message to NUMBER at Twilio.

Someone  has messaged  us at  NUMBER, and  Twilio needs  to know  how to
reply. Send an XML (TwiML) response."
  (error 'unimplemented))



(defendpoint (post "/gossip/twilio/incoming/verify" "text/xml")
    "Check a Verify code from a user's phone.

We have sent a Verify code  to someone through Twilio. They have replied
by entering  that code, which  we now need  to verify through  the Authy
Verify endpoint."
  (error 'unimplemented))


