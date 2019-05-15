(in-package :Tootsville)

(defendpoint (post "/gossip/twilio/call/toots/incoming" "text/xml")
  "A call comes in to 954-Toots-05"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/call/toots/status-changed" "text/xml")
  "Call status changes on 954-Toots-05"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/text/toots/incoming" "text/xml")
  "An SMS or MMS is received on 954-Toots-05"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/call/joy/incoming" "text/xml")
  "A call comes in to 9-Joy-4-Toots"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/call/joy/status-changed" "text/xml")
  "Call status changes on 9-Joy-4-Toots"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/text/joy/incoming" "text/xml")
  "An SMS or MMS is received on 9-Joy-4-Toots"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/call/toots.uk/incoming" "text/xml")
  "A call comes in to +44 12507 70075"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/call/toots.uk/status-changed" "text/xml")
  "Call status changes on +44 12507 70075"
  (error 'unimplemented))

(defendpoint (post "/gossip/twilio/text/toots.uk/incoming" "text/xml")
  "An SMS or MMS is received on +44 12507 70075"
  (error 'unimplemented))
