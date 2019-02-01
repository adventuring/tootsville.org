;;;; -*- lisp -*-
;;;
;;;; ./servers/src/endpoints/gossip/twilio/gossip-twilio.lisp is part of
;;;; Tootsville
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
