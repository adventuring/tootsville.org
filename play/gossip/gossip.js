/* -*- js2 -*-*/

/*@license
 *
 * ./play/gossip/gossip.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
 * Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You  should have  received a  copy of  the GNU  Affero General  Public
 * License     along    with     this     program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */

if (!Tootsville.gossip) { Tootsville.gossip = {}; }

if (!Tootsville.gossip.peers)
{ Tootsville.gossip.peers = [];}

Tootsville.gossip.connectedP = function ()
{ for (var peer in Tootsville.gossip.peers)
  { if (peer && peer.openP) { return true; } }
  return false; };

Tootsville.gossip.stunServers = function ()
{ var sample = [ { urls: [ "stun:stun.l.google.com:19302",
                           "stun:stun1.l.google.com:19302",
                           "stun:stun2.l.google.com:19302",
                           "stun:stun3.l.google.com:19302" ] } ];
  /* Firefox wants 2, preferably, and never more than 5, so 3 seems safe. */
  sample = sample.concat ({ urls: [ Tootsville.gossip.stunList [Math.floor (Math.random () * Tootsville.gossip.stunList.length)] ]});
  sample = sample.concat ({ urls: [ Tootsville.gossip.stunList [Math.floor (Math.random () * Tootsville.gossip.stunList.length)] ]});
  return sample; };

Tootsville.gossip.makeConnection = function ()
{ return { connection: undefined,
           sendChannel: undefined,
           receiveChannel: undefined,
           pcConstraint: undefined,
           dataConstraint: undefined }; };

Tootsville.gossip.peers = [];

Tootsville.gossip.postDescription = function (peer, description)
{ return new Promise (success =>
                      { Tootsville.util.rest ('POST', "/gossip/offers", JSON.stringify ({ offers: [ description ] })).
                        then (success); }); };

Tootsville.gossip.createConnection = function ()
{ var servers = Tootsville.gossip.stunServers ();
  Tootsville.trace ("STUN servers", servers);
  var peer = Tootsville.gossip.makeConnection ();
  Tootsville.trace ('Using SCTP based data channels');
  // For  SCTP,  reliable  and  ordered delivery  is  true  by  default.
  // Add peer.connection  to global  scope to make  it visible  from the
  // browser console.
  peer.connection = new RTCPeerConnection( { iceServers: servers }, peer.pcConstraint);
  Tootsville.trace ('Created local peer connection object peer.connection');

  peer.sendChannel = peer.connection.createDataChannel('sendDataChannel',
                                                       peer.dataConstraint);
  Tootsville.trace ('Created send data channel');

  peer.connection.onicecandidate = event => { Tootsville.gossip.iceCallback1 (peer, event); };
  peer.sendChannel.onopen = event => { Tootsville.gossip.onSendChannelStateChange (peer, event); };
  peer.sendChannel.onclose = event => { Tootsville.gossip.onSendChannelStateChange (peer, event); };

  peer.connection.createOffer().then(
      desc => { Tootsville.gossip.gotDescription1 (peer, desc);
                Tootsville.gossip.postDescription (peer, desc).then (
                    answer =>
                        { Tootsville.warn('Answer from remote peer', answer.sdp);
                          peer.connection.setRemoteDescription (answer);
                          Tootsville.gossip.peers = Tootsville.gossip.peers.concat (peer); });},
      Tootsville.gossip.onCreateSessionDescriptionError ); };

Tootsville.gossip.onCreateSessionDescriptionError = function (error)
 { Tootsville.error ('Failed to create session description: ' + error.toString()); };

Tootsville.gossip.sendData = function (data)
 { sendChannel.send(data);
   Tootsville.trace ('Sent Data', data); };

Tootsville.gossip.closeDataChannels = function (peer)
{ Tootsville.warn ('Closing data channels');
  peer.sendChannel.close();
  Tootsville.warn ('Closed data channel with label: ' + sendChannel.label);
  peer.receiveChannel.close();
  Tootsville.warn ('Closed data channel with label: ' + receiveChannel.label);
  peer.connection.close();
  peer.connection = null;
  Tootsville.warn ('Closed peer connections'); };

Tootsville.gossip.gotDescription1 = function (peer, desc)
{ peer.connection.setLocalDescription(desc);
  Tootsville.warn('Offer from peer.connection', desc.sdp); };

Tootsville.gossip.iceCallback1 = function (peer, event)
{ Tootsville.warn ('local ice callback');
  if (event.candidate)
  { Tootsville.warn ('ICE candidate', event.candidate);
  peer.connection.addIceCandidate (event.candidate).
    then(
        () => { Tootsville.gossip.onAddIceCandidateSuccess (peer); },
        Tootsville.gossip.onAddIceCandidateError
    );}
  else
  { Tootsville.warn ("No candidate in ICE callback"); } };

Tootsville.gossip.iceCallback2 = function (peer, event)
{ Tootsville.warn('remote ice callback');
  if (event.candidate)
  { peer.connection.addIceCandidate (event.candidate).
    then(
        () => { Tootsville.gossip.onAddIceCandidateSuccess (peer); },
        Tootsville.gossip.onAddIceCandidateError
    );
    Tootsville.warn('Remote ICE candidate: \n ' + event.candidate.candidate); }
  else
  { Tootsville.warn ("No candidate in ICE callback", event); } };

Tootsville.gossip.onAddIceCandidateSuccess = function (peer)
 {    Tootsville.warn('AddIceCandidate success.');
 };

Tootsville.gossip.onAddIceCandidateError = function (error)
 {    Tootsville.warn('Failed to add Ice Candidate: ' + error.toString());
 };

Tootsville.gossip.receiveChannelCallback = function (peer, event)
 { Tootsville.warn('Receive Channel Callback');
   peer.receiveChannel = event.channel;
   peer.receiveChannel.onmessage = Tootsville.gossip.onReceiveMessageCallback;
   peer.receiveChannel.onopen = Tootsville.gossip.onReceiveChannelStateChange;
   peer.receiveChannel.onclose = Tootsville.gossip.onReceiveChannelStateChange;
 };

Tootsville.gossip.onReceiveMessageCallback = function (event)
 { Tootsville.warn('Received Message');
   Tootsville.gossip.processIncomingDatagram(event.data);
 };

Tootsville.gossip.onSendChannelStateChange = function (peer, event)
 { var readyState = peer.sendChannel.readyState;
   Tootsville.warn('Send channel state is: ' + readyState);
   if (readyState === 'open')
   { peer.openP = true; }
   else
   { peer.openP = false; } };

Tootsville.gossip.onReceiveChannelStateChange = function ()
 { var readyState = receiveChannel.readyState;
    Tootsville.warn('Receive channel state is: ' + readyState);
 };

Tootsville.gossip.connect = function ()
{ Tootsville.gossip.createConnection (); };

Tootsville.gossip.ensureConnected = function ()
{ return new Promise (
    (success, failure) =>
        { if (Tootsville.gossip.connectedP ())
          { Tootsville.warn ("Gossipnet already connected");
            success (); } else
          { Tootsville.warn ("Gossipnet not connected; connecting now");
            Tootsville.gossip.connect ().then (success, failure); } }); };

