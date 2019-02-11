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

if (!Tootsville.gossip.iceServers)
{ Tootsville.gossip.iceServers = [ { urls: [ 'stun:stun.l.google.com:19302' ] } ];}

Tootsville.gossip.makeConnection = function ()
{ return { localConnection: undefined,
           remoteConnection: undefined,
           sendChannel: undefined,
           receiveChannel: undefined,
           pcConstraint: undefined,
           dataConstraint: undefined }; };

Tootsville.gossip.peers = [];

Tootsville.gossip.createConnection = function ()
{ var servers = Tootsville.gossip.iceServers;
  var peer = Tootsville.gossip.makeConnection ();
  Tootsville.warn('Using SCTP based data channels');
  // For  SCTP,  reliable  and  ordered delivery  is  true  by  default.
  // Add localConnection  to global  scope to make  it visible  from the
  // browser console.
  peer.localConnection = new RTCPeerConnection(servers, peer.pcConstraint);
  Tootsville.warn('Created local peer connection object localConnection');

  peer.sendChannel = peer.localConnection.createDataChannel('sendDataChannel',
                                                       peer.dataConstraint);
  Tootsville.warn('Created send data channel');

  peer.localConnection.onicecandidate = event => { Tootsville.gossip.iceCallback1 (peer, event); };
  peer.sendChannel.onopen = event => { Tootsville.gossip.onSendChannelStateChange (peer, event); };
  peer.sendChannel.onclose = event => { Tootsville.gossip.onSendChannelStateChange (peer, event); };

  // Add remoteConnection  to global scope  to make it visible  from the
  // browser console.
  peer.remoteConnection = new RTCPeerConnection(servers, peer.pcConstraint);
  Tootsville.warn('Created remote peer connection object remoteConnection');

  peer.remoteConnection.onicecandidate = event => { Tootsville.gossip.iceCallback2 (peer, event); };
  peer.remoteConnection.ondatachannel = Tootsville.gossip.receiveChannelCallback;

  peer.localConnection.createOffer().then(
      desc => { Tootsville.gossip.gotDescription1 (peer, desc); },
      Tootsville.gossip.onCreateSessionDescriptionError ); };

Tootsville.gossip.onCreateSessionDescriptionError = function (error)
 { Tootsville.warn('Failed to create session description: ' + error.toString()); };

Tootsville.gossip.sendData = function (data)
 { sendChannel.send(data);
   Tootsville.warn('Sent Data: ' + data); };

Tootsville.gossip.closeDataChannels = function (peer)
{ Tootsville.warn('Closing data channels');
  peer.sendChannel.close();
  Tootsville.warn('Closed data channel with label: ' + sendChannel.label);
  peer.receiveChannel.close();
  Tootsville.warn('Closed data channel with label: ' + receiveChannel.label);
  peer.localConnection.close();
  peer.remoteConnection.close();
  peer.localConnection = null;
  peer.remoteConnection = null;
  Tootsville.warn('Closed peer connections'); };

Tootsville.gossip.gotDescription1 = function (peer, desc)
{ peer.localConnection.setLocalDescription(desc);
  Tootsville.warn('Offer from localConnection \n' + desc.sdp);
  peer.remoteConnection.setRemoteDescription(desc);
  peer.remoteConnection.createAnswer().then(
      desc => { Tootsville.gossip.gotDescription2 (peer, desc); },
      onCreateSessionDescriptionError ); };

Tootsville.gossip.gotDescription2 = function (peer, desc)
{ peer.remoteConnection.setLocalDescription (desc);
  Tootsville.warn('Answer from remoteConnection \n' + desc.sdp);
  peer.localConnection.setRemoteDescription (desc);
  Tootsville.gossip.peers[ Tootsville.gossip.peers.length ] = peer; };

Tootsville.gossip.iceCallback1 = function (peer, event)
{ Tootsville.warn('local ice callback');
  if (event.candidate)
 { peer.remoteConnection.addIceCandidate(
            event.candidate
        ).then(
            onAddIceCandidateSuccess,
            onAddIceCandidateError
        );
        Tootsville.warn('Local ICE candidate: \n' + event.candidate.candidate);
    }
 };

Tootsville.gossip.iceCallback2 = function (peer, event)
 {    Tootsville.warn('remote ice callback');
    if (event.candidate)
 { peer.localConnection.addIceCandidate(
            event.candidate
        ).then(
            () => { Tootsville.gossip.onAddIceCandidateSuccess (peer) },
            onAddIceCandidateError
        );
        Tootsville.warn('Remote ICE candidate: \n ' + event.candidate.candidate);
    }
 };

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

