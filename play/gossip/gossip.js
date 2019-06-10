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

if (!('Tootsville' in window)) { Tootsville = {gossip: { peers: [], iceServers: {} } }; }
if (!('gossip' in Tootsville)) { Tootsville.gossip = { peers: [], iceServers: {} }; }
if (!('peers' in Tootsville.gossip)) { Tootsville.gossip.peers = [];}
if (!('iceServers' in Tootsville.gossip)) { Tootsville.gossip.iceServers = {}; };

/**
 * Accept an offer which was exchanged
 */
Tootsville.gossip.acceptOffer = function (offer)
{ if ( 0 == offer.length )
  { Tootsville.trace ("No offer to accept");
    return; }
  Tootsville.trace ("Accepting offer", offer);
  let peer = {};
  peer.connection = new RTCPeerConnection ({ iceServers: Tootsville.gossip.iceServers, iceCandidatePoolSize: 10 });
  peer.connection.ondatachannel = event =>
  { peer.receiveChannel = event.channel;
    peer.receiveChannel.onmessage = message => { Tootsville.gossip.gatekeeperAccept (peer, message); };
    // peer.receiveChannel.onopen , .onclose
  };
  peer.connection.setRemoteDescription (offer.offer);
  peer.connection.createAnswer ().then
  ( answer => { Tootsville.trace ("Created answer ", answer);
                peer.connection.setLocalDescription (answer);
                return answer; } ).then
  ( answer => { Tootsville.trace ("Answering received offer", offer.offer,
                                  "with answer", answer);
                Tootsville.util.rest ('POST', 'gossip/answers/' + offer.uuid, answer); } ); };

/**
 * Accept an offer from the central switchboard
 */
Tootsville.gossip.getOffer = function ()
{ Tootsville.trace ("Fetching offer now");
  Tootsville.util.rest ('GET', 'gossip/offers').then (Tootsville.gossip.acceptOffer); };

/**
 * Wait for an answer to an offer which was posted. Comet-type long poll.
 */
Tootsville.gossip.waitForAnswer = function (peer, offer, retries, next)
{ Tootsville.trace ("Waiting for peer answer at " + next);
  Tootsville.util.rest ('GET', next, null, { "Accept": "application/sdp" }).then (
      answer => { peer.connection.setRemoteDescription (answer); },
      error =>
          { Tootsville.trace ("No answer yet at " + next +
                              ( retries-- > 0 ? " Done waiting." : " Waiting for " + retries + " retries" ));
            return Tootsville.gossip.waitForAnswer (peer, offer, retries, next); });};

/**
 * Create and advertise an offer for connection.
 */
Tootsville.gossip.createConnection = function ()
{ let peer = {connection: new RTCPeerConnection (
    { iceServers: Tootsville.gossip.iceServers, iceCandidatePoolSize: 10 }) };
  Tootsville.trace ('Created local peer connection object peer.connection');
  peer.infinityChannel = peer.connection.createDataChannel('∞ Mode ℵ₀',
                                                           { ordered: false,
                                                             negotiated: false });
  Tootsville.trace ('Created Infinity Mode data channel');

  peer.infinityChannel.onopen = event => { Tootsville.gossip.openInfinityMode (peer, event); };

  peer.connection.createOffer ().then (
      offer => { return peer.connection.setLocalDescription(offer); }
  ).then (
      offer => { Tootsville.trace ("Posting offer to servers. Expect confirmation log line next.",
                                   peer.connection.localDescription);
                 Tootsville.util.rest ('POST', 'gossip/offers', peer.connection.localDescription).then (
                     next => { Tootsville.gossip.waitForAnswer (peer, offer, 30, next.location); }); });
  Tootsville.trace ("Offer should be posting now. This is confirmation."); };

/**
 * Accept an inbound datagram from an EVENT
 *
 * See the server documentation of `DEFINFINITY' for a description of the
 * Infinity Mode protocols.
 *
 * Commands are handled via the Tootsville.game.gatekeeper handlers.
 */
Tootsville.gossip.gatekeeperAccept = function (peer, event)
{ let gram = event.data;
  if (gram.c && Tootsville.game.commands [ gram.c ])
  { Tootsville.game.commands [ gram.c ] (gram.d, gram.u, gram.r); }
  else if (gram.from && Tootsville.game.gatekeeper [ gram.from ])
  { Tootsville.game.gatekeeper [ gram.from ] (gram); }
  else if (gram._cmd && gram._cmd == 'logOK')
  { Tootsville.game.gatekeeper.logOK (gram); }
  else
  { Tootsville.warn ("Unknown datagram type received", gram); }
  /* TODO: echo routing */ };

/**
 * Remove a gossip PEER connection
 */
Tootsville.gossip.closeInfinityMode = function (peer, event)
{ Tootsville.warn ("Dropped peer connection", peer, event);
  Tootsville.gossip.peers = Tootsville.gossip.peers.filter ( el => { return el != peer; } ); };
/**
 * Initiate Infinity mode communications; send a login packet out to $Eden
 */
Tootsville.gossip.openInfinityMode = function (peer, event)
{ Tootsville.trace ("Added Infinity Mode connection", peer, event);
  peer.infinityChannel.onmessage = event => { Tootsville.gossip.gatekeeperAccept (peer, event); };
  peer.infinityChannel.onclose = event => { Tootsville.gossip.closeInfinityMode (peer, event); };
  peer.infinityChannel.send (Tootsville.gossip.createPacket
                             ('login',
                              { userName: Tootsville.character,
                                password: Tootsville.gossip.keyPair.publicKey,
                                zone: 'Universe' },
                              '$Eden'));
  Tootsville.gossip.peers = Tootsville.gossip.peers.concat (peer);
  Tootsville.gossip.ensureConnected (); };

/**
 * Broadcast a packet.
 *
 * C is the command;  D is the command's data (if any);  R is the target
 * Recipient (originally  Room), which  defaults to  '$World', A  is the
 * author   (default  self),   and   V  (via),   if  present,   prevents
 * rebroadcasting the packet to the original sender. V (via) is expected
 * to be null, or an array of UUIDs.
 */
Tootsville.gossip.send = function (c, d, r, a, v)
{ let packet = Tootsville.gossip.createPacket (c, d, r || '$World', a, v);
  /* TODO: Routing */
  for (let i = 0; i < Tootsville.gossip.peers.length; ++i)
  { if ((! v)
        || (! (v.includes (Tootsville.gossip.peers[ i ].uuid))))
    { Tootsville.gossip.peers[ i ].infinityChannel.send (packet); } } };

/**
 * Ensure that we have an unique public/private key pair for this session
 */
Tootsville.gossip.ensureKeyPair = function ()
{ if (! (Tootsville.gossip.keyPair) )
  { Tootsville.gossip.keyPair = ed25519.generateKeyPair (); } };

/**
 * Sign a packet with our private key
 */
Tootsville.gossip.signPacket = function (c, d, r)
{ let payload = "→" + r + "⫽" + c + "⫽" + d;
  let signature = ED25519.sign (
      { message: payload,
        encoding: 'utf8',
        privateKey: Tootsville.gossip.keyPair.privateKey }); };

/**
 * Create and sign a packet; mandatory:  C command and D data; optional:
 * R recipient (defaulth "$World")
 */
Tootsville.gossip.createPacket = function (c, d, r)
{ let packet = (( c == 'logOK' ) || ( c.substring (0, 1) == ':' ))
      ? d : { d: d };
  packet.r = r || '$World';
  packet.a = Tootsville.characterUUID;
  packet.u = Tootsville.characterUUID;
  packet.s = Tootsville.gossip.signPacket (c, d, r);
  packet [ ((c == 'logOK') ? '_cmd'
            : (c.substring (0,1) == ':') ? 'from'
            : 'c') ] = (c.substring (0,1) == ':') ? c.substring(1) : c;
  return JSON.stringify (packet); };

/**
 * Connect to the global gossip network.
 */
Tootsville.gossip.connect = function ()
{ Tootsville.gossip.ensureKeyPair ();
  Tootsville.gossip.getOffer ();
  Tootsville.gossip.ensureConnected (); };

/**
 * Are we connected to the global gossip network?
 */
Tootsville.gossip.connectedP = function ()
{ return Tootsville.gossip.peers.length > 0; };

/**
 * Ensure that we have at least 5 gossip network connections.
 */
Tootsville.gossip.ensureConnected = function ()
{ let length = Tootsville.gossip.peers.length;
  if (length > 4)
  { Tootsville.warn ("Gossipnet already connected at " + length + " points");
    success (); } else
  { Tootsville.warn ("Gossipnet has " + length + " connections; adding one …");
    Tootsville.gossip.connect (); } };

/**
 * Obtain ICE server info from the game server.
 */
Tootsville.gossip.getICE = function ()
{ Tootsville.util.rest ('GET', 'gossip/ice-servers').then
  ( response => { Tootsville.gossip.iceServers = response;
                  Tootsville.gossip.ensureConnected (); },
    error => { Tootsville.parrot.say (
        "Squawk! Trouble getting connection servers",
        "I'm not able to get connection servers needed to join the game. Are you online?" ); } ); };
