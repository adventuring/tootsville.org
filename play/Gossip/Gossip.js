/* -*- js2 -*-*/

/**@license
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

if (!('Tootsville' in window)) { Tootsville = {Gossip: { peers: [], iceServers: {} } }; }
if (!('Gossip' in Tootsville)) { Tootsville.Gossip = { peers: [], iceServers: {} }; }
if (!('peers' in Tootsville.Gossip)) { Tootsville.Gossip.peers = [];}
if (!('iceServers' in Tootsville.Gossip)) { Tootsville.Gossip.iceServers = {}; };

/**
 * Accept an offer which was exchanged
 */
Tootsville.Gossip.acceptOffer = function (offer)
{ if ( (!(offer)) || (0 == offer.length) )
  { Tootsville.trace ("No offer to accept");
    return; }
  Tootsville.trace ("Accepting offer", offer);
  let peer = {};
  peer.connection = new RTCPeerConnection ({ iceServers: Tootsville.Gossip.iceServers,
                                             iceCandidatePoolSize: 10,
                                             type: 'answer'});
  peer.connection.ondatachannel = event =>
  { peer.receiveChannel = event.channel;
    peer.receiveChannel.onmessage = message => { Tootsville.Gossip.gatekeeperAccept (peer, message); };
    // peer.receiveChannel.onopen , .onclose
  };
  peer.connection.setRemoteDescription (offer.offer);
  peer.connection.createAnswer ().then
  ( answer => { Tootsville.trace ("Created answer ", answer);
                peer.connection.setLocalDescription (answer);
                return answer; } ).then
  ( answer => { Tootsville.trace ("Answering received offer", offer.offer,
                                  "with answer", answer);
                Tootsville.Util.rest ('POST', 'gossip/answers/' + offer.uuid, answer); } ); };

/**
 * Accept an offer from the central switchboard
 */
Tootsville.Gossip.getOffer = function (success)
{ Tootsville.trace ("Fetching offer now");
  Tootsville.Util.rest ('GET', 'gossip/offers').then (
      (offer) => { Tootsville.Gossip.acceptOffer (offer);
                   if (success) { success (); } }); };

/**
 * Wait for an answer to an offer which was posted. Comet-type long poll.
 */
Tootsville.Gossip.waitForAnswer = function (peer, offer, retries, next)
{ Tootsville.trace ("Waiting for peer answer at " + next);
  Tootsville.Util.rest ('GET', next, null, { "Accept": "application/sdp" }).then (
      answer => { peer.connection.setRemoteDescription (answer); },
      error =>
          { Tootsville.trace ("No answer yet at " + next +
                              ( retries-- > 0 ? " Done waiting." : " Waiting for " + retries + " retries" ));
            return Tootsville.Gossip.waitForAnswer (peer, offer, retries, next); });};

/**
 * Create and advertise an offer for connection.
 */
Tootsville.Gossip.createConnection = function ()
{ let peer = {connection: new RTCPeerConnection (
    { iceServers: Tootsville.Gossip.iceServers, iceCandidatePoolSize: 10 }) };
  Tootsville.trace ('Created local peer connection object peer.connection');
  peer.infinityChannel = peer.connection.createDataChannel('∞ Mode ℵ₀',
                                                           { ordered: false,
                                                             negotiated: false });
  Tootsville.trace ('Created Infinity Mode data channel');

  peer.infinityChannel.onopen = event => { Tootsville.Gossip.openInfinityMode (peer, event); };

  peer.connection.createOffer ().then (
      offer => { return peer.connection.setLocalDescription(offer); }
  ).then (
      offer => { Tootsville.trace ("Posting offer to servers. Expect confirmation log line next.",
                                   peer.connection.localDescription);
                 Tootsville.Util.rest ('POST', 'gossip/offers', peer.connection.localDescription).then (
                     next => { console.debug ("⁂", next);
                               Tootsville.Gossip.waitForAnswer (peer, offer, 30, next.location); }); });
  Tootsville.trace ("Offer should be posting now. This is confirmation."); };

/**
 * Accept an inbound datagram from a peer for an event.
 *
 * See the server documentation of `DEFINFINITY' for a description of the
 * Infinity Mode protocols.
 *
 * Commands are handled via the Tootsville.Game.gatekeeper handlers.
 */
Tootsville.Gossip.gatekeeperAccept = function (peer, event)
{ let gram = event.data;
  if (gram.c && Tootsville.Game.Commands [ gram.c ])
  { Tootsville.Game.Commands [ gram.c ] (gram.d, gram.u, gram.r); }
  else if (gram.from && Tootsville.Game.gatekeeper [ gram.from ])
  { Tootsville.Game.gatekeeper [ gram.from ] (gram); }
  else if (gram.seq)
  { for (let i = 0; i < gram.seq.length; ++i)
    { Tootsville.Gossip.gatekeeperAccept (peer, { data: gram.seq [i] }); } }
  else if (gram._cmd && gram._cmd == 'logOK')
  { Tootsville.Game.gatekeeper.logOK (gram); }
  else
  { Tootsville.warn ("Unknown datagram type received", gram); }
  /* TODO: echo routing */ };

/**
 * Remove a gossip PEER connection
 */
Tootsville.Gossip.closeInfinityMode = function (peer, event)
{ Tootsville.warn ("Dropped peer connection", peer, event);
  Tootsville.Gossip.peers = Tootsville.Gossip.peers.filter ( el => { return el != peer; } ); };

/**
 * Ensure that we have at least 5 gossip network connections.
 */
Tootsville.Gossip.ensureConnected = function (success)
{ let length = Tootsville.Gossip.peers.length;
  if (length > 4)
  { Tootsville.warn ("Gossipnet already connected at " + length + " points");
    success (); } else
  { Tootsville.warn ("Gossipnet has " + length + " connections; adding one …");
    Tootsville.Gossip.connect (success); } };


/**
 * Initiate Infinity mode communications; send a login packet out to $Eden
 */
Tootsville.Gossip.openInfinityMode = function (peer, event)
{ Tootsville.trace ("Added Infinity Mode connection", peer, event);
  peer.infinityChannel.onmessage = event => { Tootsville.Gossip.gatekeeperAccept (peer, event); };
  peer.infinityChannel.onclose = event => { Tootsville.Gossip.closeInfinityMode (peer, event); };
  peer.infinityChannel.send (Tootsville.Gossip.createPacket
                             ('login',
                              { userName: Tootsville.character,
                                password: Tootsville.Gossip.keyPair.publicKey,
                                zone: 'Universe' },
                              '$Eden'));
  Tootsville.Gossip.peers = Tootsville.Gossip.peers.concat (peer);
  setInterval (Tootsville.Gossip.ensureConnected, 500); };

/**
 * Broadcast a packet.
 *
 * The constructed  packet will be  signed and  broadcast to each  of the
 * mesh partners in the gossipnet.
 *
 * @table @code
 * @item c
 * is the command;  
 * @item d
 * is the command's data (if any);  
 * @item r
 * is the target Recipient (originally  Room), which  defaults to  '$World', 
 * @item a 
 *  is the author   (default  self),   and 
 * @item  v 
 *  (via),   if  present,   prevents
 * rebroadcasting the packet to the original sender. V (via) is expected
 * to be null, or an array of UUIDs.
 * @end table
 */
Tootsville.Gossip.send = function (c, d, r, a, v)
{ let packet = Tootsville.Gossip.createPacket (c, d, r || '$World', a, v);
  /* TODO: Routing */
  for (let i = 0; i < Tootsville.Gossip.peers.length; ++i)
  { if ((! v)
        || (! (v.includes (Tootsville.Gossip.peers[ i ].uuid))))
    { Tootsville.Gossip.peers[ i ].infinityChannel.send (packet); } } };

/**
 * Ensure that we have an unique public/private key pair for this session
 */
Tootsville.Gossip.ensureKeyPair = function ()
{ if (! (Tootsville.Gossip.keyPair) )
  { Tootsville.Gossip.keyPair = forge.rsa.generateKeyPair (); } };

/**
 * Sign a packet with our private key
 */
Tootsville.Gossip.signPacket = function (c, d, r)
{ let payload = JSON.stringify ({ c: c, d: d, r: r });
  let signature = forge.rsa.encrypt (
      forge.sha256.create ().start ().update (payload).digest ().data,
       Tootsville.Gossip.keyPair.privateKey ); };

/**
 * Create and sign a packet.
 * @table @code
 * @item c
 * Command to broadcast.  This can be the string  @code{logOK}, which is
 * handled    as   a    @code{_cmd}    (in   fact,    it's   the    only
 * remaining @code{_cmd} command); a  string beginning with @code{:}, in
 * which case  the command will be  a reply packet with  @code{from}; or
 * any other string, in which case it's treated as a @code{c} command.
 * @item d
 * Data for the command. 
 * @item r
 * Optional recipient UUID (or default @code{$World})
 * @end table
 *
 * Automatically   adds  @code{a}   for  author,   @code{u}  for   user,
 * and @code{s} signature.
 *
 * Returns a JSON string of the signed packet. 
 */
Tootsville.Gossip.createPacket = function (c, d, r)
{ let packet = (( c == 'logOK' ) || ( c.substring (0, 1) == ':' ))
      ? d : { d: d };
  packet.r = r || '$World';
  packet.a = Tootsville.characterUUID;
  packet.u = Tootsville.characterUUID;
  packet.s = Tootsville.Gossip.signPacket (c, d, r);
  packet [ ((c == 'logOK') ? '_cmd'
            : (c.substring (0,1) == ':') ? 'from'
            : 'c') ] = (c.substring (0,1) == ':') ? c.substring(1) : c;
  return JSON.stringify (packet); };

/**
 * Send a logOK message to the gossip net.
 */
Tootsville.Gossip.sendLogOK = function ()
{ Tootsville.Gossip.sendPacket ('logOK', { neighbor: Tootsville.characterUUID }); };

/**
 * Connect to the global gossip network.
 *
 * Calls next function @code{success} on success.
 */
Tootsville.Gossip.connect = function (success)
{ Tootsville.Gossip.ensureKeyPair ();
  Tootsville.Gossip.createConnection ();
  Tootsville.Gossip.getOffer (success); };

/**
 * Are we connected to the global gossip network (at all)?
 */
Tootsville.Gossip.connectedP = function ()
{ return Tootsville.Gossip.peers.length > 0; };

/**
 * Obtain ICE server info from the game server.
 */
Tootsville.Gossip.getICE = function ()
{ Tootsville.Util.rest ('GET', 'gossip/ice-servers').then
  ( response => { Tootsville.Gossip.iceServers = response;
                  Tootsville.Gossip.ensureConnected ().then
                  ( Tootsville.Gossip.sendLogOK ); },
    error => { Tootsville.Parrot.say (
        "Squawk! Trouble getting connection servers",
        "I'm not able to get connection servers needed to join the game. Are you online?" );
               Tootsville.UI.HUD.refreshHUD (); } ); };
