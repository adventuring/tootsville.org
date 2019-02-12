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
{ return (Tootsville.gossip.peers != []);; };

Tootsville.gossip.stunServers = function ()
{ var sample = [ { urls: [ "stun:stun.l.google.com:19302" ] } ];
  /* Firefox wants 2, preferably, and never more than 5, so 3 seems safe. */
  sample = sample.concat ({ urls: [ Tootsville.gossip.stunList [Math.floor (Math.random () * Tootsville.gossip.stunList.length)] ]});
  sample = sample.concat ({ urls: [ Tootsville.gossip.stunList [Math.floor (Math.random () * Tootsville.gossip.stunList.length)] ]});
  return sample; };

Tootsville.gossip.postDescription = function (peer, description)
{ return new Promise (success =>
                      { Tootsville.util.rest ('POST', "/gossip/offers", JSON.stringify ({ offers: [ description ] })).
                        then (success); }); };

Tootsville.gossip.createConnection = function ()
{ var servers = Tootsville.gossip.stunServers ();
  Tootsville.trace ("STUN servers", servers);
  var peer = {connection: new RTCPeerConnection({ iceServers: servers, iceCandidatePoolSize: 10 }) };
  Tootsville.trace ('Created local peer connection object peer.connection');
  peer.infinityChannel = peer.connection.createDataChannel('∞ Mode ℵ₀',
                                                           { ordered: false,
                                                             negotiated: false });
  Tootsville.trace ('Created Infinity Mode data channel');

  peer.infinityChannel.onopen = event => { Tootsville.gossip.openInfinityMode (peer, event); };

  peer.connection.createOffer().then(
      () => { Tootsville.utils.rest ('POST', 'gossip/offers', JSON.stringify ({ offers: peer.connection.localDescription }) ); }); };

/**
* Accept an inbound datagram.
*
* See the server documentation of `DEFINFINITY' for a description of the
* Infinity Mode protocols.
*/
Tootsville.gossip.gatekeeperAccept = function (peer, event)
{ var gram = event.data;
  if (gram.c && Tootsville.game.commands [ gram.c ])
  { Tootsville.game.commands [ gram.c ] (gram.d, gram.u, gram.r); }
  else if (gram.from && Tootsville.game.gatekeeper [ gram.from ])
  { Tootsville.game.gatekeeper [ gram.from ] (gram); }
  else if (gram._cmd && gram._cmd == 'logOK')
  { Tootsville.game.gatekeeper.logOK (gram); }
  else
  { Tootsville.warn ("Unknown datagram type received", gram); }
};

Tootsville.gossip.closeInfinityMode = function (peer, event)
{ Tootsville.warn ("Dropped peer connection", peer, event);
  Tootsville.gossip.peers = Tootsville.gossip.peers.filter ( el => { return el != peer; } ); };

Tootsville.gossip.openInfinityMode = function (peer, event)
{ Tootsville.trace ("Added Infinity Mode connection", peer, event);
  peer.infinityChannel.onmessage = event => { Tootsville.gossip.gatekeeperAccept (peer, event); };
  peer.infinityChannel.onclose = event => { Tootsville.gossip.closeInfinityMode (peer, event); };
  peer.infinityChannel.send (JSON.stringify ( { c: login, d: { userName: Tootsville.character, password: null, zone: 'Tootsville Ⅴ' }, r: '$Eden' }));
  Tootsville.gossip.peers = Tootsville.gossip.peers.concat (peer); };

Tootsville.gossip.connect = function ()
{ Tootsville.gossip.createConnection (); };

Tootsville.gossip.ensureConnected = function ()
{ return new Promise (
    (success, failure) =>
        { if (Tootsville.gossip.connectedP ())
          { Tootsville.warn ("Gossipnet already connected");
            success (); } else
          { Tootsville.warn ("Gossipnet not connected; connecting now");
            Tootsville.gossip.connect ().then (success, failure); } } ); };

