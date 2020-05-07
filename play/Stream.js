/* -*- js2 -*-*/

/**@license
 *
 * ./play/Stream.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
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
if (!("Util" in Tootsville)) { Tootsville.Util = {}; }

Tootsville.Util.checkStream = function () {
    if (Tootsville.Util.WebSocket)
    { if (Tootsville.Util.WebSocket.readyState == WebSocket.OPEN)
      { Tootsville.Util.infinity ("ping", { pingStarted: Tootsville.Game.now } ); }
      else if (! (Tootsville.Util.WebSocket.readyState == WebSocket.CONNECTING) )
      { Tootsville.Util.connectWebSocket (); } } };

Tootsville.Util.connectWebSocket = function () {
    if ( Tootsville.Util.WebSocket &&
         (Tootsville.Util.WebSocket.readyState == WebSocket.OPEN ||
          Tootsville.Util.WebSocket.readyState == WebSocket.CONNECTING) )
    { console.log ("WebSocket already ready already.");
      return; }
    if (! Tootsville.Login.firebaseAuth)
    { console.warn ("Can't connect until authenticated");
      return; }
    let uri = Tootsville.host.game.replace("http", "ws") + "/infinity/alef-null";
    if (/:5000/.test (uri)) {
        uri = uri.replace (":5000", ":5004");
    }
    console.log ("Connecting WebSocket to " + uri);
    Tootsville.Util.WebSocket = new WebSocket (uri); 
    Tootsville.Util.WebSocket.onopen = (event) => { Tootsville.Util.openWebSocket (event); }
    Tootsville.Util.WebSocket.onclose = (event) => { Tootsville.Util.closeWebSocket (event); }
    Tootsville.Util.WebSocket.onmessage = (event) => { Tootsville.Util.messageFromWebSocket (event); }
    Tootsville.Util.WebSocket.onerror = (event) => { Tootsville.Util.errorFromWebSocket (event); }
    if (! Tootsville.Util.checkStreamRunning)
    { Tootsville.Util.checkStreamRunning = setInterval (Tootsville.Util.checkStream, 300000); } };

Tootsville.Util.stream = function (json)
{ console.debug ("WebSocket stream send command " + json.c, json);
  if (Tootsville.Util.WebSocket.readyState == WebSocket.CONNECTING)
  { setTimeout ( () => Tootsville.Util.stream (json), 10 ); }
  Tootsville.Util.WebSocket.send (JSON.stringify(json)); };

Tootsville.Util.openWebSocket = function (event) {
    Tootsville.Util.stream ({ c: "Auth/∞/ℵ₀",
                              provider: "Firebase",
                              token: Tootsville.Login.firebaseAuth });
};

Tootsville.Util.closeWebSocket = function (event) {
    console.warn ("Closing WebSocket connection", event);
};

Tootsville.Util.messageFromWebSocket = function (event)
{ let data = JSON.parse (event.data);
  console.debug ("Received data from " + data.from, data);
  Tootsville.Gossip.gatekeeperAccept (null, { data: data } ); };

Tootsville.Util.errorFromWebSocket = function (event)
{ console.warn ("WebSocket error", event);
  Tootsville.Gossip.Parrot.say ("Connection Problem",
                                "WebSocket connection problem: " + event.type +
                                " connecting to " + event.target.url +
                                ". Are you online?"); };

/**
* Submit an Infinity Mode command to  the servers or peers, but wait for
* the next reply  of the given type for a  Promised call-back. Note that
* the Gatekeeper still gets a chance to handle any returned values, this
* just registers a Promise to listen for the reply.
*/
Tootsville.Util.infinityAwaits = function (command, fromType, params) {
    return new Promise ( resolve => {
        Tootsville.Gossip.eavesdrop (fromType || command, (data) => {
            console.log ("Got awaited reply from: " + fromType);
            resolve (data); });
        Tootsville.Util.stream (Tootsville.Gossip.createPacket (command, params || {}) ); }); };

Tootsville.Util.infinity = function (command, params) {
    Tootsville.Util.stream (Tootsville.Gossip.createPacket (command, params || {}) ); };
