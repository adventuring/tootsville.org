/* -*- js2 -*- */

/**@license
 *
 * play/Game/Gatekeeper.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2021   The
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

if (!('Game' in Tootsville)) { Tootsville.Game = {Gatekeeper: {}}; }
if (!('Gatekeeper' in Tootsville.Game)) { Tootsville.Game.Gatekeeper = {}; }

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper._err = function (gram) {
    if (!(gram.status)) {
        Tootsville.UI.confirmPretty ('Error',
                                     `
<p> There was an error: </p>
<p> ${ gram.error } </p>
<p><small> Error from ${ gram.from }, code ${ gram.err } </small></p>`,
                                     'Oops', null); } };


/**
 * Acknowledge a new player's login
 *
 * neighbor: next-hop neighbor's UUID for peer-to-peer connections
 *
 * Note that this message (only) uses @code{_cmd} as its attribute rather than
 * @code{c} or @code{from} for historical reasons.
 *
 * This message is usually unicast.
 *
 * Hides Toots view, if it was present.
 * Displays any @code{motd} on server connection.
 *
 * UNIMPLEMENTED TODO peer connections.
 */
Tootsville.Game.Gatekeeper.logOK = function (gram)
{ let neighbor = gram.neighbor;
  if (gram.status )
  { console.log ("Logged in to servers");
    Tootsville.Gossip.ensureKeyPair ();
    if ('none' != document.getElementById ('sign-in').style.display)
    { Tootsville.Login.switchTootsView (); }
    if (gram.greet) console.info ("Greeting", gram.greet);
    if (gram.auth) console.log ("Authentication", gram.auth);
    if (gram.motd && ! ( Tootsville.seen.motd ) )
    { Tootsville.Gossip.Parrot.say ("Message of the Day", gram.motd);
      Tootsville.seen.motd = true; } }
  else
  { Tootsville.Gossip.Parrot.say ("Error from login", gram.error + " due to " + gram.auth +
                                  " This will prevent you from playing. " +
                                  "Contact support if you don't understand.");
    Tootsville.Util.WebSocket.close ();
    Tootsville.Util.connectWebSocket (); }
  if (neighbor)
      Tootsville.warn ("logOK not handled for peer connections", gram); };

/**
 * Receive a list of avatar info that describes an area of the world.
 *
 * This is one observer's set of nearby avatars or objects.
 *
 */
Tootsville.Game.Gatekeeper.avatars = function (gram)
{ let world = gram.inRoom;
  let avatars = Object.values (gram.avatars);
  console.log ("Received info on " + avatars.length + " avatars");
  for (let i = 0; i < avatars.length; ++i)
  { let avatar = avatars [ i ];
    Tootsville.Game.acceptAvatar (avatar); } };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.bots = function (gram)
{ let bots = gram.bots;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * Not currently used. UNIMPLEMENTED.
 *
 * XXX bring back passports
 */
Tootsville.Game.Gatekeeper.passport = function (gram)
{ if (!gram.status)
    Tootsville.Game.Gatekeeper._err (gram);
  let passport = gram.passport;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * Mostly just for fountains and shops, now
 *
 * WRITEME
 */
Tootsville.Game.Gatekeeper.startEvent = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  if (gram.handler) {
      if ('fountain' == gram.handler) {
          Tootsville.UI.confirmPretty (
              "Fountain",
              "Make a wish?",
              "Make a wish", "No wish").then
          ( confirm => {
              Tootsville.Util.infinity
              ('endEvent',
               { eventID: gram.eventID,
                 status: confirm ? "cmp" : "cxl" });});
          return;
      }
      if ('vitem' == gram.handler) {
          Tootsville.UI.confirmPretty (
              "A Discovery",
              `<p>You have made a discovery.</p>
<h4>${gram.vitem.name}</h4>
<p>${gram.vitem.description}</p>`,
              'Take', "Don't Take").then
          ( confirm => {
              Tootsville.Util.infinity
              ('endEvent',
               { eventID: gram.eventID,
                 status: confirm ? 'cmp' : 'cxl' });});
          return;
      }
      if ('shop' == gram.handler) {
          Tootsville.UI.confirmPretty (
              "Purchase?",
              `<p>Would you like to buy this?</p>
<p> Price: ${ gram.price } 🥜 </p>
<hr>
<h4>${gram.item.name}</h4>
<p>${gram.item.description}</p>`,
              `Pay ${gram.price}🥜`,
              "Don't Buy").then
          ( confirm => {
              Tootsville.Util.infinity
              ('endEvent',
               { eventID: gram.eventID,
                 status: confirm ? 'cmp' : 'cxl' });});
      }
      if ('download' == gram.handler) {
          Tootsville.UI.confirmPretty (
              'A Download!',
              `<p>You can download a gift from here.</p>`,
              'Download', "Don't Download").then
          (confirm => {
              Tootsville.Util.infinity
              ('endEvent',
               { eventID: gram.eventID,
                 status: confirm ? "cmp" : "cxl" });});
      }
  }
  if (gram.asVersion) {
      if ('html5' === gram.asVersion)
          Tootsville.Game.startMinigameEvent (gram);
      else
          Tootsville.UI.confirmPretty(
              "Can't perform event",
              `The event requires a processor for ${gram.asVersion},
which is not available.`,
              'O.K.', null); }
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Used to be used for minigame scores; not currently used.
 *
 * UNIMPLEMENTED. Display the rank and score in an overlay.
 */
Tootsville.Game.Gatekeeper.scoreUpdate = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  const rank = gram.place;
  const score = gram.score;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * End an event begun by startEvent.
 *
 * Earn peanuts for event participation.
 *
 */
Tootsville.Game.Gatekeeper.endEvent = function (gram)
{ if (! (gram.status) ) {
    Tootsville.UI.confirmPretty ("Can't do that",
                                 `<large>${gram.error}</large>
<P> Code: ${gram.err} </P>`,
                                 'Sorry', null);
    return; }
  if (gram.canceled) return;
  if (gram.download) {
      Tootsville.UI.confirmPretty ('Download',
                                   `
<p> You can <a href="${gram.download}" target="download">download
 this file</a> now. </p>`,
                                   'Done', null);
      return;
  }
  const eventID = gram.eventID;
  const peanuts = gram.peanuts;
  const fairyDust = gram.fairyDust;
  const item = gram.item;
  let nn = (item && item.template && /^[aeiouhAEIOUH]/.test(item.template.name[0])) ? 'n' : '';
  let earnedPeanuts = (peanuts && peanuts > 0) ? 'earned' : 'spent';
  let earnedFairyDust = (fairyDust && fairyDust > 0) ? 'earned' : 'spent';
  Tootsville.UI.confirmPretty ("Success!",
                               '<P>You finished' +
                               (peanuts && peanuts != 0 ? ` and ${earnedPeanuts} ${Math.abs(peanuts)} 🥜` : '') +
                               (fairyDust && fairyDust != 0 ? ` and ${earnedFairyDust} ${Math.abs(fairyDust)} ⁂` : '') +
                               (item && item != '' ? ` and got a${nn} ${item.template.name}. </P><P> ${item.template.description} </P>` : '.</P>'),
                               'Cool', null);
  if ((peanuts != 0) || (fairyDust != 0))
      Tootsville.Util.infinity('getWallet'); };

/**
 * Not currently in use. UNIMPLEMENTED.
 */
Tootsville.Game.Gatekeeper.gameAction = function (gram)
{ if (! gram.status)
    Tootsville.Game.Gatekeeper._err (gram);
  const data = gram.data;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * The player  has been teleported  to the given coördinates  and should
 * now explore  the surroundings (discover  what objects are  nearby, et
 * al.).
 *
 * The author must have the privilege to beam this player, or the signal
 * should be discarded.
 *
 * @subsection Packet Format
 *
 * @verbatim
 * { world: WORLD, latitude: LAT, longitude: LONG, altitude: ALT,
 *   x: X, y: Y, z: Z }
 * @end verbatim
 */
Tootsville.Game.Gatekeeper.beam = function (gram)
{ if (! gram.status)
    Tootsville.Game.Gatekeeper._err (gram);
  const world = gram.world;
  const latitude = gram.latitude;
  const longitude = gram.longitude;
  const altitude = gram.altitude;
  const x = gram.x || 0;
  const y = gram.y || 0;
  const z = gram.z || 0;
  Tootsville.Game.Nav.enterArea (latitude, longitude, altitude, world, x, y, z); };

/**
 * Player has received money (peanuts) or fairy dust.
 *
 * TODO: update wallet displays with an animation.
 */
Tootsville.Game.Gatekeeper.earning = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.Tank.avatars[Tootsville.character].peanuts = parseInt(gram.currency['X-TVPN']);
  Tootsville.Tank.avatars[Tootsville.character].fairyDust = parseInt(gram.currency['X-FADU']);
  return true;};

/**
 * No longer used. Ignored.
 */
Tootsville.Game.Gatekeeper.getAwardRankings = function (gram)
{ if (! gram.status)
    Tootsville.Game.Gatekeeper._err (gram);
  let ranks = gram.ranks;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * Get an apple from the server for a child's login.
 *
 * Upon receipt, create the SHA1-digest-hex password code and submit
 * an `INFINITY-LOGIN' packet.
 *
 * See  `INFINITY-GET-APPLE'  for  an  overview  of  the  login  process
 * for children.
 *
 * If status = false, retries to call @code{getApple} again.
 */
Tootsville.Game.Gatekeeper.getApple = function (gram)
{ if (gram.status)
  { let apple = gram.apple;
    let sha1 = window.forge.md.sha1.create ();
    sha1.update (apple);
    sha1.update (Tootsville.childCode);
    Tootsville.Util.infinity ("login", { userName: Tootsville.character,
                                         password: sha1.digest ().toHex (),
                                         zone: "$Eden" }); }
  else
  { Tootsville.Util.infinity ("getApple"); } };

/**
 * Response to a login attempt (for a child).
 *
 * On success, displays the @code{child-wait} overlay, awaiting
 * parental approval.
 *
 * On failure, the gossip parrots reveal the error message and
 * detailed error code.
 */
Tootsville.Game.Gatekeeper.login = function (gram)
{ if (gram.status)
  { Tootsville.UI.HUD.showHUDPanel ('child-wait');
    Tootsville.UI.endBackgroundMusic (); }
  else
  { Tootsville.Gossip.Parrot.say ("Unable to log in",
                                  gram.msg + "<small> (code " + gram.err2 + ")</small>"); } };

/**
 * Receive parent approval for a child's Toot to log in.
 *
 * @verbatim
 * { from: "parentApproval",
 *   status: true,
 *   until: 1611945540 }
 *
 * { from: "parentApproval",
 *   status: false }
 * @end verbatim
 *
 * If @code{status} is @code{true}, the parent has approved;
 * if @code{status} is @code{false}, then the parent has denied
 * permission.
 *
 * The time until which the player is allowed to play is passed
 * in @code{until} as a Unix time
 */
Tootsville.Game.Gatekeeper.parentApproval = function (gram)
{ if (gram.status)
  { Tootsville.Gossip.Parrot.say ("Let's play",
                                  "It's OK for you to play in Tootsville");
    Tootsville.playUntil = gram.until;
    document.getElementById('time-left').style.display = 'block'; }
  else
  { Tootsville.Gossip.Parrot.say ("Sorry",
                                  "You do not have permission to play in Tootsville right now. Talk to your parent or guardian."); } };

/**
 * A store item has been clicked on; ask the user if they want to buy it.
 *
 */
Tootsville.Game.Gatekeeper.getStoreItems = function (gram)
{ const totalPeanuts = gram.totalPeanuts;
  // XXX check the datagram layout
  const stores = gram.stores;
  const item = gram.stores[0].storeItems[0];
  const currency = ( (item.currency === 'X-TvPn')
                     ? '🥜' :
                     (item.currency === 'X-FaDu')
                     ? '⁂' : item.currency );
  Tootsville.UI.confirmPretty (
      'Buy Item?',
      `Do you want to buy <Q>${item.n}</Q> for ${item.price} ${currency}?`,
      "Buy", "Don't Buy").then
  (confirm => {
      if (confirm)
          // XXX check the params layout
          Tootsville.Util.infinity('startEvent', {d: {buy: item.id }});});};

/**
 * Public message (speech)
 *
 * See `INFINITY-SPEAK' for how speech is generated, or `TOOT-SPEAK'.
 *
 * The @code{pub} packet contains the speaker's name and UUID, the
 * text spoken, and (optionally) whether it was whispered or shouted.
 *
 * @subsection Example
 *
 * @verbatim
 * { from: "pub",
 *   status: true,
 *   u: "Toot-name",
 *   t: "This is what I say",
 *   x: "whisper",
 *   id: "2259E5F5-CDED-4A6A-AE68-1C4BA481CB7C" }
 * @end verbatim
 *
 */
Tootsville.Game.Gatekeeper.pub = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  if (gram.id === Tootsville.characterUUID)
      Tootsville.Game.Speech.say (gram.t, gram.x);
  else
      Tootsville.Game.Speech.say (gram.t, gram.x, gram.u); };

/**
 * Not currently used.
 */
Tootsville.Game.Gatekeeper.purchase = function (gram)
{ let totalPeanuts = gram.totalPeanuts;
  if (! gram.status )
      return Tootsville.Game.Gatekeeper._err (gram);
  let bought = gram.bought;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * No longer handled by ∞ mode protocols; now, fetched directly from the
 * game server over REST API. FIXME not necessarily true
 */
Tootsville.Game.Gatekeeper.inventory = function (gram)
{ let inv = gram.inv;
  let type = gram.type;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.Game.Gatekeeper.ping = function (gram)
{ if (gram.ping === 'ping')
  { Tootsville.Util.infinity ("ping"); }
  else if (gram.ping === 'pong')
  { if (gram.pingStarted)
    { Tootsville.Game.lag = Tootsville.Game.now - gram.pingStarted;
      console.info ("Received Ping-Pong with " + Tootsville.Game.lag + " lag"); }
    else
    { console.info ("Received Ping-Pong without lag info"); } } };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.getColorPalettes = function (gram)
{ Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * Obtains  visible information  about one  avatar at  a time;  not only
 * clothing, but any equipped object.
 */
Tootsville.Game.Gatekeeper.wardrobe = function (gram)
{ let avatar = gram.avatar;
  Tootsville.Game.acceptAvatar (avatar); };

/**
 * Add a room (including a first room) to a user's house/lot.
 *
 * UNIMPLEMENTED. it's unclear if this is actually needed on the
 * client side regardless.
 */
Tootsville.Game.Gatekeeper.initUserRoom = function (gram)
{ let successP = gram.status;
  if (successP)
  { let world = gram.moniker;
    Tootsville.warn ("unhandled datagram", gram); }
  else
  { let err = gram.err;
    if ("showFirstRun" === err)
    { Tootsville.warn ("ancient datagram now ignored", gram); }
    else if ("exists" === err)
    { let world = gram.moniker;
      Tootsville.warn ("unhandled datagram", gram); }}};

/**
 * Get a list of lots and house types which are available for starter houses
 * to build on a lot.
 *
 * @table @code
 * @item status
 * This packet is ignored unless @code{status} is @code{true}
 * @item lots
 * The list of available lots in the given neighborhood.
 * FIXME: format?
 * @item houses
 * The list of available houses in the given neighborhood.
 * FIXME: format?
 * @item neighborhood
 * The selected neighborhood.
 * @end table
 */
Tootsville.Game.Gatekeeper.getAvailableHouses = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let lots = gram.lots;
  let houses = gram.houses;
  let neighborhood = gram.neighborhood;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Get an enumeration of messages in the player's SMS ``inbox''
 *
 * @table @code
 * @item status
 * If @code{true}, there are messages.
 * If @code{false}, there are no messages and the @code{mail}
 * object is not present.
 * @item mail
 * An enumeration of message objects. Each message object
 * has FIXME format.
 * @end table
 *
 * UNIMPLEMENTED.
 *
 * See `INFINITY-GET-MAIL-IN-BOX'
 */
Tootsville.Game.Gatekeeper.getMailInBox = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let mail = gram.mail;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Fetch one SMS message by UUID.
 *
 * @table @code
 * @item status
 * If @code{true}, a message was found.
 * @item message
 * The message object. This contains the
 * message uuid, sender, and body in FIXME format.
 * @end table
 *
 * See `INFINITY-GET-MAIL-IN-BOX'
 */
Tootsville.Game.Gatekeeper.getMailMessage = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let message = gram.message;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Confirmation that a message were sent.
 */
Tootsville.Game.Gatekeeper.sendMailMessage = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Notification of new SMS message(s)
 */
Tootsville.Game.Gatekeeper.postman = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let newMailP = gram.newMail;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * The user's buddy list and ignore list.
 *
 * @table @code
 * @item buddyList
 * Buddy list
 * @item ignoreList
 * Ignore List
 * @end table
 */
Tootsville.Game.Gatekeeper.getUserLists = function (gram)
{ const hudPanel = Tootsville.UI.HUD.getOpenPanel ();
  if (hudPanel && 'player-card' === hudPanel.id)
      Tootsville.UI.HUD.showPlayerCard (hudPanel.querySelector ('h2').getAttribute ('data-player-name'));
  Tootsville.buddyList = gram.buddyList || [];
  Tootsville.ignoreList = gram.ignoreList || [];
  let starredContacts = document.getElementById ('starred-contacts');
  let unstarredContacts = document.getElementById ('unstarred-contacts');
  if (starredContacts) {
      starredContacts.innerHTML = '';
      unstarredContacts.innerHTML = '';
      for (let i = 0; i < Tootsville.buddyList.length; ++i) {
          let buddy = Tootsville.buddyList [ i ];
          let buddyLI = document.createElement ('LI');
          if (Tootsville.buddyList [i].onlineP)
              buddyLI.innerHTML = `<B>${buddy.n}</B>`;
          else
              buddyLI.innerHTML = buddy.n;
          buddyLI.setAttribute('data-buddy', buddy.n);
          buddyLI.setAttribute('data-online-p', buddy.onlineP);
          buddyLI.setAttribute('data-starred-p', buddy.starredP);
          buddyLI.onclick = event => { Tootsville.Game.buddyClicked (event); };
          if (buddy.starredP)
              starredContacts.appendChild (buddyLI);
          else
              unstarredContacts.appendChild (buddyLI); }}};

/**
 * Display a notification about a buddy list action
 */
Tootsville.Game.Gatekeeper.buddyList = function (gram)
{ let notice = gram.notice;
  Tootsville.UI.confirmPretty ("Contacts", notice, "O.K.", null);};

/**
 * You have been requested to be someone's buddy.
 *
 * See `INFINITY-REQUEST-BUDDY'
 *
 * @subsection Example
 *
 * @verbatim
 * { from: "buddyRequest",
 *   status: true,
 *   sender: "Pil",
 *   signature: "xyzzyfoo"
 * @end verbatim
 */
Tootsville.Game.Gatekeeper.buddyRequest = function (gram)
{ let sender = gram.sender;
  let signature = gram.sign;
  if (sender === Tootsville.character)
      return;
  else
      Tootsville.UI.confirmPretty (
          "Contact Request",
          sender + " wants to add you as a Contact.",
          "Add Contact", "Don't Add").then (
              outcome => {
                  if (outcome)
                      Tootsville.Util.infinity ('requestBuddy',
                                                { buddy: sender,
                                                  sign: signature });});};

/**
 * General out-of-band messaging between users. Typically used for
 * invitation to a location.
 *
 * @table @code
 * @item type
 * The type of out-of-band message
 * @item body
 * The body of that message
 * @item status
 * Must be @code{true} or the message will be ignored.
 * @end table
 *
 * The contents of the @code{body} vary by @code{type}
 *
 * @subsection Invitation
 *
 * An invitation has type @code{invite}.
 *
 * The body contains FIXME.
 *
 * @subsection Response
 *
 * WRITEME
 *
 * @subsection To Room
 *
 * WRITEME
 *
 */
Tootsville.Game.Gatekeeper.outOfBand = function (gram)
{ let type = gram.type;
  if ("invite" === type)
  { let body = gram.body;
    Tootsville.warn ("unhandled datagram", gram);  }
  else if ("response" === type)
  { let body = gram.body;
    Tootsville.warn ("unhandled datagram", gram);  }
  else if (true === gram.body.toRoom)
  { let vars = gram.body.vars;
    Tootsville.warn ("unhandled datagram", gram);  }
  else
  { Tootsville.warn("Unhandled out-of-band message type " + type); } };

/**
 * An administrative message
 *
 * This message is presented by  the Gossip Parrots in the UI.
 *
 * @table @code
 * @item title
 * The title of the message.
 * @item message
 * The body of the message.
 * @item label
 * The label of the message.
 * @end table
 */
Tootsville.Game.Gatekeeper.admin = function (gram)
{ let title = gram.title;
  let message = gram.message;
  let label = gram.label;
  Tootsville.Gossip.Parrot.say (title, message, label); };

/**
 * Received acknowledgement of the server's time.
 *
 * @table @code
 * @item status
 * should be @code{true}
 * @item serverTime
 * In milliseconds since Unix epoch
 * @item gameTime
 * In milliseconds since Unix epoch
 * @end table
 *
 * UNIMPLEMENTED. Should show the game timer, if present.
 *
 * UNIMPLEMENTED. Should update estimated server lag?
 */
Tootsville.Game.Gatekeeper.serverTime = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let serverTime = gram.serverTime;
  let gameTime = gram.gameTime;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * We no longer have badges
 */
Tootsville.Game.Gatekeeper.badgeUpdate = function (gram)
{ let badges = gram.badges;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * Force the character to move to (x,y,z) local.
 *
 * @table @code
 * @item status
 * Must be @code{true}
 * @item x,y,z
 * Local co@"ordinates
 * @end table
 */
Tootsville.Game.Gatekeeper.forceMove = function (gram)
{ let x = gram.x;
  let y = gram.y;
  let z = gram.z;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Acknowledgment of a bug report
 */
Tootsville.Game.Gatekeeper.reportBug = function (gram)
{ Tootsville.warn ("unhandled datagram", gram); };

/**
 * Receive a list of Toots from the server.
 *
 * When ``status'' is false, the user has no Toots.
 */
Tootsville.Game.Gatekeeper.tootList = function (gram)
{ if (gram.status)
    Tootsville.Login.saveTootsList (gram.toots);
  else
      Tootsville.Login.startCharacterCreation (); };

/**
 * The server has  given us ``permission'' to play as  the Toot we asked
 * for — i.e. the second step of signing in is complete.
 */
Tootsville.Game.Gatekeeper.playWith = function (gram)
{ if (gram.status)
  { Tootsville.characterUUID = gram.uuid;
    Tootsville.character = gram.playWith;
    Tootsville.player = gram.player;
    Tootsville.Tank.start3D ();
    document.getElementById('toolbox-mobile-phone').style.visibility = 'visible';
    Tootsville.Util.infinity('getUserLists'); }
  else
      Tootsville.Gossip.Parrot.say ("You can't play right now", gram.error); };

/**
 * Load a new Javascript file pushed from the server.
 *
 * This is potentially useful for patching the game on-the-fly.
 *
 * The expected syntax is something like:
 *
 * @verbatim
 * { from: "newScript",
 *   status: true,
 *   script: "/path/to/script.js" }
 * @end verbatim
 *
 * See `TOOTSVILLE-USER::PUSH-SCRIPT' for one way to generate these
 * packets.
 */
Tootsville.Game.Gatekeeper.newScript = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  if (gram.script)
      Tootsville.Util.loadScript (gram.script); };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.joinOK = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  if (gram.uLs === Tootsville.characterUUID)
  { console.log ("I have joined " + gram.r);
    Tootsville.Util.infinity ("getRoomVars"); }
  else
  { console.log (gram.n + " has joined " + gram.r);
    Tootsville.Game.Nav.sendWTL ();
    if (Tootsville.Tank.avatars)
    { if (Tootsville.Tank.avatars [ gram.n ])
      { if (Tootsville.Tank.avatars [ gram.n ].model.course )
          Tootsville.Util.infinity ("wtl4",
                                    { u: gram.n,
                                      course: Tootsville.Tank.avatars [ gram.n ].model.course,
                                      facing: Tootsville.Tank.avatars [ gram.n ].model.facing });
        else if ( Tootsville.Tank.avatars [ gram.n ].model.position )
            Tootsville.Util.infinity ("wtl4",
                                      { u: gram.n,
                                        course: { startPoint: Tootsville.Tank.avatars [ gram.n ].model.position,
                                                  endPoint: Tootsville.Tank.avatars [ gram.n ].model.position,
                                                  startTime: Tootsville.Game.now },
                                        facing: Tootsville.Tank.avatars [ gram.n ].model.facing });}
      else
          Tootsville.Tank.avatars [ gram.n ] = { name: gram.n, uuid: gram.uLs }; }}};

/**
 * Walk The Line
 *
 * This is the method that Romance 2.0 uses for moving avatars in the
 * game world.
 *
 * See also: `INFINITY-WTL'
 *
 * There are some bookkeeping side-effects here:
 *
 * @itemize
 *
 * @item
 * If the avatar named is not already known, we'll try to look them
 * up with @code{finger}, but we'll also try to remember their WTL course.
 *
 * @item
 * If the name and UUID for a character mismatch, we'll discard the
 * packet.
 *
 * @item
 * If the avatar is known, but has no associated model, we'll try to
 * build the model.
 *
 * @end itemize
 *
 * These are basically part of the attempt to synchronize the client
 * in the presence of missing information, which should not happen
 * often.
 */
Tootsville.Game.Gatekeeper.wtl = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let avatar = Tootsville.Tank.avatars [ gram.n ];
  if (! avatar)
  { console.warn ("Unexpected WTL from surprise user " + gram.n);
    Tootsville.Tank.avatars [ gram.n ] = { name: gram.n,
                                           uuid: gram.u,
                                           course: gram.course,
                                           facing: gram.facing };
    Tootsville.Util.infinity("finger", { walker: gram.n });
    return; }
  if (! (avatar.uuid === gram.u))
  { console.warn ("UUID mismatch, not walking the line", gram);
    return; }
  if (! avatar.model)
      Tootsville.AvatarBuilder.build (avatar);
  avatar.course = gram.course;
  avatar.facing = gram.facing; };

/**
 * The user has left the game.
 *
 * The user named @code{n} with UUID @code{u} has left the game.
 *
 * This destroys their avatar.
 */
Tootsville.Game.Gatekeeper.bye = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  let avatar = Tootsville.Tank.avatars [ gram.n ];
  if (gram.u === Tootsville.characterUUID || gram.n === Tootsville.character)
  { console.error ("Received a “bye” packet for myself", gram);
    return; }
  if (! (avatar && avatar.uuid === gram.u))
  { console.warn ("UUID mismatch, not destroying avatar. may be a zombie", gram);
    return; }
  Tootsville.Tank.destroyAvatar (avatar); };

/**
 * Handle and report errors from the command processor to the user.
 *
 * This may be removed in the final release, but it's mighty convenient for debugging now.
 * These errors should also be reported to
 */
Tootsville.Game.Gatekeeper.c = function (gram)
{ if (gram.status)
    console.info ("Command processor reports success", gram);
  else {
      console.error ("Command processor reports error", gram);
      Tootsville.Gossip.Parrot.say ("Server error", `
<P>The server reports that this game made a mistake while trying to <q>${gram.command}:</q> </P>
<BLOCKQUOTE ID="error-text-for-parrot"></BLOCKQUOTE>
<A HREF="https://wiki.tootsville.org/wiki/Server_command_processor_error">Learn more…</A>
`);
      document.getElementById('error-text-for-parrot').innerText = gram.error;
  } };

/**
 * This  packet  instructs  the  user  to  leave  the  game  and  go  to
 * a different web site.
 *
 * The packet's @code{url} is immediately loaded, kicking them out of
 * Tootsville.
 */
Tootsville.Game.Gatekeeper.goToWeb = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  document.location = gram.url; };

/**
 * Respond to AYT (Are You There) inquiry
 *
 * If the server sees no activity for a long time, it'll send an Are You
 * There (ayt)  packet to  verify that  the client  isn't just  a zombie
 * connection. We reply with a ping to show some activity.
 */
Tootsville.Game.Gatekeeper.ayt = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.Util.infinity ("ping"); };

/**
 * React to ``room variables''.
 *
 * See `LOCAL-ROOM-VARS' for a discussion. Room variables are a general
 * communication channel of miscellaneous information about the game
 * world. When received, they are interpreted to change or update
 * information about the player's surroundings.
 *
 * Currently, the client supports the following room variable types:
 *
 * @code{s}ky,                @code{w}eather,               @code{f}loor
 * (ignored), @code{m}usic, @code{item}, @code{itm2},
 * @code{furn}iture, @code{text}, @code{zone} (place).
 *
 * Destruction of objects is UNIMPLEMENTED in the client currently (TODO)
 *
 * @subsection See Also
 *
 * `INFINITY-GET-ROOM-VARS', `Tootsville.SkyBuilder.buildMatchingSky',
 * `Tootsville.SkyBuilder.buildMatchingWeather',
 * `Tootsville.UI.setMusicForArea',
 * `Tootsville.SceneBuilder.addItem1',
 * `Tootsville.SceneBuilder.addItem2',
 * `Tootsville.SceneBuilder.addFurn',
 * `Tootsville.SceneBuilder.addText',
 * `Tootsville.SceneBuilder.addPlace'
 */
Tootsville.Game.Gatekeeper.rv = function (gram)
{ if (gram.status)
  { Tootsville.SceneBuilder.beginRefresh ();
    for (let key in gram.var)
    { if ('s' === key) { Tootsville.SkyBuilder.buildMatchingSky (gram.var.s); }
      else if ('w' === key) { Tootsville.SkyBuilder.buildMatchingWeather (gram.var.w); }
      else if ('f' === key) {}
      else if ('m' === key) { Tootsville.UI.setMusicForArea (gram.var.m); }
      else if (key.startsWith ("itm2")) { Tootsville.SceneBuilder.addItem2 (gram.var [ key ]); }
      else if (key.startsWith ("item")) { Tootsville.SceneBuilder.addItem1 (gram.var [ key ]); }
      else if (key.startsWith ('furn')) { Tootsville.SceneBuilder.addFurn (gram.var [ key ]); }
      else if (key.startsWith ('text')) { Tootsville.SceneBuilder.addText (gram.var [ key ]); }
      else if (key.startsWith ('zone')) { Tootsville.SceneBuilder.addPlace (key, gram.var [ key ]); }
      else
      { console.warn ("Unrecognized room var: " + key, gram [ key ]); }}
    Tootsville.GroundBuilder.build ();
    Tootsville.SceneBuilder.endRefresh (); }
  else {
      if (gram.var.mist)
          Tootsville.SceneBuilder.buildMistScene (); }};


/**
 * Display a server-pushed prompt and prepare to reply.
 *
 * See `INFINITY-PROMPT-REPLY' for a discussion of the prompt system and
 * the format of this datagram.
 *
 * See `Tootsville.UI.makePrompt' for the implementation of the prompt
 * dialog builder.
 */
Tootsville.Game.Gatekeeper.prompt = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.UI.makePrompt (gram,
                            reply => {
                                Tootsville.Util.infinity ('promptReply',
                                                          { reply: reply, id: gram.id }); }); };

/**
 * Respond to a request to quiesce.
 *
 * See `INFINITY-QUIESCE', `Tootsville.Game.Nav.quiesce'
 */
Tootsville.Game.Gatekeeper.quiesce = function (gram)
{ if (gram.status)
    console.log ("Quiesced");
  else
      Tootsville.Game.Nav.quiesce (); };

/**
 * The user has been kicked out. Quit the game.
 *
 * TODO kick reason display
 */
Tootsville.Game.Gatekeeper.kick = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.Login.quit (); };

/**
 * Burgeon the Toot on logging back in.
 *
 * TODO extended attributes are ignored
 *
 * XXX @code{d3} objects are ignored. @code{wtl} is the only supported
 * course type for now.
 */
Tootsville.Game.Gatekeeper.burgeon = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.activity.world = gram.world;
  Tootsville.activity.lat = gram.latitude;
  Tootsville.activity.long = gram.longitude;
  Tootsville.activity.alt = gram.altitude;
  if (gram.wtl)
  { Tootsville.Tank.avatars [ Tootsville.character ].course = gram.wtl.course;
    Tootsville.Tank.avatars [ Tootsville.character ].facing = gram.wtl.facing; }
  else console.warn ("No WTL received in burgeon packet");
  if (gram.d3) { console.warn ("d₃ packet ignored while burgeoning"); }
  Tootsville.Tank.avatars [ Tootsville.character ].peanuts = 0 + gram.peanuts;
  Tootsville.Tank.avatars [ Tootsville.character ].fairyDust = 0 + gram.fairyDust;
  if (gram.attribs) { console.warn ("burgeon extended attributes ignored"); } };

/**
 * Migrate from the current websockets server to another one.
 *
 * Optional parameter `newConnection' can be used to provide a new
 * ``Tootsville.host'' @code{game} URL.  Normally, though, this will be
 * @code{#same}, indicating no change --- the load balancers should take
 * care of things.
 */
Tootsville.Game.Gatekeeper.migrate = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  if (gram.newConnection && "#same" !== gram.newConnection)
      Tootsville.host.game = gram.newConnection;
  if (! Tootsville.Util.Websocket)
      Tootsville.Util.checkStream ();
  Tootsville.Util.Websocket.onclose = (event) => { Tootsville.Util.connectWebSocket (); };
  Tootsville.Util.Websocket.close (1001, "migrate"); };

/**
 * roomJoin WRITEME
 */
Tootsville.Game.Gatekeeper.roomJoin = function (gram)
{ if (! gram.status )
    return Tootsville.Game.Gatekeeper._err (gram);
  Tootsville.activity.lat = gram.lat;
  Tootsville.activity.long = gram.long;
  Tootsville.activity.alt = gram.alt;
  Tootsville.activity.world = gram.world;
  return true; };

/**
 *WRITEME
 */
Tootsville.Game.Gatekeeper.addToList = function (gram)
{ if (gram.status && gram.ignore)
    Tootsville.UI.confirmPretty ('Ignore',
                                 'Now ignoring ' + gram.ignore,
                                 'Ssh!', null);
  if (!(gram.status))
      Tootsville.UI.confirmPretty ('Ignore',
                                   gram.error,
                                   'Ssh!', null); };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.removeFromList = function (gram)
{ if (gram.status)
  { if (gram.ignore)
      Tootsville.UI.confirmPretty ('Ignore',
                                   'You are now paying attention to ' + gram.ignore,
                                   'O.K.', null);
    else if (gram.buddy)
        Tootsville.UI.confirmPretty ('Contact',
                                     'Removed ' + gram.buddy + ' from your Contacts' ,
                                     'Bye!', null);
  }
  else
      Tootsville.UI.confirmPretty ("Can't Remove",
                                   gram.error,
                                   'Oops!', null); };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.readMap = function (gram) {
    if (! gram.status )
        return Tootsville.Game.Gatekeeper._err (gram);
    Tootsville.activity.badges = gram.badges;
    Tootsville.activity.spots = gram.spots;
    Tootsville.UI.HUD.refreshMapBadges ();
    return true; };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.getInventory = function (gram) {
    if (! gram.status )
        return Tootsville.Game.Gatekeeper._err (gram);
    Tootsville.activity.inv = gram.inv;
    Tootsville.UI.HUD.refreshInventory ();
    return true;};


/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.don = function (gram) {
    if (!gram.status)
        return Tootsville.Game.Gatekeeper._err (gram);
    return true; };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.doff = function (gram) {
    if (!gram.status)
        return Tootsville.Game.Gatekeeper._err (gram);
    return true; };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.drop = function (gram) {
    if (!gram.status)
        return Tootsville.Game.Gatekeeper._err (gram);
    return true; };
