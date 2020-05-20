/* -*- js2 -*- */

/**@license
 *
 * play/Game/Gatekeeper.js is part of Tootsville
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
 * Acknowledge  a new  player's login  and introduce  yourself as  a new
 * next-hop neighbor.
 *
 * neighbor: next-hop neighbor's UUID
 *
 * Note that this message (only) uses “_cmd” as its attribute rather than
 * “c” or “from” for historical reasons.
 *
 * This message is usually unicast.
 */
Tootsville.Game.Gatekeeper.logOK = function (gram)
{ let neighbor = gram.neighbor;
  if ( gram.status )
  { console.log ("Logged in to servers");
    Tootsville.Gossip.ensureKeyPair ();
    if ('none' != document.getElementById ('sign-in').style.display)
    { Tootsville.Login.switchTootsView (); }
    if (gram.greet) { console.info ("Greeting", gram.greet); }
    if (gram.auth) { console.log ("Authentication", gram.auth); }
    if (gram.motd && ! ( Tootsville.seen.motd ) )
    { Tootsville.Gossip.Parrot.say ("Message of the Day", gram.motd);
      Tootsville.seen.motd = true; } }  else
  { Tootsville.Gossip.Parrot.say ("Error from login", gram.error + " due to " + gram.auth +
                                  " This will prevent you from playing. " +
                                  "Contact support if you don't understand.");
    Tootsville.Util.WebSocket.close ();
    Tootsville.Util.connectWebSocket (); }
  if (neighbor) { Tootsville.warn ("logOK not handled for peer connections", gram); } };

/**
 * Receive a  list of avatar  info that describes  an area of  the world.
 * This is one observer's set of nearby avatars or objects.
 */
Tootsville.Game.Gatekeeper.avatars = function (gram)
{ let world = gram.inRoom;
  let avatars = Object.values (gram.avatars);
  console.log ("Received info on " + avatars.length + " avatars");
  for (let i = 0; i < avatars.length; ++i)
  { let avatar = avatars [ i ];
    console.log ("Got avatar info for " + avatar.name);
    if (Tootsville.Login.toots [ avatar.name ])
    { console.log (avatar.name + " is one of my Toots");
      Tootsville.Game.Nav.mergeObjects (Tootsville.Login.toots [ avatar.name ], avatar);
      Tootsville.Login.populateTootsList (); };
    const orig = Tootsville.Tank.avatars [ avatar.name ];
    if (orig)
    { Tootsville.Game.Nav.mergeObjects (orig, avatar); }
    else { console.warn ("New avatar info for " + avatar.name);
           Tootsville.Tank.avatars [ avatar.name ] = avatar; }
    Tootsville.Tank.updateAvatarFor (avatar.name);} };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.bots = function (gram)
{ let bots = gram.bots;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.passport = function (gram)
{ let passport = gram.passport;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * Mostly just for fountains, now
 */
Tootsville.Game.Gatekeeper.startEvent = function (gram)
{ let eventMoniker = gram.moniker;
  let eventID = gram.eventID;
  let scriptLanguage = gram.script || 'ActionScript';
  let scriptVersion = gram.asVersion;
  let vitOnlyP = gram.vitOnly;
  let successP = gram.status;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Used to be used for minigame scores; no longer needed.
 */
Tootsville.Game.Gatekeeper.scoreUpdate = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
  let rank = gram.place;
  let score = gram.score;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * End   an    event   begun    by   startEvent.   Earn    peanuts   for
 * event participation.
 */
Tootsville.Game.Gatekeeper.endEvent = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
  let eventID = gram.eventID;
  let peanuts = gram.peanuts;
  let totalPeanuts = gram.totalPeanuts;
  let canceledP = gram.canceled;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * No longer in use.
 */
Tootsville.Game.Gatekeeper.gameAction = function (gram)
{ let data = gram.data;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * The player  has been teleported  to the given coördinates  and should
 * now explore  the surroundings (discover  what objects are  nearby, et
 * al.).
 *
 * The author must have the privilege to beam this player, or the signal
 * should be discarded.
 */
Tootsville.Game.Gatekeeper.beam = function (gram)
{ let world = gram.room;
  let x = gram.x;
  let y = gram.y;
  let z = gram.z;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Player has received money (peanuts) or fairy dust.
 */
Tootsville.Game.Gatekeeper.earning = function (gram)
{
    Tootsville.warn ("unhandled datagram", gram);};

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.getAwardRankings = function (gram)
{ let ranks = gram.ranks;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * WRITEME
 *
 * See  `INFINITY-GET-APPLE'  for  an  overview  of  the  login  process
 * for children.
 */
Tootsville.Game.Gatekeeper.getApple = function (gram)
{ if (gram.status)
  { let apple = gram.apple;
    let sha1 = forge.md.sha1.create ();
    sha1.update (apple);
    sha1.update (Tootsville.childCode);
    Tootsville.Util.infinity ("login", { userName: Tootsville.character,
                                         password: sha1.digest ().toHex (),
                                         zone: "$Eden" }); }
  else
  { Tootsville.Util.infinity ("getApple"); } };
  
/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.login = function (gram)
{ if (gram.status)
  { Tootsville.UI.HUD.showHUDPanel ('child-wait');
    Tootsville.UI.endBackgroundMusic (); }
  else
  { Tootsville.Gossip.Parrot.say ("Unable to log in",
                                  gram.msg + "<small> (code " + gram.err2 + ")</small>"); } };

/**
 * WRITEME — this function is not yet documented.
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
 * No longer used.
 */
Tootsville.Game.Gatekeeper.getStoreItems = function (gram)
{ let totalPeanuts = gram.totalPeanuts;
  let stores = gram.stores;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * Public message (speech)
 */
Tootsville.Game.Gatekeeper.pub = function (gram)
{ if (gram.id == Tootsville.characterUUID)
  { Tootsville.Game.Speech.say (gram.t, gram.x); }
  else
  { Tootsville.Game.Speech.say (gram.t, gram.x, gram.u); } };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.purchase = function (gram)
{ let totalPeanuts = gram.totalPeanuts;
  let successP = gram.status;
  if (! successP ) { return; }
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
{ if (gram.ping == 'ping')
  { Tootsville.Util.infinity ("ping"); }
  else if (gram.ping == 'pong')
  { if (gram.pingStarted)
    { Tootsville.Game.lag = Tootsville.Game.now - gram.pingStarted;
      console.info ("Received Ping-Pong with " + Tootsville.Game.lag + " lag"); }
    else
    { console.info ("Received Ping-Pong without lag info"); } } };

/**
 * No longer used.
 */
Tootsville.Game.Gatekeeper.getColorPalettes = function (gram)
{ let baseColors = gram.baseColors;
  let padColors = gram.extraColors;
  let patternColors = gram.patternColors;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * Obtains  visible information  about one  avatar at  a time;  not only
 * clothing, but any equipped object.
 */
Tootsville.Game.Gatekeeper.wardrobe = function (gram)
{ let avatar = gram.avatar;
  let userName = gram.avatar.userName;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Add a room (including a first room) to a user's house/lot.
 */
Tootsville.Game.Gatekeeper.initUserRoom = function (gram)
{ let successP = gram.status;
  if (successP)
  { let world = gram.moniker;
    Tootsville.warn ("unhandled datagram", gram); }
  else
  { let err = gram.err;
    if ("showFirstRun" == err)
    { Tootsville.warn ("ancient datagram now ignored", gram); }
    else if ("exists" == err)
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
 * The list of available hoses in the given neighborhood.
 * FIXME: format?
 * @item neighborhood
 * The selected neighborhood.
 * @end table
 */
Tootsville.Game.Gatekeeper.getAvailableHouses = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
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
 */
Tootsville.Game.Gatekeeper.getMailInBox = function (gram)
{ let successP = gram.status;
  let mail;
  if (successP)
  { mail = gram.mail; }
  else
  { mail = []; }
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
 */
Tootsville.Game.Gatekeeper.getMailMessage = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
  let message = gram.message;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Confirmation that a message were sent.
 */
Tootsville.Game.Gatekeeper.sendMailMessage = function (gram)
{ let successP = gram.status;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Notification of new SMS message(s)
 */
Tootsville.Game.Gatekeeper.postman = function (gram)
{ let newMailP = gram.newMail;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * The user's buddy list and ignore list.
 *
 * @table @code
 * @item buddyList
 * @item ignoreList
 * @end table
 */
Tootsville.Game.Gatekeeper.getUserLists = function (gram)
{ let buddies = gram.buddyList || [];
  let ignored = gram.ignoreList || [];
  if (document.getElementById ('starred-contacts'))
  { for (let i = 0; i < buddies.length; ++i)
    { let buddy = buddies [ i ];
      let buddyLI = document.createElement ('LI');
      buddyLI.innerText = buddy.n;
      if (buddy.starredP)
      { document.getElementById ('starred-contacts').appendChild (buddyLI); }
      else
      { document.getElementById ('unstarred-contacts').appendChild (buddyLI); } } } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.buddyList = function (gram)
{ let notice = gram.notice;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.buddyRequest = function (gram)
{ let sender = gram.sender;
  let signature = gram.sign;
  Tootsville.warn ("unhandled datagram", gram);};

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
  if ("invite" == type)
  { let body = gram.body;
    Tootsville.warn ("unhandled datagram", gram);  }
  else if ("response" == type)
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
 */
Tootsville.Game.Gatekeeper.serverTime = function (gram)
{ let successP = gram.status;
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
  { Tootsville.Login.saveTootsList (gram.toots); }
  else
  { Tootsville.Login.startCharacterCreation (); } };

/**
 * The server has  given us ``permission'' to play as  the Toot we asked
 * for — i.e. the second step of signing in is complete.
 */
Tootsville.Game.Gatekeeper.playWith = function (gram)
{ if (gram.status)
  { Tootsville.characterUUID = gram.uuid;
    Tootsville.character = gram.playWith;
    Tootsville.player = gram.player;
    Tootsville.Tank.start3D (); }
  else { Tootsville.Gossip.Parrot.say ("You can't play right now", gram.error); } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.newScript = function (gram)
{ if (gram.status && gram.script)
  { Tootsville.Util.loadScript (gram.script); } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.joinOK = function (gram)
{ if (gram.status)
  { if (gram.uLs == Tootsville.characterUUID)
    { console.log ("I have joined " + gram.r);
      Tootsville.Util.infinity ("getRoomVars"); }
    else
    { console.log (gram.n + " has joined " + gram.r);
      Tootsville.Game.Nav.sendWTL ();
      if (Tootsville.Tank.avatars)
      { if (Tootsville.Tank.avatars [ gram.n ])
        { if (Tootsville.Tank.avatars [ gram.n ].model.course )
          { Tootsville.Util.infinity ("wtl4",
                                      { u: gram.n,
                                        course: Tootsville.Tank.avatars [ gram.n ].model.course,
                                        facing: Tootsville.Tank.avatars [ gram.n ].model.facing }); }
          else if ( Tootsville.Tank.avatars [ gram.n ].model.position )
          { Tootsville.Util.infinity ("wtl4",
                                      { u: gram.n,
                                        course: { startPoint: Tootsville.Tank.avatars [ gram.n ].model.position,
                                                  endPoint: Tootsville.Tank.avatars [ gram.n ].model.position,
                                                  startTime: Tootsville.Game.now },
                                        facing: Tootsville.Tank.avatars [ gram.n ].model.facing });} }
        else
        { Tootsville.Tank.avatars [ gram.n ] = { name: gram.n, uuid: gram.uLs }; }}}}};

/**
 * Walk The Line
 *
 * WRITEME
 *
 * See alse: `INFINITY-WTL'
 */
Tootsville.Game.Gatekeeper.wtl = function (gram)
{ if (gram.status)
  { let avatar = Tootsville.Tank.avatars [ gram.n ];
    if (! avatar)
    { console.warn ("Unexpected WTL from surprise user " + gram.n);
      Tootsville.Tank.avatars [ gram.n ] = { name: gram.n,
                                             uuid: gram.u,
                                             course: gram.course,
                                             facing: gram.facing };
      Tootsville.Util.infinity("finger", { walker: gram.n });
      return; }
    if (! (avatar.uuid == gram.u))
    { console.warn ("UUID mismatch, not walking the line", gram);
      return; }
    if (! avatar.model)
    { Tootsville.AvatarBuilder.build (avatar); }
    avatar.course = gram.course;
    avatar.facing = gram.facing; } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.bye = function (gram)
{ let avatar = Tootsville.Tank.avatars [ gram.n ];
  if (! (avatar && avatar.uuid == gram.u))
  { console.warn ("UUID mismatch, not destroying avatar. may be a zombie", gram);
    return; }
  Tootsville.Tank.destroyAvatar (avatar); };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.c = function (gram)
{ if (gram.status)
  { console.info ("Command processor reports success", gram); }
  else
  { console.warn ("Command processor reports error", gram); } };

/**
 * This  packet  instructs  the  user  to  leave  the  game  and  go  to
 * a different web site.
 */
Tootsville.Game.Gatekeeper.goToWeb = function (gram)
{ if (gram.status) { document.location = gram.url; } };

/**
 * If the server sees no activity for a long time, it'll send an Are You
 * There (ayt)  packet to  verify that  the client  isn't just  a zombie
 * connection. We reply with a ping to show some activity.
 */
Tootsville.Game.Gatekeeper.ayt = function (gram)
{ if (gram.status)
  { Tootsville.Util.infinity ("ping"); } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.Game.Gatekeeper.rv = function (gram)
{ if (gram.status)
  { let radians = gram.rad;
    if (! radians)
    { console.warn ("Non-Radian facings not supported TODO"); }
    for (let key in gram.var)
    { if ('s' == key) { Tootsville.SkyBuilder.buildMatchingSky (gram.var.s); }
      else if ('w' == key) { Tootsville.SkyBuilder.buildMatchingWeather (gram.var.w); }
      else if ('f' == key) {}
      else if (key.startsWith ("itm2")) { Tootsville.SceneBuilder.addItem2 (gram.var [ key ]); }
      else if (key.startsWith ("item")) { Tootsville.SceneBuilder.addItem1 (gram.var [ key ]); }
      else if (key.startsWith ('furn')) { Tootsville.SceneBuilder.addFurn (gram.var [ key ]); }
      else if (key.startsWith ('text')) { Tootsville.SceneBuilder.addText (gram.var [ key ]); }
      else if (key.startsWith ('zone')) { Tootsville.SceneBuilder.addPlace (key, gram.var [ key ]); }
      else
      { console.warn ("Unrecognized room var: " + key, gram [ key ]); }}
    Tootsville.GroundBuilder.build (); }};


/**
 * Display a server-pushed prompt and prepare to reply.
 *
 * See `INFINITY-PROMPT-REPLY' for a discussion of the prompt system and
 * the format of this datagram.
 */
Tootsville.Game.Gatekeeper.prompt = function (gram)
{ if (gram.status)
  { Tootsville.UI.makePrompt (gram,
                              reply => {
                                  Tootsville.Util.infinity ('promptReply',
                                                            { reply: reply, id: gram.id }); }); } };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.quiesce = function (gram)
{ if (gram.status)
  { console.log ("Quiesced"); }
  else
  { Tootsville.Game.Nav.quiesce (); } };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.kick = function (gram)
{ if (gram.status)
  { Tootsville.Login.quit (); } };

/**
 * WRITEME
 */
Tootsville.Game.Gatekeeper.burgeon = function (gram)
{ if (gram.status)
  { Tootsville.activity.world = gram.world;
    Tootsville.activity.lat = gram.latitude;
    Tootsville.activity.long = gram.longitude;
    Tootsville.activity.alt = gram.altitude;
    if (gram.wtl)
    { Tootsville.Tank.avatars [ Tootsville.character ].course = gram.wtl.course;
      Tootsville.Tank.avatars [ Tootsville.character ].facing = gram.wtl.facing; }
    if (gram.d3) { console.warn ("d₃ packet ignored while burgeoning"); }
    Tootsville.Tank.avatars [ Tootsville.character ].peanuts = 0 + gram.peanuts;
    Tootsville.Tank.avatars [ Tootsville.character ].fairyDust = 0 + gram.fairyDust;
    if (gram.attribs) { console.warn ("burgeon extended attributes ignored"); } } };
