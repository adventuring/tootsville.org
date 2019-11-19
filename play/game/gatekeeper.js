/* -*- js2 -*- */

/*@license
 *
 * ./play/game/gatekeeper.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = {game: {gatekeeper: {}}}; }
if (!('game' in Tootsville)) { Tootsville.game = {gatekeeper: {}}; }
if (!('gatekeeper' in Tootsville.game)) { Tootsville.game.gatekeeper = {}; }

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
Tootsville.game.gatekeeper.logOK = function (gram)
{ let neighbor = gram.neighbor;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Receive a  list of avatar  info that describes  an area of  the world.
 * This is one observer's set of nearby avatars or objects.
 */
Tootsville.game.gatekeeper.avatars = function (gram)
{ let world = gram.inRoom;
  let avatars = gram.avatars;
  if (world == Tootsville.activity.world)
  { for (let i = 0; i < avatars.length; ++i)
    { Tootsville.game.updateAvatar (avatars [i].userName, avatars [i].avatar); } } };

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.bots = function (gram)
{ let bots = gram.bots;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.passport = function (gram)
{ let passport = gram.passport;
  Tootsville.warn ("ancient datagram now ignored", gram); };

/**
 * Mostly just for fountains, now
 */
Tootsville.game.gatekeeper.startEvent = function (gram)
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
Tootsville.game.gatekeeper.scoreUpdate = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
  let rank = gram.place;
  let score = gram.score;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * End   an    event   begun    by   startEvent.   Earn    peanuts   for
 * event participation.
 */
Tootsville.game.gatekeeper.endEvent = function (gram)
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
Tootsville.game.gatekeeper.gameAction = function (gram)
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
Tootsville.game.gatekeeper.beam = function (gram)
{ let world = gram.room;
      let x = gram.x;
      let y = gram.y;
      let z = gram.z;
      Tootsville.warn ("unhandled datagram", gram);};

/**
 * Player has received money (peanuts) or fairy dust.
 */
Tootsville.game.gatekeeper.earning = function (gram)
{     
    Tootsville.warn ("unhandled datagram", gram);};

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.getAwardRankings = function (gram)
{ let ranks = gram.ranks;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.getStoreItems = function (gram)
{ let totalPeanuts = gram.totalPeanuts;
  let stores = gram.stores;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.purchase = function (gram)
{ let totalPeanuts = gram.totalPeanuts;
  let successP = gram.status;
  if (! successP ) { return; }
  let bought = gram.bought;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * No longer handled by ∞ mode protocols; now, fetched directly from the
 * game server over REST API.
 */
Tootsville.game.gatekeeper.inventory = function (gram)
{ let inv = gram.inv;
  let type = gram.type;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * No longer used.
 */
Tootsville.game.gatekeeper.getColorPalettes = function (gram)
{ let baseColors = gram.baseColors;
  let padColors = gram.extraColors;
  let patternColors = gram.patternColors;
  Tootsville.warn ("ancient datagram now ignored", gram);};

/**
 * Obtains  visible information  about one  avatar at  a time;  not only
 * clothing, but any equipped object.
 */
Tootsville.game.gatekeeper.wardrobe = function (gram)
{ let avatar = gram.avatar;
  let userName = gram.avatar.userName;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Add a room (including a first room) to a user's house/lot.
 */
Tootsville.game.gatekeeper.initUserRoom = function (gram)
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
Tootsville.game.gatekeeper.getAvailableHouses = function (gram)
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
Tootsville.game.gatekeeper.getMailInBox = function (gram)
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
Tootsville.game.gatekeeper.getMailMessage = function (gram)
{ let successP = gram.status;
  if (! successP ) { return; }
  let message = gram.message;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Confirmation that a message were sent.
 */
Tootsville.game.gatekeeper.sendMailMessage = function (gram)
{ let successP = gram.status;
  Tootsville.warn ("unhandled datagram", gram);};

/**
 * Notification of new SMS message(s)
 */
Tootsville.game.gatekeeper.postman = function (gram)
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
Tootsville.game.gatekeeper.getUserLists = function (gram)
{ let buddies = gram.buddyList || [];
  let ignored = gram.ignoreList || [];
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.buddyList = function (gram)
{ let notice = gram.notice;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.buddyRequest = function (gram)
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
Tootsville.game.gatekeeper.outOfBand = function (gram)
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
Tootsville.game.gatekeeper.admin = function (gram)
{ let title = gram.title;
  let message = gram.message;
  let label = gram.label;
  // FIXME call to parrot ⋯
  Tootsville.gossip.parrot.say (title, message, label); };

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
Tootsville.game.gatekeeper.serverTime = function (gram)
{ let successP = gram.status;
  let serverTime = gram.serverTime;
  let gameTime = gram.gameTime;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * We no longer have badges
 */
Tootsville.game.gatekeeper.badgeUpdate = function (gram)
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
Tootsville.game.gatekeeper.forceMove = function (gram)
{ let x = gram.x;
  let y = gram.y;
  let z = gram.z;
  Tootsville.warn ("unhandled datagram", gram); };

/**
 * Acknowledgment of a bug report
 */
Tootsville.game.gatekeeper.reportBug = function (gram)
{ Tootsville.warn ("unhandled datagram", gram); };
