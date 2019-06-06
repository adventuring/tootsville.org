/* -*- js2 -*- */

if (!('Tootsville' in window)) { Tootsville = {game: {gatekeeper: {}}}; }
if (!('game' in Tootsville)) { Tootsville.game = {gatekeeper: {}}; }
if (!('gatekeeper' in Tootsville.game)) { Tootsville.game.gatekeeper = {}; }

Tootsville.game.dispatchDatagram = function (datagram)
{ var from = datagram.from;
  var handler;
  if ((handler = Tootsville.game.gatekeeper[ from ]))
  { handler (datagram); }
  else
  { Tootsville.warn ("Unhandled datagram from: " + from, datagram); } };


Tootsville.game.gatekeeper.logOK = function (gram)
{ Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.avatars = function (gram)
{ var world = gram.inRoom;
  var avatars = gram.avatars;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.bots = function (gram)
{ var bots = gram.bots;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.passport = function (gram)
{ var passport = gram.passport;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.startEvent = function (gram)
{ var eventMoniker = gram.moniker;
  var eventID = gram.eventID;
  var scriptLanguage = gram.script || 'ActionScript';
  var scriptVersion = gram.asVersion;
  var vitOnlyP = gram.vitOnly;
  var successP = gram.status;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.scoreUpdate = function (gram)
{ var successP = gram.status;
  if (! successP ) { return; }
  var rank = gram.place;
  var score = gram.score;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.endEvent = function (gram)
{ var successP = gram.status;
  if (! successP ) { return; }
  var eventID = gram.eventID;
  var peanuts = gram.peanuts;
  var totalPeanuts = gram.totalPeanuts;
  var canceledP = gram.canceled;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.gameAction = function (gram)
{ var data = gram.data;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.beam = function (gram)
{     var world = gram.room;
      var x = gram.x;
      var y = gram.y;
      var z = gram.z;
      Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.earning = function (gram)
{     
    Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getAwardRankings = function (gram)
{ var ranks = gram.ranks;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getStoreItems = function (gram)
{ var totalPeanuts = gram.totalPeanuts;
  var stores = gram.stores;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.purchase = function (gram)
{ var totalPeanuts = gram.totalPeanuts;
  var successP = gram.status;
  if (! successP ) { return; }
  var bought = gram.bought;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.inventory = function (gram)
{ var inv = gram.inv;
  var type = gram.type;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getColorPalettes = function (gram)
{ var baseColors = gram.baseColors;
  var padColors = gram.extraColors;
  var patternColors = gram.patternColors;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.wardrobe = function (gram)
{ var avatar = gram.avatar;
  var userName = gram.avatar.userName;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.initUserRoom = function (gram)
{ var successP = gram.status;
  if (successP)
  { var world = gram.moniker;
    Tootsville.warn ("unhandled datagram", gram); }
  else
  { var err = gram.err;
    if ("showFirstRun" == err)
    {
        Tootsville.warn ("unhandled datagram", gram); }
    else if ("exists" == err)
    { var world = gram.moniker;
      Tootsville.warn ("unhandled datagram", gram); }}};

Tootsville.game.gatekeeper.getAvailableHouses = function (gram)
{ var successP = gram.status;
  if (! successP ) { return; }
  var lots = gram.lots;
  var houses = gram.houses;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getMainInBox = function (gram)
{ var successP = gram.status;
  var mail;
  if (successP)
  { mail = gram.mail; }
  else
  { mail = []; }
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getMailMessage = function (gram)
{ var successP = gram.status;
  if (! successP ) { return; }
  var message = gram.message;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.sendMailMessage = function (gram)
{ var successP = gram.status;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.postman = function (gram)
{ var newMailP = gram.newMail;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.getUserLists = function (gram)
{ var buddies = gram.buddyList || [];
  var starred = gram.starred || [];
  var ignored = gram.ignoreList || [];
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.buddyList = function (gram)
{ var notice = gram.notice;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.buddyRequest = function (gram)
{ var sender = gram.sender;
  var signature = gram.sign;
  Tootsville.warn ("unhandled datagram", gram);};

Tootsville.game.gatekeeper.outOfBand = function (gram)
{ var type = gram.type;
  if ("invite" == type)
  { var body = gram.body;
    Tootsville.warn ("unhandled datagram", gram);  }
  else if ("response" == type)
  { var body = gram.body;
    Tootsville.warn ("unhandled datagram", gram);  }
  else if (true === gram.body.toRoom)
  { var vars = gram.body.vars;
    Tootsville.warn ("unhandled datagram", gram);  }
  else
  { Tootsville.warn("Unhandled out-of-band message type " + type); } };

Tootsville.game.gatekeeper.admin = function (gram)
{ var title = gram.title;
  var message = gram.message;
  var label = gram.label;
  // FIXME call to parrot ⋯
  Tootsville.gossip.parrot.say (title, message, label); };

Tootsville.game.gatekeeper.serverTime = function (gram)
{ var successP = gram.status;
  var serverTime = gram.serverTime;
  var gameTime = gram.gameTime;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.badgeUpdate = function (gram)
{ var badges = gram.badges;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.forceMove = function (gram)
{ var x = gram.x;
  var y = gram.y;
  var z = gram.z;
  Tootsville.warn ("unhandled datagram", gram); };

Tootsville.game.gatekeeper.reportBug = function (gram)
{ Tootsville.warn ("unhandled datagram", gram); };


