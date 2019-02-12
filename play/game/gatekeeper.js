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


Tootsville.game.gatekeeper.logOK = function (memo)
{ Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.avatars = function (memo)
{ var world = memo.inRoom;
  var avatars = memo.avatars;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.bots = function (memo)
{ var bots = memo.bots;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.passport = function (memo)
{ var passport = memo.passport;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.startEvent = function (memo)
{ var eventMoniker = memo.moniker;
  var eventID = memo.eventID;
  var scriptLanguage = memo.script || 'ActionScript';
  var scriptVersion = memo.asVersion;
  var vitOnlyP = memo.vitOnly;
  var successP = memo.status;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.scoreUpdate = function (memo)
{ var successP = memo.status;
  if (! successP ) { return; }
  var rank = memo.place;
  var score = memo.score;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.endEvent = function (memo)
{ var successP = memo.status;
  if (! successP ) { return; }
  var eventID = memo.eventID;
  var peanuts = memo.peanuts;
  var totalPeanuts = memo.totalPeanuts;
  var canceledP = memo.canceled;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.gameAction = function (memo)
{ var data = memo.data;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.beam = function (memo)
{     var world = memo.room;
      var x = memo.x;
      var y = memo.y;
      var z = memo.z;
      Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.earning = function (memo)
{     
    Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getAwardRankings = function (memo)
{ var ranks = memo.ranks;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getStoreItems = function (memo)
{ var totalPeanuts = memo.totalPeanuts;
  var stores = memo.stores;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.purchase = function (memo)
{ var totalPeanuts = memo.totalPeanuts;
  var successP = memo.status;
  if (! successP ) { return; }
  var bought = memo.bought;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.inventory = function (memo)
{ var inv = memo.inv;
  var type = memo.type;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getColorPalettes = function (memo)
{ var baseColors = memo.baseColors;
  var padColors = memo.extraColors;
  var patternColors = memo.patternColors;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.wardrobe = function (memo)
{ var avatar = memo.avatar;
  var userName = memo.avatar.userName;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.initUserRoom = function (memo)
{ var successP = memo.status;
  if (successP)
  { var world = memo.moniker;
    Tootsville.warn ("unhandled datagram", memo); }
  else
  { var err = memo.err;
    if ("showFirstRun" == err)
    {
        Tootsville.warn ("unhandled datagram", memo); }
    else if ("exists" == err)
    { var world = memo.moniker;
      Tootsville.warn ("unhandled datagram", memo); }}};

Tootsville.game.gatekeeper.getAvailableHouses = function (memo)
{ var successP = memo.status;
  if (! successP ) { return; }
  var lots = memo.lots;
  var houses = memo.houses;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getMainInBox = function (memo)
{ var successP = memo.status;
  var mail;
  if (successP)
  { mail = memo.mail; }
  else
  { mail = []; }
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getMailMessage = function (memo)
{ var successP = memo.status;
  if (! successP ) { return; }
  var message = memo.message;
  Tootsville.warn ("unhandled datagram", memo);}

Tootsville.game.gatekeeper.sendMailMessage = function (memo)
{ var successP = memo.status;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.postman = function (memo)
{ var newMailP = memo.newMail;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.getUserLists = function (memo)
{ var buddies = memo.buddyList || [];
  var starred = memo.starred || [];
  var ignored = memo.ignoreList || [];
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.buddyList = function (memo)
{ var notice = memo.notice;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.buddyRequest = function (memo)
{ var sender = memo.sender;
  var signature = memo.sign;
  Tootsville.warn ("unhandled datagram", memo);};

Tootsville.game.gatekeeper.outOfBand = function (memo)
{ var type = memo.type;
  if ("invite" == type)
  { var body = memo.body;
    Tootsville.warn ("unhandled datagram", memo);  }
  else if ("response" == type)
  { var body = memo.body;
    Tootsville.warn ("unhandled datagram", memo);  }
  else if (true === memo.body.toRoom)
  { var vars = memo.body.vars;
    Tootsville.warn ("unhandled datagram", memo);  }
  else
  { Tootsville.warn("Unhandled out-of-band message type " + type); } };

Tootsville.game.gatekeeper.admin = function (memo)
{ var title = memo.title;
  var message = memo.message;
  var label = memo.label;
  // FIXME call to parrot ⋯
  Tootsville.gossip.parrot.say (title, message, label); };

Tootsville.game.gatekeeper.serverTime = function (memo)
{ var successP = memo.status;
  var serverTime = memo.serverTime;
  var gameTime = memo.gameTime;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.badgeUpdate = function (memo)
{ var badges = memo.badges;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.forceMove = function (memo)
{ var x = memo.x;
  var y = memo.y;
  var z = memo.z;
  Tootsville.warn ("unhandled datagram", memo); };

Tootsville.game.gatekeeper.reportBug = function (memo)
{ Tootsville.warn ("unhandled datagram", memo); };


