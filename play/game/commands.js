/* -*- js2 -*- */
/* @license 
 * play/game/commands.js is part of Tootsville
 *
 * Copyright © 2008-2017, Bruce-Robert  Pocock; Copyright © 2018, 2019,
 * the Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software: you can redistribute it and/or modify
 * it  under the  terms of  the GNU  Affero General  Public License  as
 * published by the  Free Software Foundation; either version  3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the  hope that it will be useful, but
 * WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
 * Affero General Public License for more details.
 *
 * You should  have received a  copy of  the GNU Affero  General Public
 * License    along     with    this     program.    If     not,    see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 */

/*
 * Legacy ∞ mode options
 *
 * These options were in existence for Romance 1.1 and/or 1.2. Some are
 * deprecated;   others   are  now   useless   and   report  as   much.
 * Removed options will return an HTTP  410 ``Gone'' message over REST or
 * the analogous reply over a stream connection.
 *
 * TODO a manual section explaining the differences under DEFINFINITY
 *
 * r = recipient/s
 *
 * u is a user UUID
 *
 * d is the d: element of the command 
*/

if (!('Tootsville' in window)) { Tootsville = { game: { commands: {}}}; };
if (!('game' in Tootsville)) { Tootsville.game = { commands: {}}; };
if (!('commands' in Tootsville.game)) { Tootsville.game.commands = {}; };

/**
 * Alias for `setFurniture', q.v.
 */
Tootsville.game.commands.addFurniture = function (d, u, r)
{ Tootsville.game.commands.setFurniture (d, u, r); };


/**
* No longer used
 */
Tootsville.game.commands.addToList = function (d, u, r)
{
    /* TODO */ };


/**
 * Used by the client  to report a mouse click or  finger tap.
 *
 *If the user  clicks on a placed-item, this method  should be called with
 *the following syntax:
 *
 *@example
 *{"c":"click", "d":{ "on": itemID, "x": x, "y": y, "z": z, "with": mods } }
 *@end example
 *
 *Note that the  (x,y,z) values passed are relative to  the origin point
 *of  the item;  thus, if  an  item is  placed at  (200,200,200) and  is
 *clicked  at   (210,210,210),  the   coördinates  reported   should  be
 *(10,10,10).
 *
 *@subsection{Modifiers characters}
 *
 *The mods string can contain any of the following symbols in any order,
 *representing modifier keys  that were held down when  the user clicked
 *on the item:
 *
 *@table @samp
 *@item ^
 *
 *Caret  represents  the @key{Control}  or  @key{Ctrl}  key on  Linux®  or
 *Windows systems, or the @key{Command} key on macOS™.
 *
 *@item S
 *
 *Ess represents the @key{Shift} key on any platform.
 *
 *@item C
 *
 *Ci represents  the @key{Caps Lock}  state being enabled. May  be ignored
 *or omitted.
 *
 *@item N
 *
 *En represents  the @key{Num  Lock} state being  enabled. May  be ignored
 *or omitted.
 *
 *@item M
 *
 *Em  represents  the @key{Meta}  key  on  Linux,  @key{Alt} on  Linux  or
 *Windows, or @key{Option} on macOS.
 *
 *@item L
 *
 *Ell represents the @key{Scroll Lock} state being enabled. May be ignored
 *or omitted.
 *
 *@item A
 *
 *Ay  represents the  @key{Alt-Gr}  key on  any  platform (if  supported).
 *May be ignored or omitted.
 *
 *@item *
 *
 *Asterisk represents  the @key{Super} key on  Linux or @key{Windows-Logo}
 *key on Windows.
 *
 *@item 1, 2, 3
 *Numbers represent mouse buttons: 1 for left, 2 for middle, 3 for right.
 *
 *@item +, -
 *Plus represents rolling a scroll wheel down; Minus to scroll up
 *
 *@item <, >
 *Less-than represents rolling a scroll knob left; greater-than, right.
 *
 *@subsection{Flash details}
 *
 *In the Flash MouseEvent object, you can create the "mods" with the following:
 *
 *@example
 *var mods:String = "";
 *if (ev.altKey) mods += "M";
 *if (ev.commandKey || ev.ctrlKey) mods += "^";
 *if (ev.shiftKey) mods += "S";
 *if (ev.type == ev.CLICK) mods += "1";
 *if (ev.type == ev.MIDDLE_CLICK) mods += "2";
 *if (ev.type == ev.RIGHT_CLICK) mods += "3";
 *if (ev.type == ev.MOUSE_WHEEL)
 *{ if (ev.delta < 0) mods += "-";
 *    if (ev.delta > 0) mods += "+"; }
 *if (Keyboard.numLock) mods += "N";
 *if (Keyboard.capsLock) mods += "C";
 *@end example
 *
 *@subsection{Changes from 1.2}
 *
 *@itemize
 *
 *@item
 *
 *The @samp{z} coördinate is no longer optional.
 *
 *@item
 *
 *A form of the @samp{click} command which omitted the @samp{on} parameter.
 *
 *@item
 *
 *The itemID is  an UUID Base64 string,  not a moniker string  — but these
 *were always meant to be opaque identifiers.
 *
 *@end itemize
 *
 *@subsection{202 Accepted}
 *
 *The  click event  has been  noted. Any  outcomes of  that event  will be
 *broadcast over other channels.
 *
 *@subsection{204 No Content}
 *
 *The click event is being ignored; ITEM-ID was not an interesting item to
 *the server.
 *
 */
Tootsville.game.commands.click = function (d, u, r)
{
    /* TODO */ };


/**
 * Either claim the user's house and lot, or add a room to their house.
 *
 *Data describing the user's lot
 *
 *@verb{| { lot: lot-ID, house: house-ID },  |}
 *
 *or adding a room, @verb{| { index: roomIndex } |}
 *
 *
 */
Tootsville.game.commands.createUserHouse = function (d, u, r)
{
    /* TODO */ };


/**
 * Doff an item
 *
 *TODO: document parameters
 *
 *TODO document Response with total avatar info from "wardrobe"
 */
Tootsville.game.commands.doff = function (d, u, r)
{
    /* TODO */ };


/**
 * Don an item
 *
 *JSON object  has the item slot  number to be worn  (clothes, patterns,
 *pivitz) and optionally set the color (for patterns)
 *
 *Response with total avatar info from
 */
Tootsville.game.commands.don = function (d, u, r)
{
    /* TODO */ };


/**
 * Echoes back the supplied JSON (or ActionScript) object to the client.
 *
 *This method exists solely for testing purposes.
 *
 *Sends response containing:
 *
 *@example
 *ECHO => Can you hear me now?
 *status => true
 *You said: => (the ActionScript object passed in)
 *@end example
 *
 *Note that the field name is literally ``@samp{You said:}.''
 *
 *Parameters:
 *jso - Any JSON object, the contents of which will be returned to the caller.
 *u - The user calling (to whom the response is sent)
 *
 *
 *
 */
Tootsville.game.commands.echo = function (d, u, r)
{
    /* TODO */ };


/**
 *  This method terminates  an event (probably a  minigame, but possibly
 *a fountain) which was initiated by startEvent.
 *
 *For fountains, the  score is ignored, and a random  number from 1..100
 *is used  as the effective score.  Since fountains (should) have  a 1:1
 *points:peanuts  ratio,  this  will  earn  the  player  1..100  peanuts
 *randomly per fountain visit.
 *
 *Response: JSON  sent to user:  { ended:  event ID; peanuts:  number of
 *peanuts earned;  highScores: array of  scores, indexed by  position on
 *the high score list (1..24), each  of which contains: { points: number
 *of points scored  by the high-scoring user; userName: the  name of the
 *user }, totalPeanuts: user's new total peanut balance }
 *
 *Additionally, if  this user earned  a high  score on this  event, s/he
 *will  get  the  attribute  in  the   top  level  of  the  response  as
 *"gotHighScore": with  the value  being the  position number  which was
 *earned. For  example, earning no  high score omits  the "gotHighScore"
 *attribute  altogether; earning  the  third highest  score will  return
 *instead "gotHighScore" == 3.
 *
 *jso - JSON parameters. { moniker  = the event's moniker; eventID = the
 *event  ID to  be  ended; score  =  the earned  score,  in points  (not
 *peanuts); status  = one of  "cxl" to cancel  an event (in  which case,
 *score should be 0),  or "cmp" to complete an event  (score may be zero
 *or more). }
 *
 *
 */
Tootsville.game.commands.endevent = function (d, u, r)
{
    /* TODO */ };


/**
 * Get public info for a list of (other) users.
 *
 *Reply format:
 *
 *{ from: avatars, status: true, avatars: { 0: { USER-INFO â€¦ }, â€¦ }
 *
 *
 *User public information is in the format of AbstractUser.getPublicInfo()
 *
 *jso - JSON object, with (ignored) keys tied to values which must be the names of users.
 *
 *
 */
Tootsville.game.commands.finger = function (d, u, r)
{
    /* TODO */ };


/**
 * gameAction(org.json.JSONObject jso,
 *                                AbstractUser u,
 *                                Room room)
 *throws org.json.JSONException
 *
 *WRITEME â€”  basically similar to  an sendOutOfBandMessage(JSONObject,
 *AbstractUser, Room) but specifically something to do with a game
 *
 *Parameters: jso - { "action": (verb), (other params...) } u - The user
 *calling this  method room -  The room in  which this user  is standing
 *Throws:  org.json.JSONException  -  Thrown   if  the  data  cannot  be
 *interpreted  from the  JSON objects  passed in,  or conversely,  if we
 *can't encode a response into a JSON form
 *
 *
 */
Tootsville.game.commands.gameAction = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the apple to get into, or out of, $Eden
 */
Tootsville.game.commands.getApple = function (d, u, r)
{
    /* TODO */ };


/**
 * Get avatar data for a list of (other) users. cv. finger
 *
 *Parameters:
 *
 *jso - JSON object, with (ignored) keys  tied to values which must be the
 *names of users. e.g. { 0:
 */
Tootsville.game.commands.getAvatars = function (d, u, r)
{
    /* TODO */ };


/**
 * getColorPalettes
 *
 *@subsection{410 Gone}
 *
 *Removed.. This  routine appeared to be  unused by anyone in  Romance 1.1
 *and was removed in 1.2.
 *
 *returns palettes  in "extraColors",  "baseColors", "patternColors"
 *in the JSON result object (from: "getColorPalettes")
 *
 *@subsection{Note}
 *
 *Not used in Tootsville any more.  The analogous palettes in Li'l Vampies
 *and Empires  of the Air are  being replaced with algorithmic  checks, so
 *this routine was removed in Appius 1.2.0.
 */
Tootsville.game.commands.getColorPalettes = function (d, u, r)
{
    /* TODO */ };


/**
 * get all inventory for an user — both active and inactive
 *
 *Returns a set of items as
 *@example
 *inv: { 0: { id: 123, isActive: boolean }, ... }
 *@end example
 *
 *furniture with  placement data  will also  have x,  y, and  facing vars.
 *Other attributes  are "from":"inventory",  "type": matching the  type of
 *the question
 *
 *d — empty
 *
 *u — The user whose inventory to be searched
 */
Tootsville.game.commands.getInventory = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a subset of items from your own inventory
 *
 *Parameters:
 *
 *jso    -    {     type:    TYPE-STRING    }    —
 *
 * JSON object has the type of item from the strings in the config file.
 *
 *OR, you can specify  an item type by passing # plus its  ID, or a string
 *of them; e.g. for items of type 1, pass "#1," for items of types 2 or 3,
 *pass "#2:3"
 *
 *OR, you  can specify  a list  of item  type strings  using '$'  plus the
 *string identifiers divided by ':', e.g. "$Pants:Shirts"
 *
 *Returns a  set of items  as inv:  { 0: {  id: 123, isActive:  boolean },
 *... } â€” furniture with placement data  will also have x, y, and facing
 *vars. Other attributes are "from":"inventory", "type": matching the type
 *of the question
 *
 *You can also supply withActive: false to screen out active items.
 *Return data
 *
 *@example
 *  { from: inventory, for: USER-LOGIN, type: TYPE-STRING,
 *    inv: {
 *     #: { ITEM-INFO },
 *      #: { ITEM-INFO } â€¦
 *    }
 *  }
 *@end example
 *
 *TODO    see    Java
 *getInventoryByType(JSONObject,  AbstractUser,  AbstractUser,  Room)  for
 *discussion of TYPE-STRING; or { type: TYPE-STRING, withActive: BOOLEAN }
 *to  mask out  active items;  optional  { who:  LOGIN-NAME }  to look  at
 *someone else's inventory
 *
 *u - The user whose inventory to be searched, who is the caller of this routine
 *
 *
 */
Tootsville.game.commands.getInventoryByType = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of users in a Zone, or in a Room.
 *
 *This is an administrative function, only available to staff members.
 *
 *Parameters:
 *
 *jso  -  The JSON  data  provided  by the  caller.  If  this contains  an
 *attribute of "inRoom"  with a room moniker, we'll only  return the users
 *in that room. Otherwise, all users in the Zone will be returned.
 *
 *u - The caller's ID. Must have staff privileges.
 *
 *room - The room from which the caller is making the extension call: ignored.
 *Throws:
 *
 *org.json.JSONException - if the JSON data can't be processed, in or out.
 *
 *PrivilegeRequiredException    -    if     the    user    doesn't    have
 *STAFF_LEVEL_STAFF_MEMBER
 *
 *NotFoundException - if the room requested doesn't exist
 *
 *
 */
Tootsville.game.commands.getOnlineUsers = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all “well known” Rooms currently active/visible.
 *
 *“Rooms” no longer exist. The “rooms” are now known as “planes.”
 *
 *The following planes exist in Tootsville:
 *
 *@itemize
 *
 *@item
 *Tootanga
 *
 *@item
 *Space
 *
 *@item
 *Moon
 *
 *@item
 *Other-Moon
 *
 *@item
 *Pink-Moon
 *
 *@end itemize
 *
 */
Tootsville.game.commands.getRoomList = function (d, u, r)
{
    /* TODO */ };


/**
 * Send the server time to the client requesting it
 *
 *For synchronization purposes.
 *
 *Sends a JSON object with a single property, serverTime, with the current
 *time in milliseconds (give or take transit time). This is the Unix time,
 *not the Universal time.
 */
Tootsville.game.commands.getservertime = function (d, u, r)
{
    /* TODO */ };


/**
 * Initialise a session key for stream or batch mode operations
 *
 *Replies with { from: initSession, key: (OPAQUE-STRING) }
 *
 *
 */
Tootsville.game.commands.getSessionApple = function (d, u, r)
{
    /* TODO */ };


/**
 * WRITEME: Document this method brpocock@star-hope.org
 *
 *jso - JavaScript array-style object where the key names are ignored, but the values are item ID's
 *
 *
 */
Tootsville.game.commands.getStoreItemInfo = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the user's buddy list and ignore list.
 *
 *{ buddyList: { … } , ignoreList: { … } }
 *
 *jso - no parameters needed
 *
 *u - The user whose buddy and ignore lists will be fetched
 *
 *
 */
Tootsville.game.commands.getUserLists = function (d, u, r)
{
    /* TODO */ };


/**
 *
 *WRITEME: Document this method brpocock@star-hope.org
 *
 *
 *
 */
Tootsville.game.commands.getWallet = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all Zones currently active/visible.
 *
 *This returns "Universe" as the only Zone.
 */
Tootsville.game.commands.getZoneList = function (d, u, r)
{
    /* TODO */ };


/**
 * Give an item to another user
 *
 *XXX: notify the recipient using notifications (currently using a Message
 *Box popup message)
 *
 *jso - { slot: SLOT-NUMBER, to: USER-LOGIN }
 *
 *u - giver
 *
 *AlreadyExistsException - if the event can't be started for some reason
 *
 *
 */
Tootsville.game.commands.give = function (d, u, r)
{
    /* TODO */ };


/**
 * go to a place and/or perform a gesture
 *
 *@example
 *
 *{ do: VERB (required) x: DEST, y: DEST, z: DEST (each optional, but if
 *  x or  y is  given, both must  be; z can  be omitted)  facing: FACING
 *  (optional) }
 *@end example
 *
 *u - the user doing something
 *
 */
Tootsville.game.commands.go = function (d, u, r)
{
    /* TODO */ };


/**
 *
 *
 *Creates room  named user/user's name/room  â€” room is the  room index
 *number given in the JSON data as  “room,” it will always be zero right
 *now  as all  users have  single-room  houses. This  will populate  all
 *furniture-type items for that room onto  a set of room variables owned
 *by the  user. The user  calling this method must  be the owner  of the
 *room. If  the user  has not  visited his/her  house before,  this will
 *return an asynchronous
 */
Tootsville.game.commands.initUserRoom = function (d, u, r)
{
    /* TODO */ };


/**
 * Join a room.  On success, sends back the set  of room join events;
 *     but on failure, replies with  { from: roomJoin, status: false, err:
 *     ...}
 *
 *  NOTE the inconsistency: the command is  join, but the reply comes from
 *  roomJoin
 *
 *zone.notFound
 *  The user is not in a Zone
 *
 *room.noMoniker
 *  No room moniker was given to be joined
 *
 *room.notFound
 *  The room moniker does not refer to an actual room in this Zone
 *
 *room.full
 *  The room is too full (too many users)
 *
 *  Parameters:
 *  jso - { room: MONIKER } or { room: MONIKER, from: MONIKER }
 *
 *u - the user joining the room
 *
 *
 */
Tootsville.game.commands.join = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle a login request
 *
 *We no longer do this …
 *    Parameters:
 *        jso - { userName: LOGIN, password: $(sha1hex(concat(apple, pass))), zone: ZONE }
 *
 *Response: logOK or { err: login.fail, msg: reason } with
 *
 */
Tootsville.game.commands.login = function (d, u, r)
{
    /* TODO */ };


/**
 * Log out of this game session (or zone)
 *
 *  There's a bug in the Persephone client that causes it to explode if we
 *  log it out  before it receives & processes the  logout message. So, we
 *  wait for the expected lag time to expire and then throw 2 full seconds
 *  of wasted wait time after it, which had ought to be enough time
 *
 *  Note: in  the future, this  will be configured  to be off  by default.
 *  Tootsville servers  will need to incorporate  the configuration key
 *  value if Persephone 2 hasn't been fixed by that time.
 *
 *
 */
Tootsville.game.commands.logout = function (d, u, r)
{
    /* TODO */ };


/**
 *   send an eMail to customer service (feedback)
 *
 *  Parameters:
 *  jso - { subject: STRING, body: STRING }
 *  u - the user sending the feedback
 *  room - the room in which the user is standing
 *
 *
 */
Tootsville.game.commands.mailCustomerService = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle looking at other user's inventories
 *
 *  Parameters: jso - {"who": the login name  of the user of whom to get
 *  the  inventory  };   optional  "type":  to  filter   by  type.  (see
 *  getInventoryByType(JSONObject,  AbstractUser, AbstractUser,  Room) for
 *  details) u  - The  user requesting  the inventory room  - The  room in
 *  which the  request occurs  Throws: org.json.JSONException -  Thrown if
 *  the data  cannot be interpreted  from the  JSON objects passed  in, or
 *  conversely,  if  we   can't  encode  a  response  into   a  JSON  form
 *  NotFoundException - Could not find a user with that name
 *
 *
 */
Tootsville.game.commands.peekAtInventory = function (d, u, r)
{
    /* TODO */ };


/**
 *   Send a ping to the server to get back a pong.
 *
 *This also updates the user's last-active timestamp, to prevent them from
 *being idled offline.
 *
 *
 */
Tootsville.game.commands.ping = function (d, u, r)
{
    /* TODO */ };


/**
 * promptReply(org.json.JSONObject jso,
 *                                   AbstractUser u,
 *                                   Room room)
 *  throws org.json.JSONException
 *
 *  Server initiates prompt with:
 *
 *@example
 *
 *  { "from" : "prompt",
 *  "id" : $ID,
 *  "label" : $LABEL,
 *  "label_en_US" : $LABEL,
 *  "title" : $TITLE,
 *  [ "attachUser" : $AVATAR_LABEL || "attachItem" : $ITEM_ID ] ,
 *  "msg" : $TEXT,
 *  "replies" :
 *  {  $TOKEN :
 *  { "label" : $BUTTON_LABEL,
 *  "label_en_US" : $BUTTON_LABEL,
 *  "type" : $BUTTON_TYPE },
 *  [ â€¦ ]
 *  }
 *  }
 *
 *@end example
 *
 *  Where:
 *
 *  $ID = arbitrary string with no 0 representing this question uniquely.
 *  This is not an user-visible string.
 *
 *  $LABEL  =  concatenated to  the  window  title,  but  can be  used  to
 *  special-case / theme dialogs in future for certain purposes
 *
 *  $TITLE = dialog title
 *
 *  Only one of either  “attachUser” or “attachItem” will be
 *  included. $AVATAR_LABEL is the full avatar label of the user/avatar to
 *  which  the prompt  should  be attached  â€”  including “$”  and
 *  instance ID, if necessary â€” where $ITEM_ID is the room variable item
 *  ID for a placed item in the room.
 *
 *  $TEXT = message text, may have  n, will often need word-wrapping, and
 *  ideally might make use of scroll bars
 *
 *  The "replies"  assoc-array is of  arbitrary length â‰¥ 2,  where the
 *  key to each item is a $TOKEN,  again an arbitrary string without 0 to
 *  represent this response uniquely. This is not an user-visible string.
 *
 *  $BUTTON_LABEL = the text to display. In future, the client may want to
 *  special-case specific text  to use icons or something:  e.g. "OK" will
 *  always be sent as precisely "OK" in English locale.
 *
 *  $BUTTON_TYPE  = the  type of  the  button for  theming purposes  only.
 *  This is from the enumerated set [ "aff" | "neg" | "neu" ];
 *
 *  aff = affirmative button, e.g. green button
 *
 *  neg = negative button, e.g. red button
 *
 *  neu = neutral button, e.g. purple button
 *
 *  To  simplify future  i18n/l10n efforts,  the $LABEL  and $BUTTON_LABEL
 *  will always be sent twice. The user's current language version will be
 *  in  the "label"  properties. The  versions of  those strings  in the
 *  "en_US"  locale will  always be  in the  "label_en_US" properties.
 *  For purposes of theming and such, the label_en_US properties should be
 *  considered; the  "label" properties, however, should  always be used
 *  in presentation to the end-user.
 *
 *  Example:
 *@example
 *
 *  { "from": "prompt", "status": "true",
 *  "id": "fountain/tootSquare/Ã¾=?/x'deadbeef'",
 *  "label": "Fountain", "label_en_US": "Fountain",
 *  "title": "Make a Wish?", "msg": "Do you want to make a wish on the Toot Square fountain?",
 *  "replies":
 *  { "yes": { "label": "Make a Wish!", "label_en_US": "Make a Wish!", "type": "aff" },
 *  "no": { "label": "Not now", "label_en_US": "Not now", "type": "neg" }
 *  }
 *  }
 *
 *@end example
 *
 *  The client's response is a bit simpler:
 *@example
 *  { "c": "promptReply", "d": { "id": $ID, "reply": $TOKEN } }
 *  @end example
 *
 *  e.g.
 *
 *  { "c":"promptReply", "d": { "id":  "fountain/tootSquare/Ã¾=?/x'deadbeef'", "reply": "yes" } }
 *
 *
 *  As a special-case, for the reply only, the special $TOKEN of "close"
 *  should  be sent  if  the  user dismissed  the  dialog  box with  the
 *  close button.
 *
 *  I'd suggest that  the GUI attach anonymous functions  with the reply
 *  packets already  constructed to the  various dialog box  controls at
 *  creation  time,  rather   than  trying  to  manage   some  queue  of
 *  pending prompts.
 *
 *  To handle user expectations, it would  be best to display the button
 *  in a  "down" state until  receiving the server's  acknowledgement of
 *  the "promptReply" and disallow multiple-clicking in the window.
 *
 *  The server will respond with
 *
 *  { "from": "promptReply", "status": "true", "id": $ID }
 *
 *
 *  For debugging purposes, the server may reply with
 *
 *  { "from": "promptReply", "status": "false", "err": $ERR }
 *
 *
 *  Where $ERR  will be a  brief description of  the problem. e.g.  $ERR =
 *  "reply.notFound" might represent a reply button that was not a valid
 *  $TOKEN from  the "prompt"  command nor  the special  case "close".
 *  $ERR =  "id.notFound" might represent a  reply to a prompt  that was
 *  not (recently) asked.
 *
 *  A prompt  ID is not valid  across sessions; pending prompts  should be
 *  auto-closed   on  logout.   Prompts   can,   however,  remain   active
 *  indefinitely, even across room joins.
 *
 *  Optional implementation:  the server may cancel  an outstanding prompt
 *  request by sending a packet with the following properties:
 *
 *  from: prompt
 *  status: true
 *  cancel: $ID
 *
 *  Client  applications may  choose to  dismiss the  prompt automatically
 *  upon  receiving such  a packet.  Failure  to do  so is  not an  error,
 *  however, later  attempting to reply  to a canceled prompt  will return
 *  status:  false, err:  id.notFound. Clients  must accept  a cancelation
 *  packet silently if they do not process it.
 *
 *  Parameters:
 *  jso - in the form { id: $ID, reply: $TOKEN }, as detailed above
 *
 *  u - the user replying to a prompt
 *
 *room - the room in which the user is standing (unimportant)
 *
 *Throws:
 *  org.json.JSONException - for really bad syntax errors
 *
 *
 */
Tootsville.game.commands.promptReply = function (d, u, r)
{
    /* TODO */ };


/**
 * Remove someone from a buddy list or ignore list.
 *
 *  jso - To remove a buddy: { buddy: (name) }; or to attend to someone who had previously been ignored: { ignore: (name) }
 *
 *  u - The user whose buddy list or ignore list will be updated
 *
 */
Tootsville.game.commands.removeFromList = function (d, u, r)
{
    /* TODO */ };


/** *  This method allows  the client  to “phone home”  to report
a bug.  The bug  report itself is  just a giant  string embedded  in the
“bug” element,  but a “cause”  element will be  treated as
the subject. Note that  the bug report â€” like all  JSON input â€” will
be cut off at a certain limit  (typically 4KiB), so it's most helpful to
keep it short & sweet: Typically, this should be something like a single
stack  backtrace  (with  as  much   detail  as  possible),  rather  than
a complete log trace or something.
 *
 *  The  suggested  usage   is  to  include  the   exception  itself  as
 *  “cause,”  the  backtrace up  to  a  maximum  of 1KiB,  a  log
 *  backtrace  up  to  its  last  1KiB  as  “bug,”  and  as  much
 *  machine-formatted   system   information    as   possible   in   the
 *  “info” object. Fields of “info”
 *
 *  As many fields as possible, limit the contents to a reasonable length thoughâ€¦
 *
 *  Note that the keys listed are strings, so e.g.:
 *
 *@example
 *  info ["navigator.language"] = navigator.language;
 *  info ["navigator.product"] = navigator.product;
 *@end example
 *
 *  ActionScript example:
 *
 *@example
 *  var info:Object = {
 *  "flash.sys.ime": flash.system.System.ime,
 *  "flash.sys.totalMemory": flash.system.System.totalMemory,
 *  "flash.sys.useCodePage": flash.system.System.useCodePage
 *  };
 *  // imperfect but close
 *  for ( var key in flash.system.Capabilities ) {
 *  info["flash.sysCap." + key] = flash.system.Capabilities[key];
 *  }
 *@end example
 *
 *@table samp
 *
 *@item navigator.language
 *  JavaScript: navigator.language
 *  @item navigator.product
 *  JavaScript: navigator.product
 *  @item navigator.appVersion
 *  JavaScript: navigator.appVersion
 *  @item navigator.platform
 *  JavaScript: navigator.platform
 *  @item navigator.vendor
 *  JavaScript: navigator.vendor
 *  @item navigator.appCodeName
 *  JavaScript: navigator.appCodeName
 *  @item navigator.cookieEnabled
 *  JavaScript: navigator.cookieEnabled
 *  @item navigator.appName
 *  JavaScript: navigator.appName
 *  @item navigator.productSub
 *  JavaScript: navigator.productSub
 *  @item navigator.userAgent
 *  JavaScript: navigator.userAgent
 *  @item navigator.vendorSub
 *  JavaScript: navigator.vendorSub
 *  @item screen.height
 *  JavaScript: screen.height; ActionScript: flash.system.Capabilities.screenResolutionX
 *  @item screen.width
 *  JavaScript: screen.width; ActionScript: flash.system.Capabilities.screenResolutionY
 *  @item screen.availHeight
 *  JavaScript: screen.availHeight; ActionScript: flash.display.Stage.fullScreenHeight
 *  @item screen.availWidth
 *  JavaScript: screen.availWidth; ActionScript: flash.display.Stage.fullScreenWidth
 *  @item window.outerHeight
 *  JavaScript: window.outerheight note case
 *  @item window.outerWidth
 *  JavaScript: window.outerwidth note case
 *  @item window.innerHeight
 *  JavaScript: window.innerheight note case
 *  @item window.innerWidth
 *  JavaScript: window.innerwidth note case
 *  @item window.windowName
 *  JavaScript: the window.name property of the highest parent of this window (frame); e.g.
 *@example
 *  var topWindow = window.parent;
 *  for (; topWindow.parent != topWindow; topWindow = topWindow.parent)
 *                                        ;
 *       info ["window.windowName"] = topWindow.name;
 *@end example
 *
 *       @item flash.sys.totalMemory
 *       ActionScript: flash.system.System.totalMemory
 *       @item flash.sys.ime
 *       ActionScript: flash.system.System.ime
 *       @item flash.sys.useCodePage
 *       ActionScript: flash.system.System.useCodePage
 *       @item flash.sysCap.avHardwareDisable
 *       ActionScript: flash.system.Capabilities.avHardwareDisable
 *       @item flash.sysCap.hasAccessibility
 *       ActionScript: flash.system.Capabilities.hasAccessibility
 *       @item flash.sysCap.hasAudio
 *       ActionScript: flash.system.Capabilities.hasAudio
 *       @item flash.sysCap.hasAudioEncoder
 *       ActionScript: flash.system.Capabilities.hasAudioEncoder
 *       @item flash.sysCap.hasEmbeddedVideo
 *       ActionScript: flash.system.Capabilities.hasEmbeddedVideo
 *       @item flash.sysCap.hasIME
 *       ActionScript: flash.system.Capabilities.hasIME
 *       @item flash.sysCap.hasMP3
 *       ActionScript: flash.system.Capabilities.hasMP3
 *       @item flash.sysCap.hasPrinting
 *       ActionScript: flash.system.Capabilities.hasPrinting
 *       @item flash.sysCap.hasScreenBroadcast
 *       ActionScript: flash.system.Capabilities.hasScreenBroadcast
 *       @item flash.sysCap.hasScreenPlayback
 *       ActionScript: flash.system.Capabilities.hasScreenPlayback
 *       @item flash.sysCap.hasStreamingAudio
 *       ActionScript: flash.system.Capabilities.hasStreamingAudio
 *       @item flash.sysCap.hasStreamingVideo
 *       ActionScript: flash.system.Capabilities.hasStreamingVideo
 *       @item flash.sysCap.hasTLS
 *       ActionScript: flash.system.Capabilities.hasTLS
 *       @item flash.sysCap.hasVideoEncoder
 *       ActionScript: flash.system.Capabilities.hasVideoEncoder
 *       @item flash.sysCap.isDebugger
 *       ActionScript: flash.system.Capabilities.isDebugger
 *       @item flash.sysCap.isEmbeddedInAcrobat
 *       ActionScript: flash.system.Capabilities.isEmbeddedInAcrobat
 *       @item flash.sysCap.language
 *       ActionScript: flash.system.Capabilities.language
 *       @item flash.sysCap.localFileReadDisable
 *       ActionScript: flash.system.Capabilities.localFileReadDisable
 *       @item flash.sysCap.manufacturer
 *       ActionScript: flash.system.Capabilities.manufacturer
 *       @item flash.sysCap.os
 *       ActionScript: flash.system.Capabilities.os
 *       @item flash.sysCap.pixelAspectRatio
 *       ActionScript: flash.system.Capabilities.pixelAspectRatio
 *       @item flash.sysCap.playerType
 *       ActionScript: flash.system.Capabilities.playerType
 *       @item flash.sysCap.screenColor
 *       ActionScript: flash.system.Capabilities.screenColor
 *       @item flash.sysCap.screenDPI
 *       ActionScript: flash.system.Capabilities.screenDPI
 *       @item flash.sysCap.version
 *       ActionScript: flash.system.Capabilities.version
 *       @item flash.displayState
 *       ActionScript: if flash.display.Stage.displayState == FULL_SCREEN_INTERACTIVE, then "fullScreen"; for NORMAL, return "window".
 *       @item flash.frameRate
 *       ActionScript: flash.display.Stage.frameRate
 *       @item flash.quality
 *       ActionScript: flash.display.Stage.quality
 *       @item flash.scaleMode
 *       ActionScript: flash.display.Stage.scaleMode
 *@end table
 *
 *@example
 *
 *       // ActionScript example
 *       function systemReport:Object () {
 *       return {
 *       "screen": {
 *       "height": flash.system.Capabilities.screenResolutionX,
 *       "width": flash.system.Capabilities.screenResolutionY,
 *       "availHeight": flash.display.Stage.fullScreenHeight,
 *       "availWidth": flash.display.Stage.fullScreenWidth,
 *       },
 *       "flash": {
 *       "sys": {
 *       "totalMemory": flash.system.System.totalMemory,
 *       "ime": flash.system.System.ime,
 *       "useCodePage": flash.system.System.useCodePage,
 *       },
 *       "sysCap": {
 *       "avHardwareDisable": flash.system.Capabilities.avHardwareDisable,
 *       "hasAccessibility": flash.system.Capabilities.hasAccessibility,
 *       "hasAudio": flash.system.Capabilities.hasAudio,
 *       "hasAudioEncoder": flash.system.Capabilities.hasAudioEncoder,
 *       "hasEmbeddedVideo": flash.system.Capabilities.hasEmbeddedVideo,
 *       "hasIME": flash.system.Capabilities.hasIME,
 *       "hasMP3": flash.system.Capabilities.hasMP3,
 *       "hasPrinting": flash.system.Capabilities.hasPrinting,
 *       "hasScreenBroadcast": flash.system.Capabilities.hasScreenBroadcast,
 *       "hasScreenPlayback": flash.system.Capabilities.hasScreenPlayback,
 *       "hasStreamingAudio": flash.system.Capabilities.hasStreamingAudio,
 *       "hasStreamingVideo": flash.system.Capabilities.hasStreamingVideo,
 *       "hasTLS": flash.system.Capabilities.hasTLS,
 *       "hasVideoEncoder": flash.system.Capabilities.hasVideoEncoder,
 *       "isDebugger": flash.system.Capabilities.isDebugger,
 *       "isEmbeddedInAcrobat": flash.system.Capabilities.isEmbeddedInAcrobat,
 *       "language": flash.system.Capabilities.language,
 *       "localFileReadDisable": flash.system.Capabilities.localFileReadDisable,
 *       "manufacturer": flash.system.Capabilities.manufacturer,
 *       "os": flash.system.Capabilities.os,
 *       "pixelAspectRatio": flash.system.Capabilities.pixelAspectRatio,
 *       "playerType": flash.system.Capabilities.playerType,
 *       "screenColor": flash.system.Capabilities.screenColor,
 *       "screenDPI": flash.system.Capabilities.screenDPI,
 *       "version": flash.system.Capabilities.version
 *       },
 *       "displayState": ( flash.display.Stage.displayState == FULL_SCREEN_INTERACTIVE ? "fullScreen" : "window" ),
 *       "frameRate": flash.display.Stage.frameRate,
 *       "quality": flash.display.Stage.quality,
 *       "scaleMode": flash.display.Stage.scaleMode
 *       }
 *       };
 *       }
 *@end example
 *
 *jso - Must contain a  single string attribute named “bug.” Should
 *contain  an  attribute  named   “info”  with  system  information
 *key-value pairs (see  above). May also have a  subject of “cause”
 *as a string.
 *
 *       u - The user reporting the bug.
 *
 *
 */
Tootsville.game.commands.reportBug = function (d, u, r)
{
    /* TODO */ };


/**
 * Report an user to the moderator(s) on duty for breaking a rule
 *
 *        { userName = user to be reported }
 *
 *
 */
Tootsville.game.commands.reportUser = function (d, u, r)
{
    /* TODO */ };


/**
 * Request adding a user to your buddy list (mutual-add) using the notification-based system
 *
 *(Added in 1.1)
 *
 *       jso - { buddy: LOGIN }
 *
 *u - user who is requesting the addition
 *
 */
Tootsville.game.commands.requestBuddy = function (d, u, r)
{
    /* TODO */ };


/**
 * Send an arbitrary JSON packet to another user, or all of the users in a room, out of the band of communications.
 *
 *This is neither a public nor a private message in the chat context: just
 *some additional data that is being provided.
 *
 *
 *       { sender: sender, from: outOfBand, status: true, body: {JSON} }
 *
 *       Adds "roomTitle"  to body if  body contains "room"  and title
 *       can be determined
 *
 *       Add  “"sendRoomList":  "true"”  to give  the  user  an
 *       updated  room list  as well.  (Necessary for  invitations to  new
 *       rooms.) Inviting to houses â€¦
 *
 *       initUserRoom { room: 0, autoJoin: false }
 *       { from: initUserRoom, status: true, moniker: ROOM-MONIKER } ** OK
 *
 *       =>  { from:  initUserRoom, status:  false, err:  exists, moniker:
 *       ROOM-MONIKER } ** OK
 *
 *       => {  from: initUserRoom, status:  false, err: showFirstRun  } **
 *       ERR (player does not have that room)
 *
 *       sendOutOfBandMessage   {  to:   USER-LOGIN,   body:  {   locType:
 *       "house", type: "invite", room: MONIKER } }
 *
 *       {  from:  outOfBand,  sender:  YOUR-LOGIN,  status:  true,  body:
 *       { locType: "house", type: "invite", room: MONIKER, roomTitle:
 *       USER-VISIBLE-NAME } }
 *
 *       for user houses, roomTitle will be like "BlackDaddyNerd's House"
 *
 *       Parameters:
 *
 *       jso - To send to one user:  { to: userName, body: {JSON} }, or to
 *       broadcast to the entire room: { toRoom: true, body: {JSON} }
 *
 *       u - The sender of the out-of-band-message
 *
 *       room -  The room in which  the sender is standing.  Necessary for
 *       the toRoom version of this method.
 *
 *       Throws:
 *
 *       org.json.JSONException - Thrown if the data cannot be interpreted
 *       from  the JSON  objects passed  in,  or conversely,  if we  can't
 *       encode a response into a JSON form
 *
 *
 */
Tootsville.game.commands.sendOutOfBandMessage = function (d, u, r)
{
    /* TODO */ };


/**
 *       This is used to synchronize universal time.
 *
 *       jso - { serverTime: LONG milliseconds since epoch 1900 }
 *
 */
Tootsville.game.commands.serverTime = function (d, u, r)
{ let serverTime = d.serverTime;
    /* TODO */ };


/**
 * No longer used
 */
Tootsville.game.commands.setAvatarColor = function (d, u, r)
{
    /* TODO */ };


/**
 * No longer used
 */
Tootsville.game.commands.spawnZone = function (d, u, r)
{
    /* TODO */ };


/**
 * speak
 *
 *       Handle speech by the user. 
 *
 *       Speech is public to all users in a room.
 *
 *       Emotes are simply speech beginning with "/". A few are special-cased. WRITEME: which
 *
 *	if (speech.contains (",dumpthreads,")) {
 *			OpCommands.op_dumpthreads (new String [] {}, u, channel);
 *			return;
 *		}
 *		if (speech.contains (",credits,")) {
 *			OpCommands.z$z (u);
 *			return;
 *		}
 *
 *       Commands are speech beginning with "#"
 *
 *       Parameters:
 *       jso - { "speech": TEXT-TO-BE-SPOKEN }
 *       u - The user speaking
 *       room - The room in which the speech occurs.
 *       Throws:
 *       org.json.JSONException - Thrown if the data cannot be interpreted from the JSON objects passed in, or conversely, if we can't encode a response into a JSON form
 *       NotFoundException - WRITEME
 *
 *	switch (speech.charAt (0)) {
 *			case '#':
 *			OpCommands.exec (channel, u, speech);
 *				return;
 *			case '@':
 *			Commands.speak_atMessage (u, channel, speech);
 *				return;
 *			case '$':
 *			OpCommands.hook (channel, u, speech);
 *				return;
 *			case '~':
 *				// no op… should have been handled by the client
 *				return;
 *			case '!':
 *			case '%':
 *			case '^':
 *			case '&':
 *			case '*':
 *				// no op… reserved for future purposes
 *				return;
 *		}
 *
 *	private static String nonObnoxious (final String speech) {
 *		return speech.replace ("!!", "!").replace (",,", ",").replace (
 *				"....", "...").replace ("??", "?");
 *	}
 *
 *
 *
 */
Tootsville.game.commands.speak = function (d, u, r)
{ let speech = d.speech;
    /* TODO */ };


/**
 * startEvent
 *
 *       Attempt to begin an event. Might return an error. Uses Quaestor for the heavy lifting.
 *
 *       Note that for all fountains, use the magic moniker “fountain”
 *
 *       Calls back the user with either of:
 *
 *       alreadyDone: true; status: false; err: "event.alreadyDone"
 *
 *       This  returns for  fountains  that have  already given  peanuts
 *       today (where today started at midnight, database local time)
 *
 *       eventID: (NUM), filename: "blah.swf", asVersion: { 2, 3, or not }, status: true
 *
 *       For successfully registered events. Must be completed or canceled using endEvent
 */
Tootsville.game.commands.startEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * endEvent(JSONObject ,AbstractUser , Room )
 *
 *       Parameters:
 *
 *       jso - JSON payload from the caller. Data: moniker = event moniker.
 *
 *       u - The caller = the user performing the event
 *
 *       room - The caller's room.  For fountains, we'll use this room's
 *       moniker to figure out which fountain is which 
 *
 * Throws:
 *
 *       org.json.JSONException  -  if  JSON  data  can't  be  put  into
 *       a response, or gotten out of a command.
 *
 *       SQLException - probably means that  the moniker is bad, but I'm
 *       not really doing much to validate it here
 *
 *
 */
Tootsville.game.commands.endEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * useEquipment
 *
 *       WRITEME: Document this method brpocock@star-hope.org
 *
 *       Parameters:
 *       jso - { t: slot-type-char, x: target-x, y: target-y, z: target-z, [ on: target-name ] }
 *       u - WRITEME
 *       r - WRITEME
 *       Throws:
 *       org.json.JSONException - WRITEME
 *
 *
 */
Tootsville.game.commands.useEquipment = function (d, u, r)
{
    /* TODO */ };

/**
 * Begin walking along a straight path. Path specification:
 *
 * o — object (person) walking UUID
 * sT — start time (Universal)
 * eT — end time (Universal)
 * sX, sY, sZ — start x, y, z
 * eX, eY, eZ — end x, y, z
 *
 * Replaces the “d” notation from Romance 1.1
*
* Since: Romance 1.2
 */
Tootsville.game.commands.walk = function (d, u, r)
{ /* TODO */ };
