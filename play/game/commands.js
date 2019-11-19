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
 * Alias for `Tootsville.game.commands.setFurniture', q.v.
 */
Tootsville.game.commands.addFurniture = function (d, u, r)
{ Tootsville.game.commands.setFurniture (d, u, r); };


/**
 * No longer used
 */
Tootsville.game.commands.addToList = function (d, u, r)
{ Tootsville.game.gossip.signalCommandGone ('addToList', u, r); };


/**
 * See `INFINTY-CLICK'
 */
Tootsville.game.commands.click = function (d, u, r)
{
    /* TODO */ };


/**
 * Either claim the user's house and lot, or add a room to their house.
 *
 * See `INFINITY-CREATE-USER-HOUSE'
 */
Tootsville.game.commands.createUserHouse = function (d, u, r)
{
    /* TODO */ };


/**
 * Doff an item
 *
 * See `INFINITY-DOFF' and `INFINITY-DOFFF'
 */
Tootsville.game.commands.doff = function (d, u, r)
{
    /* TODO */ };


/**
 * Don an item
 *
 * See `INFINITY-DON'
 */
Tootsville.game.commands.don = function (d, u, r)
{
    /* TODO */ };


/**
 * Echoes back the supplied JSON (or ActionScript) object to the client.
 *
 * This method exists solely for testing purposes.
 *
 * See `INFINITY-ECHO'
 */
Tootsville.game.commands.echo = function (d, u, r)
{
    /* TODO */ };


/**
 * This method terminates  an event (probably a  minigame, but possibly
 * a fountain) which was initiated by startEvent.
 *
 * See `INFINITY-END-EVENT' (and `INFINITY-START-EVENT' for context)
 *
 */
Tootsville.game.commands.endevent = function (d, u, r)
{
    /* TODO */ };


/**
 * Get public info for a list of (other) users.
 *
 * See `INFINITY-FINGER'
 */
Tootsville.game.commands.finger = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GAME-ACTION'
 */
Tootsville.game.commands.gameAction = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the apple to get into, or out of, $Eden
 * 
 * No longer needed
 * 
 * See `INFINITY-GET-APPLE'
 */
Tootsville.game.commands.getApple = function (d, u, r)
{
    /* TODO */ };


/**
 * Get avatar data for a list of (other) users. cv. finger
 *
 * See `INFINITY-GET-AVATARS' and `INFINITY-FINGER'
 */
Tootsville.game.commands.getAvatars = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GET-COLOR-PALETTES'
 *
 *@subsection 410 Gone
 *
 *Removed.. This  routine appeared to be  unused by anyone in  Romance 1.1
 *and was removed in 1.2.
 *
 *@subsection Note
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
 * See `INFINITY-GET-INVENTORY'
 */
Tootsville.game.commands.getInventory = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a subset of items from your own inventory
 *
 * See `INFINITY-GET-INVENTORY-BY-TYPE'
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
 * See `INFINITY-GET-ONLINE-USERS'
 */
Tootsville.game.commands.getOnlineUsers = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all “well known” Rooms currently active/visible.
 *
 *“Rooms” no longer exist. The “rooms” are now known as “planes.”
 *
 * See `INFINITY-GET-ROOM-LIST'
 */
Tootsville.game.commands.getRoomList = function (d, u, r)
{
    /* TODO */ };


/**
 * Send the server time to the client requesting it
 *
 *Sends a JSON object with a single property, serverTime, with the current
 *time in milliseconds (give or take transit time). This is the Unix time,
 *not the Universal time.
 *
 * See `INFINITY-GET-SERVER-TIME'
 */
Tootsville.game.commands.getservertime = function (d, u, r)
{
    /* TODO */ };


/**
 * Initialise a session key for stream or batch mode operations
 *
 *Replies with @{ from: initSession, key: (OPAQUE-STRING) @}
 *
 * See `INFINITY-GET-SESSION-APPLE'
 */
Tootsville.game.commands.getSessionApple = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GET-STORE-ITEM-INFO'
 */
Tootsville.game.commands.getStoreItemInfo = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the user's buddy list and ignore list.
 *
 * See `INFINITY-GET-USER-LISTS'
 */
Tootsville.game.commands.getUserLists = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GET-WALLET'
 */
Tootsville.game.commands.getWallet = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all Zones currently active/visible.
 *
 * See `INFINITY-GET-ZONE-LIST'
 */
Tootsville.game.commands.getZoneList = function (d, u, r)
{
    /* TODO */ };


/**
 * Give an item to another user
 *
 * See `INFINITY-GIVE'
 *
 */
Tootsville.game.commands.give = function (d, u, r)
{
    /* TODO */ };


/**
 * go to a place and/or perform a gesture
 *
 * See `INFINITY-GO'
 */
Tootsville.game.commands.go = function (d, u, r)
{
    /* TODO */ };


/**
 *
 *
 *Creates room  named user/user's name/room  ... 
 *
 * See `INFINITY-INIT-USER-ROOM'
 */
Tootsville.game.commands.initUserRoom = function (d, u, r)
{
    /* TODO */ };


/**
 * Join a room. 
 *
 * See `INFINITY-JOIN'
 *
 */
Tootsville.game.commands.join = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle a login request
 *
 * See `INFINITY-LOGIN'
 */
Tootsville.game.commands.login = function (d, u, r)
{
    /* TODO */ };


/**
 * Log out of this game session (or zone)
 *
 * See `INFINITY-LOGOUT'
 */
Tootsville.game.commands.logout = function (d, u, r)
{
    /* TODO */ };


/**
 *   send an eMail to customer service (feedback)
 *
 * See `INFINITY-MAIL-CUSTOMER-SERVICE'
 */
Tootsville.game.commands.mailCustomerService = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle looking at other user's inventories
 *
 * See `INFINITY-PEEK-AT-INVENTORY'
 */
Tootsville.game.commands.peekAtInventory = function (d, u, r)
{
    /* TODO */ };


/**
 *   Send a ping to the server to get back a pong.
 *
 * See `INFINITY-PING'
 *
 */
Tootsville.game.commands.ping = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-PROMPT-REPLY'
 *
 */
Tootsville.game.commands.promptReply = function (d, u, r)
{
    /* TODO */ };


/**
 * Remove someone from a buddy list or ignore list.
 *
 * See `INFINITY-REMOVE-FROM-LIST'
 */
Tootsville.game.commands.removeFromList = function (d, u, r)
{
    /* TODO */ };


/** 
 * This method allows  the client  to “phone home”  to report
 * a bug.
 *
 * See `INFINITY-REPORT-BUG'
 */
Tootsville.game.commands.reportBug = function (d, u, r)
{
    /* TODO */ };


/**
 * Report an user to the moderator(s) on duty for breaking a rule
 *
 * See `INFINITY-REPORT-USER'
 */
Tootsville.game.commands.reportUser = function (d, u, r)
{
    /* TODO */ };


/**
 * Request adding a user to your buddy list (mutual-add) using the notification-based system
 *
 * See `INFINITY-REQUEST-BUDDY'
 *
 */
Tootsville.game.commands.requestBuddy = function (d, u, r)
{
    /* TODO */ };


/**
 * Send an arbitrary JSON packet to another user, or all of the users in a room, out of the band of communications.
 *
 * See `INFINITY-SEND-OUT-OF-BAND-MESSAGE'
 *
 */
Tootsville.game.commands.sendOutOfBandMessage = function (d, u, r)
{
    /* TODO */ };


/**
 *       This is used to synchronize universal time.
 *
 * See `INFINITY-SERVER-TIME'
 */
Tootsville.game.commands.serverTime = function (d, u, r)
{ let serverTime = d.serverTime;
    /* TODO */ };


/**
 * No longer used
 *
 * See `INFINITY-SET-AVATAR-COLOR'
 */
Tootsville.game.commands.setAvatarColor = function (d, u, r)
{
    /* TODO */ };


/**
 * No longer used
 *
 * See `INFINITY-SPAWN-ZONE'
 */
Tootsville.game.commands.spawnZone = function (d, u, r)
{
    /* TODO */ };


/**
 * speak
 *
 * See `INFINITY-SPEAK'
 *
 */
Tootsville.game.commands.speak = function (d, u, r)
{ let speech = d.speech;
    /* TODO */ };


/**
 * See `INFINITY-START-EVENT' (and `INFINITY-END-EVENT')
 *
 */
Tootsville.game.commands.startEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-END-EVENT' (and `INFINITY-START-EVENT')
 */
Tootsville.game.commands.endEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-USE-EQUIPMENT'
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
 * @subsection Added in Romance 1.2
 *
 * This replaced the ``d'' notation from Romance 1.1 with 1.2.0
 *
 * @subsection Gossipnet only
 *
 * This command is valid on the Gossipnet, but does not have a 
 * REST equivalent.
 */
Tootsville.game.commands.walk = function (d, u, r)
{ /* TODO */ };
