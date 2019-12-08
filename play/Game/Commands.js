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

if (!('Tootsville' in window)) { Tootsville = { Game: { Commands: {}}}; };
if (!('Game' in Tootsville)) { Tootsville.Game = { Commands: {}}; };
if (!('Commands' in Tootsville.Game)) { Tootsville.Game.Commands = {}; };

/**
 * Alias for `Tootsville.Game.Commands.setFurniture', q.v.
 */
Tootsville.Game.Commands.addFurniture = function (d, u, r)
{ Tootsville.Game.Commands.setFurniture (d, u, r); };


/**
 * No longer used
 */
Tootsville.Game.Commands.addToList = function (d, u, r)
{ Tootsville.Game.gossip.signalCommandGone ('addToList', u, r); };


/**
 * See `INFINTY-CLICK'
 */
Tootsville.Game.Commands.click = function (d, u, r)
{
    /* TODO */ };


/**
 * Either claim the user's house and lot, or add a room to their house.
 *
 * See `INFINITY-CREATE-USER-HOUSE'
 */
Tootsville.Game.Commands.createUserHouse = function (d, u, r)
{
    /* TODO */ };


/**
 * Doff an item
 *
 * See `INFINITY-DOFF' and `INFINITY-DOFFF'
 */
Tootsville.Game.Commands.doff = function (d, u, r)
{
    /* TODO */ };


/**
 * Don an item
 *
 * See `INFINITY-DON'
 */
Tootsville.Game.Commands.don = function (d, u, r)
{
    /* TODO */ };


/**
 * Echoes back the supplied JSON (or ActionScript) object to the client.
 *
 * This method exists solely for testing purposes.
 *
 * See `INFINITY-ECHO'
 */
Tootsville.Game.Commands.echo = function (d, u, r)
{
    /* TODO */ };


/**
 * This method terminates  an event (probably a  minigame, but possibly
 * a fountain) which was initiated by startEvent.
 *
 * See `INFINITY-END-EVENT' (and `INFINITY-START-EVENT' for context)
 *
 */
Tootsville.Game.Commands.endevent = function (d, u, r)
{
    /* TODO */ };


/**
 * Get public info for a list of (other) users.
 *
 * See `INFINITY-FINGER'
 */
Tootsville.Game.Commands.finger = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GAME-ACTION'
 */
Tootsville.Game.Commands.gameAction = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the apple to get into, or out of, $Eden
 * 
 * No longer needed
 * 
 * See `INFINITY-GET-APPLE'
 */
Tootsville.Game.Commands.getApple = function (d, u, r)
{
    /* TODO */ };


/**
 * Get avatar data for a list of (other) users. cv. finger
 *
 * See `INFINITY-GET-AVATARS' and `INFINITY-FINGER'
 */
Tootsville.Game.Commands.getAvatars = function (d, u, r)
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
Tootsville.Game.Commands.getColorPalettes = function (d, u, r)
{
    /* TODO */ };


/**
 * get all inventory for an user — both active and inactive
 *
 * See `INFINITY-GET-INVENTORY'
 */
Tootsville.Game.Commands.getInventory = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a subset of items from your own inventory
 *
 * See `INFINITY-GET-INVENTORY-BY-TYPE'
 *
 */
Tootsville.Game.Commands.getInventoryByType = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of users in a Zone, or in a Room.
 *
 *This is an administrative function, only available to staff members.
 *
 * See `INFINITY-GET-ONLINE-USERS'
 */
Tootsville.Game.Commands.getOnlineUsers = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all “well known” Rooms currently active/visible.
 *
 *“Rooms” no longer exist. The “rooms” are now known as “planes.”
 *
 * See `INFINITY-GET-ROOM-LIST'
 */
Tootsville.Game.Commands.getRoomList = function (d, u, r)
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
Tootsville.Game.Commands.getservertime = function (d, u, r)
{
    /* TODO */ };


/**
 * Initialise a session key for stream or batch mode operations
 *
 *Replies with @{ from: initSession, key: (OPAQUE-STRING) @}
 *
 * See `INFINITY-GET-SESSION-APPLE'
 */
Tootsville.Game.Commands.getSessionApple = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GET-STORE-ITEM-INFO'
 */
Tootsville.Game.Commands.getStoreItemInfo = function (d, u, r)
{
    /* TODO */ };


/**
 * Get the user's buddy list and ignore list.
 *
 * See `INFINITY-GET-USER-LISTS'
 */
Tootsville.Game.Commands.getUserLists = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-GET-WALLET'
 */
Tootsville.Game.Commands.getWallet = function (d, u, r)
{
    /* TODO */ };


/**
 * Get a list of all Zones currently active/visible.
 *
 * See `INFINITY-GET-ZONE-LIST'
 */
Tootsville.Game.Commands.getZoneList = function (d, u, r)
{
    /* TODO */ };


/**
 * Give an item to another user
 *
 * See `INFINITY-GIVE'
 *
 */
Tootsville.Game.Commands.give = function (d, u, r)
{
    /* TODO */ };


/**
 * go to a place and/or perform a gesture
 *
 * See `INFINITY-GO'
 */
Tootsville.Game.Commands.go = function (d, u, r)
{
    /* TODO */ };


/**
 *
 *
 *Creates room  named user/user's name/room  ... 
 *
 * See `INFINITY-INIT-USER-ROOM'
 */
Tootsville.Game.Commands.initUserRoom = function (d, u, r)
{
    /* TODO */ };


/**
 * Join a room. 
 *
 * See `INFINITY-JOIN'
 *
 */
Tootsville.Game.Commands.join = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle a login request
 *
 * See `INFINITY-LOGIN'
 */
Tootsville.Game.Commands.login = function (d, u, r)
{
    /* TODO */ };


/**
 * Log out of this game session (or zone)
 *
 * See `INFINITY-LOGOUT'
 */
Tootsville.Game.Commands.logout = function (d, u, r)
{
    /* TODO */ };


/**
 *   send an eMail to customer service (feedback)
 *
 * See `INFINITY-MAIL-CUSTOMER-SERVICE'
 */
Tootsville.Game.Commands.mailCustomerService = function (d, u, r)
{
    /* TODO */ };


/**
 * Handle looking at other user's inventories
 *
 * See `INFINITY-PEEK-AT-INVENTORY'
 */
Tootsville.Game.Commands.peekAtInventory = function (d, u, r)
{
    /* TODO */ };


/**
 *   Send a ping to the server to get back a pong.
 *
 * See `INFINITY-PING'
 *
 */
Tootsville.Game.Commands.ping = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-PROMPT-REPLY'
 *
 */
Tootsville.Game.Commands.promptReply = function (d, u, r)
{
    /* TODO */ };


/**
 * Remove someone from a buddy list or ignore list.
 *
 * See `INFINITY-REMOVE-FROM-LIST'
 */
Tootsville.Game.Commands.removeFromList = function (d, u, r)
{
    /* TODO */ };


/** 
 * This method allows  the client  to “phone home”  to report
 * a bug.
 *
 * See `INFINITY-REPORT-BUG'
 */
Tootsville.Game.Commands.reportBug = function (d, u, r)
{
    /* TODO */ };


/**
 * Report an user to the moderator(s) on duty for breaking a rule
 *
 * See `INFINITY-REPORT-USER'
 */
Tootsville.Game.Commands.reportUser = function (d, u, r)
{
    /* TODO */ };


/**
 * Request adding a user to your buddy list (mutual-add) using the notification-based system
 *
 * See `INFINITY-REQUEST-BUDDY'
 *
 */
Tootsville.Game.Commands.requestBuddy = function (d, u, r)
{
    /* TODO */ };


/**
 * Send an arbitrary JSON packet to another user, or all of the users in a room, out of the band of communications.
 *
 * See `INFINITY-SEND-OUT-OF-BAND-MESSAGE'
 *
 */
Tootsville.Game.Commands.sendOutOfBandMessage = function (d, u, r)
{
    /* TODO */ };


/**
 *       This is used to synchronize universal time.
 *
 * See `INFINITY-SERVER-TIME'
 */
Tootsville.Game.Commands.serverTime = function (d, u, r)
{ let serverTime = d.serverTime;
    /* TODO */ };


/**
 * No longer used
 *
 * See `INFINITY-SET-AVATAR-COLOR'
 */
Tootsville.Game.Commands.setAvatarColor = function (d, u, r)
{
    /* TODO */ };


/**
 * No longer used
 *
 * See `INFINITY-SPAWN-ZONE'
 */
Tootsville.Game.Commands.spawnZone = function (d, u, r)
{
    /* TODO */ };


/**
 * speak
 *
 * See `INFINITY-SPEAK'
 *
 */
Tootsville.Game.Commands.speak = function (d, u, r)
{ let speech = d.speech;
    /* TODO */ };


/**
 * See `INFINITY-START-EVENT' (and `INFINITY-END-EVENT')
 *
 */
Tootsville.Game.Commands.startEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-END-EVENT' (and `INFINITY-START-EVENT')
 */
Tootsville.Game.Commands.endEvent = function (d, u, r)
{
    /* TODO */ };


/**
 * See `INFINITY-USE-EQUIPMENT'
 */
Tootsville.Game.Commands.useEquipment = function (d, u, r)
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
Tootsville.Game.Commands.walk = function (d, u, r)
{ /* TODO */ };
