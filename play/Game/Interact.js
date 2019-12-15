/* -*- js2 -*- */

/**@license
 *
 * play/Game/Interact.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {Interact: {}}; }
if (!('Interact' in Tootsville.Game)) { Tootsville.Game.Interact = {}; }

Tootsville.Game.Interact.TYPE_INTERACT = 1;
Tootsville.Game.Interact.TYPE_PUSH = 2;
Tootsville.Game.Interact.TYPE_PULL = 3;
Tootsville.Game.Interact.TYPE_WISH = 4;
Tootsville.Game.Interact.TYPE_SHADDOW = 5;
Tootsville.Game.Interact.TYPE_FIRE = 6;
Tootsville.Game.Interact.TYPE_FROST = 7;
Tootsville.Game.Interact.TYPE_WATER = 8;
Tootsville.Game.Interact.TYPE_WIND = 9;

Tootsville.Game.Interact.handleInteraction = function (avatar, entity, interactionType)
{ if (avatar == Tootsville.Tank.playerAvatar ())
  {} };
