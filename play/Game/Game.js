/* -*- js2 -*- */

/**@license
 *
 * play/Game/Game.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }

/**
 * Is   the   `point'   near   to  the   center   of   current   activity
 * Tootsville.activity to be of interest to us?  If the point is too far
 * away, we may not care about it.
 */
Tootsville.Game.interestingPoint = function (point)
{ return ( (Math.abs (Tootsville.activity.x - point.x) < 1000)
           &&
           (Math.abs (Tootsville.activity.y - point.y) < 1000)
           &&
           (Math.abs (Tootsville.activity.z - point.z) < 1000) ); };

/**
 * How much lag are we accommodating?
 */
Tootsville.Game.lag = 100;

/**
 * Update everything that operates on the 50Hz Game Tick clock.
 */
Tootsville.Game.update = function ()
{ const newNow = (new Date).getTime ();
  Tootsville.Game.tick = newNow - Tootsville.Game.now;
  Tootsville.Game.now = newNow;
  Tootsville.Game.Nav.updateAvatars ();
  Tootsville.Game.Speech.updateSpeech ();
  Tootsville.UI.Gamepad.updateStatus ();
  Tootsville.Game.BallSystem.updateBalls ();
  Tootsville.Game.GravitySystem.updateGravity ();
  Tootsville.Game.GrowthSystem.updateGrowth ();
  Tootsville.Game.MissileSystem.updateMissiles ();
  Tootsville.Game.NPCSystem.updateNPCs (); };

/**
 * When burgeoning a region, fast-forward system effects to the present.
 */
Tootsville.Game.fastForward = function (sinceTime)
{ Tootsville.Game.now = (new Date).getTime ();
  const δT = (Tootsville.Game.now - sinceTime) / 1000;
  Tootsville.Game.BallSystem.fastForward (δT);
  Tootsville.Game.GravitySystem.fastForward (δT);
  Tootsville.Game.GrowthSystem.fastForward (δT);
  Tootsville.Game.MissileSystem.fastForward (δT);
  Tootsville.Game.NPCSystem.fastForward (δT); };

/**
 * Respond to a click on an item (furniture)
 */
Tootsville.Game.clickedOnItem = function (itemNameString, pickedEvent)
{ console.debug ("Clicked on item ", itemNameString. pickedEvent); };

/**
 *
 */
Tootsville.Game.pivotItemTemplate = function (entity)
{ /* TODO */ };

/**
 * Is there any child or sensitive player present?
 */
Tootsville.Game.anyAvatarSensitiveP = function ()
{ for (let avatarName in Tootsville.Tank.avatars)
  { const avatar = Tootsville.Tank.avatars[avatarName];
    if (avatar.childP || avatar.sensitiveP) return true; }
  return false; };
