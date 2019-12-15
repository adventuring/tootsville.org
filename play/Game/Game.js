/* -*- js2 -*- */

/**@license
 *
 * play/Game/Game.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }

/**
* Is   the   `point'   near   to  the   center   of   current   activity
* `Tootsville.activity' to be of interest to us? If the point is too far
* away, we may not care about it.
*/
Tootsville.Game.interestingPoint = function (point)
{ return ( (Math.abs (Tootsville.activity.x - point.x) < 1000)
           &&
           (Math.abs (Tootsville.activity.y - point.y) < 1000)
           &&
           (Math.abs (Tootsville.activity.z - point.z) < 1000) ); };


Tootsville.Game.lag = 100;

/**
*
*/
Tootsville.Game.update = function ()
{ Tootsville.Game.now = (new Date).getTime ();
  Tootsville.Game.Nav.updateAvatars ();
  Tootsville.Game.Speech.updateSpeech ();
  Tootsville.UI.Gamepad.updateStatus (); };

Tootsville.Game.clickedOnItem = function (itemNameString, pickedEvent)
{ console.debug ("Clicked on item ", itemNameString. pickedEvent); };

