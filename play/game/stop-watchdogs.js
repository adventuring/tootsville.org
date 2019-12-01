/* -*- js2 -*-*/

/*@license
 *
 * ./play/game/stop-watchdogs.js is part of Tootsville
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
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
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

if (!("game" in Tootsville))
{ Tootsville.Game = {}; }

/**
 * Hide the loading overlay once the game is ready.
 *
 * Call this function when the game is ready.
 */
Tootsville.Game.hideWhenGameReady = function ()
{ var toHide = document.querySelectorAll('.hide-when-game-ready');
  for (var i = 0; i < toHide.length; i++)
  { toHide[i].style.display = 'none'; }};

/**
 * Stop the watchdogs for slow loading.
 *
 * Call this function once the loading has been completed enough.
 */
Tootsville.Game.stopSlowLoadingWatchdogs = function ()
{ clearTimeout(window.loadWatchdog1);
  clearTimeout(window.loadWatchdog2);
  window.loadWatchdog1 = null;
  window.loadWatchdog2 = null;

  var dialog = document.getElementById('elt-Slow-Loading-');
  if (dialog)
  { dialog.parentNode.removeChild(dialog); }};
