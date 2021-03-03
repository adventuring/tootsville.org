/* -*- js2 -*-*/

/**@license
 *
 * ./play/UI/panels/player-card.js is part of Tootsville
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

if (!('Tootsville' in window)) Tootsville = { UI: { PlayerCard: {}}};
if (!('UI' in Tootsville)) Tootsville.UI = { PlayerCard: {}};
if (!('PlayerCard' in Tootsville.UI)) Tootsville.UI.PlayerCard = {};

Tootsville.UI.PlayerCard.reportUser = function ()
{ let playerName = document.getElementById('other-player-name').innerText;
  if (playerName[0] === '◆')
      playerName = playerName.substr(1);
  Tootsville.UI.HUD.showHUDPanel('report-user').then
  (() => { document.getElementById('report-user-name').innerText = playerName; });
};
