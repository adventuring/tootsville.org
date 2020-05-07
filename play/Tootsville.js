/* -*- js2 -*-*/

/**@license
 *
 * play/Tootsville.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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

/**
 * The main  container object under  which nearly all other  modules are
 * located, to  avoid potential  namespace conflicts with  other, loaded
 * Javascript modules.
 */
var Tootsville = {
    /* Cluster name is hostname minus leading "play" */
    cluster: document.location.host.split('.').splice(1).join('.'),
    Worlds:
    { CHOR: { Gravity: 9.81 },
      MOON: { Gravity: 0.32 },
      OTHM: { Gravity: 0.12 },
      PINK: { Gravity: 0.08 } },
    Game:
    { lag: 50, /* arbitrary lag value until they compute one */
    },
    character: null,
    player: null,
    activity: { world: 'CHOR', x: 0, y: 0, z: 0,
                facing: 0, action: 'stand' },
    characterUUID: null,
    privateKey: null,
    audio: { volume: 80,
             savedVolume: 80 },
    UI: {},
    seen: {} };

if (('test.tootsville.org' == Tootsville.cluster) ||
    ('qa.tootsville.org' == Tootsville.cluster) ||
    ('tootsville.org' == Tootsville.cluster))
{ Tootsville.host =
  { www: 'https://www.' + Tootsville.cluster,
    play: 'https://play.' + Tootsville.cluster,
    game: 'https://game1.' + Tootsville.cluster.replace('.org','.net') + ":5000" };
  document.domain = Tootsville.cluster; }
else
{ Tootsville.cluster = 'local';
  let host = location.hostname;
  Tootsville.host =
  { www: 'http://' + host + ':5001',
    play: 'http://' + host + ':5002',
    game: 'http://' + host + ':5000' }; }

window.loadWatchdog1 = setTimeout(() => {
    if (Tootsville.UI.slowLoadingWatchdog)
        Tootsville.UI.slowLoadingWatchdog();
    else
        setTimeout(window.loadWatchdog1, 300); },
                                  5000);
