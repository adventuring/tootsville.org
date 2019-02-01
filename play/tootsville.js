/* -*- js2 -*-*/

/*@license
 *
 * ./play/tootsville.js is part of Tootsville
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
 */var Tootsville = {
     /* Cluster name is hostname minus leading "play" */
     cluster: document.location.host.split('.').splice(1).join('.'),
     Worlds:
     { Tootanga: { Gravity: 9.81 },
       TheMoon: { Gravity: 0.32 },
       TheOtherMoon: { Gravity: 0.12 },
       ThePinkMoon: { Gravity: 0.08 } },
     character: null,
     privateKey: null,
     audio: { volume: 80,
              savedVolume: 80 },
     createBoxAvatar: function () {},
     brickController: function () {},
     Item: function (json)
     { this.avatar = json.avatar || Tootsville.createBoxAvatar();
       this.controller = Tootsville.brickController();
       this.referenceFrame = json.referenceFrame || null;
       this.name = json.name;
       this.itemP = true; },
     Character: function (json)
     { Item.call(this, json);
       this.controller = json.controller;
       this.characterP = true; } };

if (('test.tootsville.org' == Tootsville.cluster) ||
    ('qa.tootsville.org' == Tootsville.cluster) ||
    ('tootsville.org' == Tootsville.cluster))
{ Tootsville.host =
  { www: 'https://www.' + Tootsville.cluster,
    play: 'https://play.' + Tootsville.cluster,
    users: 'https://users.' + Tootsville.cluster.replace('.org','.net'),
    gossip: 'https://gossip.' + Tootsville.cluster.replace('.org','.net'),
    world: 'https://world.' + Tootsville.cluster.replace('.org','.net') };
  document.domain = Tootsville.cluster; }
else
{ Tootsville.cluster = 'local';
  Tootsville.host =
  { www: 'http://localhost:5001',
    play: 'http://localhost:5002',
    users: 'http://localhost:5000',
    gossip: 'http://localhost:5000',
    world: 'http://localhost:5000' }; }

window.addEventListener('Tootsville/Login', function (ev)
                        { Rollbar.configure({ payload: { person: { id: Tootsville.player && Tootsville.player.id,
                                                                   username: Tootsville.character && Tootsville.character.name,
                                                                   email: Tootsville.player && Tootsville.player.eMail }}}); },
                        false);

window.loadWatchdog1 = setTimeout(() => {
    if (Tootsville.ui.slowLoadingWatchdog)
        Tootsville.ui.slowLoadingWatchdog();
    else
        setTimeout(window.loadWatchdog1, 300); },
                                  5000);
