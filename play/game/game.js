/* -*- js2 -*- */

/*@license
 *
 * ./play/game/game.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = {game: {}}; }
if (!('game' in Tootsville)) { Tootsville.game = {}; }

Tootsville.game.interestingPoint = function (point)
{ return ( (Math.abs (Tootsville.activity.x - point.x) < 1000)
           &&
           (Math.abs (Tootsville.activity.y - point.y) < 1000)
           &&
           (Math.abs (Tootsville.activity.z - point.z) < 1000) ); }

/*
 * Structure of avatar data:
 *
 * avatar:  Path  to  avatar;  prefix with  /Assets/Avatars/5/  and  add
 * .babylon extension.
 *
 * clothes: Array  of clothing objects, each  of which has a  name "url"
 * (prefix with /Assets/Equipment/5/ and suffix with .babylon), optional
 * "color" and "altColor"
 *
 * φ:  Physics  structure.  Absence  of  φ  indicates  that  the  object
 * is stationary. Data in Cannon format. φ contains:
 *
 * φ.mass,   φ.shape,    φ.position   =   {x:,   y:,    z:},   velocity,
 * angularVelocity,    quaternion,   material,    type,   linearDamping,
 * angularDamping,    allowSleep,    sleepSpeedLimit,    sleepTimeLimit,
 * fixedRotation, linearFactor, angularFactor
 *
 */

Tootsville.game.updateAvatar = function (userName, avatar)
{ const uuid = avatar.uuid;
  const position = avatar.φ.position;
  if (! Tootsville.game.interestingPoint (position))
  { return; }
  Tootsville.avatars [uuid] = avatar;
  if (userName)
  { Tootsville.avatars [uuid].name = userName; }
  if (Tootsville.avatars [uuid].model)
  { this.refreshAvatar (uuid); }
  else
  { this.makeAvatar (uuid); } };

Tootsville.game.refreshAvatar = function (uuid)
{ const avatar = Tootsville.avatars [uuid];
  const name = avatar.name;
  if (name && !avatar.label)
  { this.attachLabelToAvatar (name, uuid); }
  Tootsville.tank.updateAttachmentsForAvatar (avatar);
  /* TODO: update physics, position, facing, animation */ };

Tootsville.game.attachLabelToAvatar = function (name, uuid)
{ const label = document.createElement ('DIV');
  if (Tootsville.avatars [uuid].childP)
  { label.innerHTML = '◆' + name; }
  else
  { label.innerHTML = name; }
  label.className = 'name-tag';
  label.slot = 'bottom';
  const hud = document.getElementById ('hud');
  hud.appendChild (balloon);
  Tootsville.avatars [uuid].label = label;
  return label; };

Tootsville.game.attachSpeechToAvatar = function (html, style, uuid)
{ const balloon = document.createElement ('DIV');
  balloon.innerHTML = html;
  balloon.className = style;
  balloon.slot = 'top';
  const hud = document.getElementById ('hud');
  hud.appendChild (balloon);
  setTimeout ( () =>
               { if (balloon.innerHTML == html)
                 { balloon.opacity = 0;
                   setTimeout ( () =>
                                { Tootsville.avatars [uuid].speech = null;
                                  hud.removeChild (balloon); },
                                1000);} },
               30000 );
  Tootsville.avatars [uuid].speech = balloon;
  return balloon; };

