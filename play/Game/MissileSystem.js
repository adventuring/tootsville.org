/* -*- js2 -*- */

/**@license
 *
 * play/Game/MissileSystem.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = { MissileSystem: {} }; }
if (!('MissileSystem' in Tootsville.Game)) { Tootsville.Game.MissileSystem = {}; }

if (!('allMissiles' in Tootsville.Game.MissileSystem))
{ Tootsville.Game.MissileSystem.allMissiles = []; }

/**
 *
 */
Tootsville.Game.MissileSystem.register = function (entity, course)
{ entity.course = course;
  Tootsville.Game.MissileSystem.allMissiles.push (entity); };

/**
 *
 */
Tootsville.Game.MissileSystem.remove = function (entity)
{ const index = Tootsville.Game.MissileSystem.allMissiles.indexOf (entity);
  if (index >= 0) { Tootsville.Game.MissileSystem.allMissiles.splice (index, 1); } };

/**
 * Update the position of all missiles
 */
Tootsville.Game.MissileSystem.updateMissiles = function ()
{ for (let i = 0; i < Tootsville.Game.MissileSystem.allMissiles.length; ++i)
  { const missile = Tootsville.Game.MissileSystem.allMissiles [i];
    if (missile.course)
    { let finish = Tootsville.Game.Nav.moveEntityOnCourse (missile, missile.course);
      if (finish) { delete missile['course'];
                    Tootsville.Game.GravitySystem.register (missile);
                    Tootsville.Game.MissileSystem.remove (missile); } } } };

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.MissileSystem.fastForward = function (δT)
{ /* TODO */ };
