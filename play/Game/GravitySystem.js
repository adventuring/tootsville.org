/* -*- js2 -*- */

/**@license
 *
 * play/Game/GravitySystem.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = { GravitySystem: {} }; }
if (!('GravitySystem' in Tootsville.Game)) { Tootsville.Game.GravitySystem = {}; }

if (!('entities' in Tootsville.Game.GravitySystem))
{ Tootsville.Game.GravitySystem.entities = []; }


/**
 * Makes an entity's model subject to gravity.
 */
Tootsville.Game.GravitySystem.register = function (entity)
{ if (!(entity.model))
  { throw new Exception ("Can't apply gravity to entity without model"); }
  Tootsville.Game.GravitySystem.entities.push (entity); };

/**
 *
 */
Tootsville.Game.GravitySystem.updateEntityGravity = function (entity)
{ const position = entity.model.position;
  let downward = entity.model.position;
  downward.y += 9.8/50;
  const collisionP = Tootsville.Game.Nav.collisionP (entity.model, position, downward);
  if (!collisionP)
  { entity.model.position = downward; } };

/**
 *
 */
Tootsville.Game.GravitySystem.updateGravity = function ()
{ Tootsville.Game.GravitySystem.entities.forEach (Tootsville.Game.GravitySystem.updateEntityGravity);
};

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.GravitySystem.fastForward = function (δT)
{ /* TODO */ };
