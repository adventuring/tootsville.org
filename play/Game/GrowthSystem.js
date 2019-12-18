/* -*- js2 -*- */

/**@license
 *
 * play/Game/GrowthSystem.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = { GrowthSystem: {} }; }
if (!('GrowthSystem' in Tootsville.Game)) { Tootsville.Game.GrowthSystem = {}; }

if (!('allGrowing' in Tootsville.Game.GrowthSystem))
{ Tootsville.Game.GrowthSystem.allGrowing = []; }

/**
 * Register an entity that can grow.
 */
Tootsville.Game.GrowthSystem.register = function (entity)
{ Tootsville.Game.GrowthSystem.allGrowing.push (entity); };

/**
 *
 */
Tootsville.Game.GrowthSystem.remove = function (entity)
{ const index = Tootsville.Game.GrowthSystem.allGrowing.indexOf (entity);
  if (index >= 0) { Tootsville.Game.GrowthSystem.allGrowing.splice (index, 1); } };

/**
 *
 */
Tootsville.Game.GrowthSystem.evolve = function (entity)
{ entity.growth.growthTimer = 0;
  Tootsville.Game.pivotItemTemplate (entity, entity.growth.nextEvolutionTemplate);
  };

/**
 *
 */
Tootsville.Game.GrowthSystem.grow = function (entity)
{ entity.growth.growthTimer += 1/50;
  if (entity.growth.growthTimer > entity.growth.nextEvolutionTime)
  { Tootsville.Game.GrowthSystem.evolve (entity); } };

/**
 * Update the growth of all eligible entities
 */
Tootsville.Game.GrowthSystem.updateGrowth = function ()
{ for (let i = 0; i < Tootsville.Game.GrowthSystem.allGrowing.length; ++i)
  { Toosville.Game.GrowthSystem.grow (Tootsville.Game.GrowthSystem.allGrowing [i]); } };

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.GrowthSystem.fastForward = function (δT)
{ /* TODO */ };
