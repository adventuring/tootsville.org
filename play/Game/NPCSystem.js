/* -*- js2 -*- */

/**@license
 *
 * play/Game/NPCSystem.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = { NPCSystem: {} }; }
if (!('NPCSystem' in Tootsville.Game)) { Tootsville.Game.NPCSystem = {}; }
if (!('npcs' in Tootsville.Game.NPCSystem)) { Tootsville.Game.NPCSystem.npcs = []; }

/**
 * NPCSystem initializer.
 *
 * @subsection NPC System Overview
 *
 * The NPC system operates in one of two modes, burgeoning and
 * operating.  When burgeoning, NPC data is fetched from the server
 * and NPCs are fast-forwarded to the present time. When operating,
 * each NPC should behave deterministically and the simulation should
 * be equivalent on every observer's system, however, one system will
 * act as the master and others will merely cross-check their
 * observations.
 */
Tootsville.Game.NPCSystem.initNPCs = function ()
{ /* TODO */ };

/**
 *
 */
Tootsville.Game.NPCSystem.burgeonNPC = function (npc)
{ /* TODO */ };

/**
 *
 */
Tootsville.Game.NPCSystem.nextBehavior = function (npc)
{ const oldBehavior = npc.behavior;
  const unrandom = Tootsville.Game.NPCSystem.unrandom (npc) % npc.behaviors.length;
  if (npc.behaviors [unrandom] == oldBehavior)
  { npc.behavior = npc.behaviors [ (unrandom + 1) % npc.behaviors.length ]; }
  else
  { npc.behavior = npc.behaviors [ unrandom ]; }
  return npc.behavior; };

/**
 *
 */
Tootsville.Game.NPCSystem.updateNPC = function (npc)
{ const behavior = npc.behavior;
  Tootsville.Game.NPC [behavior].updateNPC (npc); };

/**
 *
 */
Tootsville.Game.NPCSystem.updateNPCs = function ()
{ for (let i = 0; i < Tootsville.Game.NPCSystem.npcs.length; ++i)
  { Tootsville.Game.NPCSystem.updateNPC (Tootsville.Game.NPCSystem.npcs [i]); } };

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.NPCSystem.fastForward = function (δT)
{ /* TODO */ };

/**
 * Register an NPC
 */
Tootsville.Game.NPCSystem.register = function (avatar)
{ const components = avatar.npc.keys;
  for (let i = 0; i < components.length; ++i)
  { Tootsville.Game.NPC [components [i]].register (avatar); } };
