/* -*- js2 -*- */

/**@license
 *
 * play/Game/Interact.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {Interact: {}}; }
if (!('Interact' in Tootsville.Game)) { Tootsville.Game.Interact = {}; }

Tootsville.Game.Interact.typeName = [ 'Nil', 'Interact', 'Push', 'Pull', 'Wish', 'Shaddow', 'Fire', 'Frost', 'Water', 'Wind', 'Impact', 'Stepped' ];

for (let i = 0; i < Tootsville.Game.Interact.typeName.length; ++i)
{ Tootsville.Game.Interact ["TYPE_" + typeName.toUpperCase ()] = i; }

Tootsville.Game.Interact.handleInteraction = function (agent, target, interactionType, interactionForce)
{ const handler = target [ 'on' + Tootsville.Game.Interact.typeName [interactionType] ];
  if (handler)
  { handler(target, agent, interactionType, interactionForce); }
  const defaultHandler = Tootsville.Game.Interact.defaultHandler [Tootsville.Game.Interact.typeName [interactionType]];
  if (defaultHandler)
  { defaultHandler (target, agent, interactionForce); } };

Tootsville.Game.Interact.defaultHandler.Push = function (target, agent, force)
{ if (target.mass && agent.mass && target.mass < agent.mass)
  { const movement = BABYLON.Vector3.Normalize (agent.model.position.subtract (target.model.position));
    Tootsville.Game.Nav.slideObject (target, movement);
    Tootsville.Game.Nav.slideObject (agent, movement); } };

Tootsville.Game.Interact.defaultHandler.Pull = function (target, agent, force)
{ if (target.mass && agent.mass && target.mass < agent.mass)
  { const movement = BABYLON.Vector3.Normalize (target.model.position.subtract (agent.model.position));
    Tootsville.Game.Nav.slideObject (target, movement);
    Tootsville.Game.Nav.slideObject (agent, target.position); } };
