/* -*- js2 -*- */

/**@license
 *
 * play/Game/VehicleRidingSystem.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = { VehicleRidingSystem: {} }; }
if (!('VehicleRidingSystem' in Tootsville.Game)) { Tootsville.Game.VehicleRidingSystem = {}; }

if (!('entities' in Tootsville.Game.VehicleRidingSystem))
{ Tootsville.Game.VehicleRidingSystem.entities = []; }

/**
 * Makes an entity's model be positioned relative to a moving vehicle.
 */
Tootsville.Game.VehicleRidingSystem.register = function (entity, vehicle)
{ Tootsville.Game.VehicleRidingSystem.entities.add (entity);
  entity.onVehicle = vehicle; };

/**
 * Update one entity riding a vehicle.
 */
Tootsville.Game.VehicleRidingSystem.updateEntityRidingVehicle = function (entity)
{ entity.model.position = entity.model.position.add (entity.onVehicle.model.lastΔPosition);
  entity.model.rotation = entity.model.rotation.add (entity.onVehicle.lastΔRotation); };

/**
 * Update all entities currently riding a vehicle.
 */
Tootsville.Game.VehicleRidingSystem.updateVehicleRiding = function ()
{ Tootsville.Game.VehicleRidingSystem.entities.forEach (Tootsville.Game.VehicleRidingSystem.updateEntityRidingVehicle);
};
