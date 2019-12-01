/* -*- js2 -*-*/

/*@license
 *
 * ./play/scene/ground-builder.js is part of Tootsville
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
 */

if (!('GroundBuilder' in Tootsville)) { Tootsville.GroundBuilder = {}; }



/**
 * Initialize the ground plane.
 *
 * TODO: have a height map across the groundplane.
 */
Tootsville.GroundBuilder.initGroundPlane = function ()
{ const ground =
        BABYLON.Mesh.CreateGround ('ground',
                                   100, 100, 10,
                                   Tootsville.tank.scene);
  ground.material = new BABYLON.StandardMaterial ('ground',
                                                  Tootsville.tank.scene);
  ground.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('green'));
  ground.material.specularColor = new BABYLON.Color3.FromHexString (interpretTootColor ('spring-green'));
  ground.receiveShadows = true;
  ground.physicsImpostor =  new BABYLON.PhysicsImpostor (
      ground,
      BABYLON.PhysicsImpostor.BoxImpostor,
      { mass: 0, restitution: 3 });
//  ground.checkCollisions = true;
  console.log ("Ground plane is ", ground);
  return ground; };

/**
 * Build the ground plane (terrain map) for the scene at x, y, z. 
 *
 * Affects Tootsville.tank.scene.
 */
Tootsville.GroundBuilder.build = function (x, y, z)
{
    /* TODO — get terrain data and build heightmap */
    Tootsville.GroundBuilder.initGroundPlane ();
}
