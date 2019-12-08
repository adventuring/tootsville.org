/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/SceneBuilder.js is part of Tootsville
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

if (!('SceneBuilder' in Tootsville)) { Tootsville.SceneBuilder = {}; }

Tootsville.SceneBuilder.makeBallPit = function (scene)
{  for (let i = 1; i < 100; ++i)
   { let sphere = BABYLON.Mesh.CreateSphere("sphere" * i, 4, 2 + 10 * i / 100, scene);
     sphere.position.x = Math.sin (i);
     sphere.position.y = 8 * i;
     sphere.position.z = Math.cos (i);
    sphere.physicsImpostor = new BABYLON.PhysicsImpostor(sphere,
                                                         BABYLON.PhysicsImpostor.SphereImpostor,
                                                         { mass: 1, restitution: 0.1 },
                                                         scene); } };

/**
 * Place furniture as found in the current scene.
 *
 * Affects Tootsville.Tank.scene
 */
Tootsville.SceneBuilder.build = function (x, y, z)
{ const scene = Tootsville.Tank.scene;
  Tootsville.FurnitureBuilder.build (
      { uuid: 'fountain1',
        template: { name: 'Fountain', avatar: 'TootSquare/FountainToot', weight: 6000 },
        baseColor: 'periwinkle', altColor: 'yellow', position: { x: 25, y: 0, z: -25 }, facing: 0 },
      scene);
  Tootsville.FurnitureBuilder.build (
      { uuid: 'fountain2',
        template: { name: 'Fountain', avatar: 'TootSquare/FountainToot', weight: 6000 },
        baseColor: 'periwinkle', altColor: 'yellow', position: { x: 25, y: 0, z: 25 }, facing: Math.PI * 2/3 },
      scene);
  Tootsville.FurnitureBuilder.build (
      { uuid: 'fountain3',
        template: { name: 'Fountain', avatar: 'TootSquare/FountainToot', weight: 6000 },
        baseColor: 'periwinkle', altColor: 'yellow', position: { x: -25, y: 0, z: -25 }, facing: Math.PI * 4/3 },
      scene);
  Tootsville.SceneBuilder.makeBallPit (scene); };
