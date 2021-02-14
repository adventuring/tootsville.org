/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/GroundBuilder.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2021   The
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

if (!('Tootsville' in window)) Tootsville = { GroundBuilder: {} };
if (!('GroundBuilder' in Tootsville)) Tootsville.GroundBuilder = {};

/**
 * Initialize the ground plane.
 *
 * TODO: have a height map across the groundplane.
 */
Tootsville.GroundBuilder.initGroundPlane = function ()
{ const ground =
        BABYLON.Mesh.CreateGround ('ground',
                                   3000, 3000, 0,
                                   Tootsville.Tank.scene);
  const groundTexture = new BABYLON.DynamicTexture ("ground",
                                                  1024, Tootsville.Tank.scene);
  const groundCanvas = groundTexture.getContext ();
  groundCanvas.fillStyle = Tootsville.UI.interpretTootColor ('green');
  groundCanvas.fillRect (0, 0, 1024, 1024);
  const groundMaterial = new BABYLON.StandardMaterial ("ground",
                                                       Tootsville.Tank.scene);
  groundMaterial.diffuseTexture = groundTexture;
  groundTexture.update ();
  ground.material = groundMaterial;
  ground.receiveShadows = true;
  ground.checkCollisions = true;
  Tootsville.Tank.ground = ground;
  Tootsville.Tank.groundTexture = groundTexture;
  return groundCanvas; };

/**
 *
 */
Tootsville.GroundBuilder.kinds =
    { grass: 'green',
      tallGrass: 'green',
      water: 'blue',
      sidewalk: 'silver',
      driveway: 'charcoal',
      'cobbles': 'silver',
      ice: 'perwinkle',
      snow: 'white',
      sand: 'yellow',
      pit: 'black'     
    };

/**
 *
 */
Tootsville.GroundBuilder.colorForPlace = function (kind)
{ return Tootsville.UI.interpretTootColor ( Tootsville.GroundBuilder.kinds [ kind ] ); };

/**
 *
 */
Tootsville.GroundBuilder.paintPlaces = function (lat, long, alt)
{ const groundCanvas = Tootsville.Tank.groundTexture.getContext ();
  const places = Object.values (Tootsville.SceneBuilder.places);
  for (let i = 0; i < places.length; ++i)
  { const kind = places [i].kind;
    const shapes = places [i].shapes;
    groundCanvas.setTransform (1, 0, 0, 1, 512, 512);
    groundCanvas.scale (1024/3000, 1024/3000);
    groundCanvas.fillStyle = Tootsville.GroundBuilder.colorForPlace (kind);
    for (let j = 0; j < places.length; ++j)
    { const shape = shapes [ j ];
      console.debug ("Drawing shape on ground", shape);
      groundCanvas.beginPath ();
      for (let n = 0; n < shape.length; ++n)
      { const point = shape [ n ];
        if (0 === n) { groundCanvas.moveTo (point.x, point.z); }
        else { groundCanvas.lineTo (point.x, point.z); } }
      groundCanvas.fill (); } }
  Tootsville.Tank.groundTexture.update (); };

/**
 * Build the ground plane (terrain map) for the scene at lat, long, alt.
 *
 * Affects Tootsville.Tank.scene.
 */
Tootsville.GroundBuilder.build = function (lat, long, alt)
{
    /* TODO — get terrain data and build heightmap */
    if (! Tootsville.Tank.ground)
    { Tootsville.GroundBuilder.initGroundPlane (); }
    Tootsville.GroundBuilder.paintPlaces (lat, long, alt); };

