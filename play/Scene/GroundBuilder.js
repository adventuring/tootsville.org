/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/GroundBuilder.js is part of Tootsville
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
 * Set the parameters of POLYGON to appear to be the kind of place PLACE is.
 */
Tootsville.GroundBuilder.setPolygonForPlace = function (polygon, place)
{ polygon.material = Tootsville.GroundBuilder.materials [ place.kind ]; };

/**
 * Build the ground for a certain shape
 */
Tootsville.GroundBuilder.buildGroundForShape = function (shape, place, placeName)
{ const path = new BABYLON.Path2 (shape [0].x, shape [0].z);
  const shapeLength = shape.length;
  for (let i = 1; i < shapeLength; ++i)
  { const point = shape [i];
    path.addLineTo (point.x, point.y); }
  path.close ();
  const triangles = new BABYLON.PolygonMeshBuilder ('ground/' + placeName, path, Tootsville.Tank.scene);
  const polygon = triangles.build (/* updateable: */ false, /* depth: */ 0);
  Tootsville.GroundBuilder.holes [ placeName ] = path;
  polygon.rotation.z = Math.PI / 2;
  Tootsville.GroundBuilder.setPolygonForPlace (polygon, place);
};

/**
 * Build the ground for a certain place
 */
Tootsville.GroundBuilder.buildGroundForPlace = function (placeName, place)
{ const shapes = place.shapes;
  if (!shapes) return;
  const shapesLength = shapes.length;
  for (let i = 0; i < shapesLength; ++i)
  { Tootsville.GroundBuilder.buildGroundForShape (shape, place, placeName); } };

/**
 * Build grass surrounding any other ground polygons that were already created
 */
Tootsville.GroundBuilder.buildSurroundingGrass = function ()
{ const holes = Tootsville.GroundBuilder.holes;
  const holesLength = holes.length;
  const triangles = new BABYLON.PolygonMeshBuilder ('ground*',
						    [ new BABYLON.Vector2 (-1500, -1500),
						      new BABYLON.Vector2 (-1500, 1500),
						      new BABYLON.Vector2 (1500, 1500),
						      new BABYLON.Vector2 (1500, -1500) ],
						    Tootsville.Tank.scene);
  for (let i = 0; i < holesLength; ++i)
  { triangles.addHole (holes [i]); }
  const surroundings = triangles.build (/* updateable: */ false, /* depth: */ 0);
  surroundings.material = Tootsville.GroundBuilder.materials [ 'grass' ]; };

/**
 * Build the ground plane (terrain map) for the scene at lat, long, alt.
 *
 * Affects Tootsville.Tank.scene.
 */
Tootsville.GroundBuilder.build = function (lat, long, alt)
{ if (Tootsville.Tank.ground)
  { Tootsville.Tank.ground.dispose (); }
  Tootsville.GroundBuilder.holes = {};
  const places = Object.keys (Tootsville.SceneBuilder.places);
  const placesLength = places.length;
  for (let i = 0; i < placesLength; ++i)
  { const place = places [i];
    Tootsville.GroundBuilder.buildGroundForPlace (Tootsville.SceneBuilder.places [place], place); }
  Tootsville.GroundBuilder.buildSurroundingGrass (); };

/**
 * Initialize the GroundBuilder so it can be used.
 *
 * Should be called immediately after Tootsville.Tank.scene is initialized.
 */
Tootsville.GroundBuilder.init = function ()
{ if (! Tootsville.GroundBuilder.materials)
  { Tootsville.GroundBuilder.materials = {};
      const grassMaterial = new BABYLON.StandardMaterial ('ground/grass', Tootsville.Tank.scene);
    grassMaterial.diffuseColor = interpretTootColor ('green');
    grassMaterial.specularColor = interpretTootColor ('spring-green');
    Tootsville.GroundBuilder.materials ['grass'] = grassMaterial;
    Tootsville.GroundBuilder.materials ['tallGrass'] = grassMaterial;

    const waterMaterial = new BABYLON.StandardMaterial ('ground/water', Tootsville.Tank.scene);
    waterMaterial.diffuseColor = interpretTootColor ('blue');
    waterMaterial.specularColor = interpretTootColor ('periwinkle');
    Tootsville.GroundBuilder.materials ['water'] = waterMaterial;

    const sidewalkMaterial = new BABYLON.StandardMaterial ('ground/sidewalk', Tootsville.Tank.scene);
    sidewalkMaterial.diffuseColor = interpretTootColor ('silver');
    sidewalkMaterial.specularColor = interpretTootColor ('white');
    Tootsville.GroundBuilder.materials ['sidewalk'] = sidewalkMaterial;
    Tootsville.GroundBuilder.materials ['cobbles'] = sidewalkMaterial;
    Tootsville.GroundBuilder.materials ['pavement'] = sidewalkMaterial; }};

