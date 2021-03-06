/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/SceneBuilder.js is part of Tootsville
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

if (!('SceneBuilder' in Tootsville)) { Tootsville.SceneBuilder = {}; }

/**
 *
 */
Tootsville.SceneBuilder.makeBallPit = function (scene)
{  for (let i = 1; i < 100; ++i)
   { let sphere = BABYLON.Mesh.CreateSphere("sphere" * i, 4, 2 + 10 * i / 100, scene);
     sphere.position.x = Math.sin (i);
     sphere.position.y = 8 * i;
     sphere.position.z = Math.cos (i); } };

/**
 * Place furniture as found in the current scene.
 *
 * Affects Tootsville.Tank.scene
 */
Tootsville.SceneBuilder.build = function (x, y, z)
{ const scene = Tootsville.Tank.scene; };

/**
 *
 */
Tootsville.SceneBuilder.addItem1  = function (item)
{ console.warn ("Unimplemented: Not adding item₁ ", item); };

/**
 * Adds a furniture item in the ``itm2'' format
 *
 * See `ITEM-INFO'
 *
 * @verbatim
 * { uuid:
 *  position: {  x: y: z: },
 *  facing: radians,
 *  baseColor: color,
 *  altColor: color,
 *  energy: number,
 *  scale:  { x: y: z: },
 *  world:  { world: lat: long: alt: },
 *  template:
 *  { id:
 *  name:
 *  description:
 *  trade: [  Y N X  ],
 *  avatar:
 *  energyKind:
 *  energyMax:
 *  onZero:
 *  wearSlot:
 *  weight: } }
 * @end verbatim
 */
Tootsville.SceneBuilder.addItem2  = function (item)
{ Tootsville.FurnitureBuilder.build (item); };

/**
 *
 */
Tootsville.SceneBuilder.addFurn  = function (item)
{ console.warn ("Unimplemented: Not adding old furniture ", item); };

/**
 *
 */
Tootsville.SceneBuilder.addText  = function (item)
{ console.warn ("Unimplemented: Not adding text ", item); };

if (! ('places' in Tootsville.SceneBuilder))
{ Tootsville.SceneBuilder.places = {}; };

/**
 *
 */
Tootsville.SceneBuilder.addPlace  = function (key, info)
{ let [ kind, shapeInfo ] = info.split (':');
  let shapes = [];
  let shape = shapeInfo.split ('~').map ( el => {
      if ("" === el) { return null; }
      else { let [x,y,z] = el.split (',');
             return new BABYLON.Vector3 (parseFloat (x),
                                         parseFloat (y),
                                         parseFloat (z)); } } );
  /* FIXME break into shapes on nulls */
  shapes = [ shape ];
  let place = { kind: kind, shapes: shapes };
  Tootsville.SceneBuilder.places [ key ] = place; };

/**
 * WRITEME
 */
Tootsville.SceneBuilder.buildMistScene = function ()
{ Tootsville.FurnitureBuilder.build ({
    uuid: "mist",
    position: { x: 0, y: 0, z: 0 },
    facing: 0,
    baseColor: 'black', altColor: 'black',
    energy: 0, scale: { x: 1, y: 1, z: 1 },
    world: { world: Tootsville.activity.world,
             lat: Tootsville.activity.lat,
             long: Tootsville.activity.long,
             alt: Tootsville.activity.alt },
    template: { id: -1, name: 'mist', description: 'mist',
                trade: 'N',
                avatar: 'MIST',
                energyKind: null, energyMax: 1, onZero: null,
                wearSlot: null, weight: 0 }}); };

/**
 * WRITEME
 */
Tootsville.SceneBuilder.beginRefresh = function () {
    Tootsville.SceneBuilder.refreshInfo = {};
};

/**
 * WRITEME
 */
Tootsville.SceneBuilder.noteRefresh = function (uuid) {
    Tootsville.SceneBuilder.refreshInfo[ uuid ] = true;
};

/**
 * WRITEME
 */
Tootsville.SceneBuilder.endRefresh = function () {
    for (let uuid in Tootsville.Tank.scene.items)
    { if (! Tootsville.SceneBuilder.refreshInfo[ uuid ])
          Tootsville.FurnitureBuilder.destroy( uuid ); } };


