/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/ItemViewer.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { ItemViewer: {} }; }
if (!('ItemViewer' in Tootsville)) { Tootsville.ItemViewer = {}; }

/**
 * Create a scene to contain the Item Viewer
 */
Tootsville.ItemViewer.createScene = function (canvas)
{ canvas.engine = new BABYLON.Engine (canvas, true, { preserveDrawingBuffer: true, stencil: true,
                                                      height: 256, width: 256 } );
  canvas.scene = new BABYLON.Scene (canvas.engine);
  canvas.scene.clearColor = new BABYLON.Color3.FromHexString (Tootsville.UI.interpretTootColor ('cyan')); };

/**
 * Create a camera through which to observe the Item Viewer
 */
Tootsville.ItemViewer.createCamera = function (canvas, name)
{ const camera = new BABYLON.FreeCamera (
    'ItemViewer ' + name + ' Camera',
    new BABYLON.Vector3 (0, 4.5, -12.5),
    canvas.scene);

  camera.inputs.clear ();
  canvas.camera = camera; };

/**
 * Create a light source for the ItemViewer.
 */
Tootsville.ItemViewer.createLight = function (canvas)
{ canvas.light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    canvas.scene);} ;

/**
 * Render the ItemViewer scene only once.
 *
 * Then, grab a screenshot of it and put that into the image instead to
 * free up the WebGL context.
 */
Tootsville.ItemViewer.createViewerReally = function (info, canvas, image)
{ Tootsville.ItemViewer.createScene (canvas);
  Tootsville.ItemViewer.createCamera (canvas, info.template.name);
  Tootsville.ItemViewer.createLight (canvas);
  Tootsville.FurnitureBuilder.build (info, canvas.scene, function () {
      canvas.scene.render ();
      BABYLON.Tools.CreateScreenshot (canvas.engine, canvas.camera, 256,
                                      (data) =>
                                      { setTimeout ( () => {
                                          canvas.scene.dispose ();
                                          canvas.camera.dispose ();
                                          canvas.light.dispose ();
                                          canvas.engine.dispose ();
                                          canvas.engine = null;
                                          canvas.scene = null;
                                          canvas.camera = null;
                                          canvas.light = null; }, 4);
                                        image.src = data;
                                        image.alt = info.template.name;
                                        canvas.style.display = 'none';
                                        console.log ("Added ItemViewer for " + info.template.name);
                                      },
                                      'image/png'); } ); };

/**
 * Create a stand-alone Item Viewer in a CANVAS.
 *
 * Create a 3D  viewer with a single  item in it, out  of an arbitrary
 * CANVAS element. Creates a Babylon 3D scene with just the item.
 *
 */
Tootsville.ItemViewer.createViewerInCanvas = function (info, canvas, image)
{ if (! canvas) { return; }
  Tootsville.ItemViewer.createViewerReally (info, canvas, image); };
