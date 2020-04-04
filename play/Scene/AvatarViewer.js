/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/AvatarViewer.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { AvatarViewer: {} }; }
if (!('AvatarViewer' in Tootsville)) { Tootsville.AvatarViewer = {}; }

/**
 * Get the avatar for the given character name.
 *
 * Returns a promise which resolves into character information.
 *
 * If character is falsey, returns a promise to an empty object.
 */
Tootsville.AvatarViewer.getAvatar = function (character)
{ if (!character) { return new Promise ( () => {} ); }
  return Tootsville.Util.rest ('GET', 'toots/' + character); };

/**
 * Create a scene to contain the Avatar Viewer
 */
Tootsville.AvatarViewer.createScene = function (canvas)
{ canvas.engine = new BABYLON.Engine (canvas, true, { preserveDrawingBuffer: true, stencil: true });
  canvas.scene = new BABYLON.Scene (canvas.engine);
  canvas.scene.clearColor = new BABYLON.Color3.FromHexString (interpretTootColor ('periwinkle')); };

/**
 * Create a camera through which to observe the Avatar Viewer
 */
Tootsville.AvatarViewer.createCamera = function (canvas, name)
{ const camera = new BABYLON.FreeCamera (
    'AvatarViewer ' + name + ' Camera',
    new BABYLON.Vector3 (0, 4.5, -12.5),
    canvas.scene);

  camera.target = BABYLON.Vector3.Zero;
  camera.inputs.clear ();
  canvas.camera = camera; };

/**
 * Create a light source for the AvatarViewer.
 */
Tootsville.AvatarViewer.createLight = function (canvas)
{ const light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    canvas.scene);} ;

/**
 * Begin the rendering in the viewer
 */
Tootsville.AvatarViewer.startRendering = function (canvas) {
    setTimeout (function ()
                { canvas.engine.runRenderLoop ( function ()
                                                { try { canvas.scene.render (); }
                                                  catch (e) {} }); },
                1);
    window.addEventListener ('resize', canvas.engine.resize);
};

Tootsville.AvatarViewer.acceptScreenshot = function (data)
{ canvas.engine.dispose ();
  canvas.engine = null;
  canvas.scene = null;
  canvas.camera = null;
  canvas.light = null;
  canvas.getContext('2d').drawImage (data,
                                     0, 0, 256, 256,
                                     0, 0, canvas.width, canvas.height); };

/**
 * Render the AvatarViewer scene only once.
 * 
 * Then, grab a screenshot of it and put that into the canvas instead to
 * free up the WebGL context.
 */
Tootsville.AvatarViewer.createViewerReally = function (toot, canvas)
{ Tootsville.AvatarViewer.createScene (canvas);
  Tootsville.AvatarViewer.createCamera (canvas, toot.name);
  Tootsville.AvatarViewer.createLight (canvas);
  Tootsville.AvatarBuilder.build (toot, canvas.scene);
  if ('avatars' in canvas.scene)
  { canvas.scene.avatars [toot.name].position = BABYLON.Vector3.Zero ();}
  canvas.scene.render ();
  BABYLON.Tools.CreateScreenshot (canvas.engine, canvas.camera, 256,
                                  Tootsville.AvatarViewer.acceptScreenshot,
                                  'image/png'); };

/**
 * Create a stand-alone Avatar Viewer in a CANVAS.
 * 
 * Create a 3D  viewer with a single  avatar in it, out  of an arbitrary
 * CANVAS element. Creates a Babylon 3D scene with just the avatar.
 *
 * This is useful for paperdolls, character selection, &c.
 *
 * The Toot info  (avatar info) passed in must be  in the form described
 * at    `TOOT-INFO',    and    will    be    ultimately    passed    to
 * `Tootsville.AvatarBuilder.build', qv.
 */
Tootsville.AvatarViewer.createViewerInCanvas = function (toot, canvas)
{ if (! canvas) { return; }
  if (toot.charAt) // is a string
  { Tootsville.AvatarViewer.getAvatar (toot).then (t2 =>
                                              { Tootsville.AvatarViewer.createViewerInCanvas (t2, canvas); });
    return; }
  Tootsville.Tank.prepareFor3D ().then ( () => { Tootsville.AvatarViewer.createViewerReally (toot, canvas); } ); };

