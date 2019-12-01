/* -*- js2 -*-*/

/*@license
 *
 * ./play/scene/avatar-viewer.js is part of Tootsville
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
  return Tootsville.util.rest ('GET', 'toots/' + character); };

/**
 * Move a Toot by @code{δv}
 */
// Tootsville.Avatars.UltraTootBuilder.moveToot = function (toot, δv)
// { //         var forwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
//     // forwards.negate ();
//     // character.moveWithCollisions (forwards);
//     // // or
//     // var backwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, -gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
//     // character.moveWithCollisions (backwards);
// };

/**
 * Create a scene to contain the Avatar Viewer
 */
Tootsville.AvatarViewer.createScene = function (canvas)
{ canvas.engine = new BABYLON.Engine (canvas, true);
  canvas.scene = new BABYLON.Scene (canvas.engine);
  scene.clearColor = new BABYLON.Color3.FromHexString (interpretTootColor ('cyan')); };

/**
 * Create a camera through which to observe the Avatar Viewer
 */
Tootsville.AvatarViewer.createCamera = function (canvas)
{ const camera = new BABYLON.FollowCamera (
    'Toot Viewer camera',
    new BABYLON.Vector3 (0, 10, -100),
    canvas.scene);
  camera.radius = 1;
  camera.heightOffset = 4.5;
  camera.rotationOffset = 0;
  camera.cameraAcceleration = 1;
  camera.maxCameraSpeed = Infinity;
  canvas.camera = camera;
};

/**
 *
 */
Tootsville.AvatarViewer.createLight = function (canvas)
{ const light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    canvas.scene);} ;

/**
 *
 */
Tootsville.AvatarViewer.createToot = function (toot, canvas)
{ Tootsville.AvatarBuilder.build (toot, canvas.scene).then (
    model =>
        { console.info ('loaded a Toot into Viewer', canvas);
          canvas.camera.lockedTarget = model; }); };

/**
 *
 */
Tootsville.AvatarViewer.startRendering = function (canvas) {
    canvas.engine.runRenderLoop (canvas.scene.render);
    window.addEventListener ('resize', canvas.engine.resize);
};

/**
 * Create a 3D  viewer with a single  avatar in it, out  of an arbitrary
 * CANVAS element. Creates a Babylon 3D scene with just the avatar.
 */
Tootsville.AvatarViewer.createViewerInCanvas = function (toot, canvas)
{ if (! canvas) { return; }
  if (toot.charAt instanceof Function) // is a string
  { Tootsville.AvatarViewer.getAvatar (toot).then (t2 =>
                                              { Tootsville.AvatarViewer.createViewerInCanvas (t2, canvas); });
    return; }
  Tootsville.Tank.prepareFor3D ().then (
      () =>
          { Tootsville.AvatarViewer.createScene (canvas);
            canvas.physics = new BABYLON.CannonJSPlugin ();
            Tootsville.Tank.initPhysics ('Tootanga', canvas.scene, canvas.physics);
            Tootsville.AvatarViewer.createCamera (canvas);
            Tootsville.AvatarViewer.createLight (canvas);
            Tootsville.AvatarViewer.createToot (toot, canvas);
            Tootsville.AvatarViewer.startRendering (canvas); } ); };







