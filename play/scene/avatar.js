/* -*- js2 -*-*/

/*@license
 *
 * ./play/scene/avatar.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { Avatars: { AvatarBuilder: {}, Viewer: {}  } }; }
if (!('Avatars' in Tootsville)) { Tootsville.Avatars = { AvatarBuilder: {}, Viewer: {}  }; }
if (!('Viewer' in Tootsville.Avatars)) { Tootsville.Avatars.Viewer = {}; }

/**
 * Get the avatar for the given character name.
 *
 * Returns a promise which resolves into character information.
 *
 * If character is falsey, returns a promise to an empty object.
 */
Tootsville.Avatars.getAvatar = function (character)
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
Tootsville.Avatars.Viewer.createScene = function (canvas)
{ canvas.engine = new BABYLON.Engine (canvas, true);
  canvas.scene = new BABYLON.Scene (canvas.engine);
  scene.clearColor = new BABYLON.Color3.FromHexString (interpretTootColor ('cyan')); };

/**
 * Create a camera through which to observe the Avatar Viewer
 */
Tootsville.Avatars.Viewer.createCamera = function (canvas)
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
Tootsville.Avatars.Viewer.createLight = function (canvas)
{ const light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    canvas.scene);} ;

/**
 *
 */
Tootsville.Avatars.Viewer.createToot = function (toot, canvas)
{ Tootsville.Avatars.AvatarBuilder.build (toot, canvas.scene).then (
    model =>
        { console.info ('loaded a Toot into Viewer', canvas);
          canvas.camera.lockedTarget = model; }); };

/**
 *
 */
Tootsville.Avatars.Viewer.startRendering = function (canvas) {
    canvas.engine.runRenderLoop (canvas.scene.render);
    window.addEventListener ('resize', canvas.engine.resize);
};

/**
 * Create a 3D  viewer with a single  avatar in it, out  of an arbitrary
 * CANVAS element. Creates a Babylon 3D scene with just the avatar.
 */
Tootsville.Avatars.createViewerInCanvas = function (toot, canvas)
{ if (! canvas) { return; }
  if (toot.charAt instanceof Function) // is a string
  { Tootsville.Avatars.getAvatar (toot).then (t2 =>
                                              { Tootsville.Avatars.createViewerInCanvas (t2, canvas); });
    return; }
  Tootsville.tank.prepareFor3D ().then (
      () =>
          { Tootsville.Avatars.Viewer.createScene (canvas);
            canvas.physics = new BABYLON.CannonJSPlugin ();
            Tootsville.tank.initPhysics ('Tootanga', canvas.scene, canvas.physics);
            Tootsville.Avatars.Viewer.createCamera (canvas);
            Tootsville.Avatars.Viewer.createLight (canvas);
            Tootsville.Avatars.Viewer.createToot (toot, canvas);
            Tootsville.Avatars.Viewer.startRendering (canvas); } ); };








if (!('AvatarBuilder' in Tootsville.Avatars)) { Tootsville.Avatars.AvatarBuilder = { baseAvatars: {} }; }

if (!('baseAvatars' in Tootsville.Avatars.AvatarBuilder))
{ Tootsville.Avatars.AvatarBuilder.baseAvatars = {}; }

Tootsville.Avatars.AvatarBuilder.colorize = function (avatar, node, scene, finish)
{ const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  skinMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.baseColor));
  /* TODO apply pattern texture map to the skin */
  const padMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  padMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.padColor));
  const eyeMaterial = new BABYLON.StandardMaterial ("eye", scene);
  eyeMaterial.diffuseColor = new BABYLON.Color3.FromHexString ('#000000');
  const meshes = node.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes[i];
    mesh.material = (
        mesh.name.indexOf ('Eye') >= 0  ? eyeMaterial :
            mesh.name.indexOf ('Skin') >= 0 ? skinMaterial :
            mesh.name.indexOf ('Pad') >= 0 ? padMaterial :
            eyeMaterial );
    console.debug ("Colorized mesh named " + mesh.name + " with material ", mesh.material.name); }
  if (finish) { finish (node); } };

Tootsville.Avatars.AvatarBuilder.build2 = function (avatar, root, scene, finish)
{ console.debug ("Building " + avatar.name + " as a " + avatar.avatar + " avatar in scene ", scene);
  var object = root.clone ("avatar/" + avatar.name);
  Tootsville.tank.shadowGenerator.getShadowMap ().renderList.push (object);
  console.debug (avatar.name, "δ", object);
  // object.physicsImpostor = new BABYLON.PhysicsImpostor (object,
  //                                                       BABYLON.PhysicsImpostor.SphereImpostor,
  //                                                       { mass: 1, restitution: 0.9 },
  //                                                       scene);
  console.debug (avatar.name, "ε", object);
  object.position = new BABYLON.Vector3 (0,0,0); /* TODO */
  if (root.skeleton)
  { object.skeleton = root.skeleton.clone ("skeleton/" + avatar.name); }
  Tootsville.avatars [avatar.name] = Object.assign ({}, avatar);
  Tootsville.avatars [avatar.name].model = object;
  Tootsville.Avatars.AvatarBuilder.colorize (avatar, root, scene, finish); };

Tootsville.Avatars.AvatarBuilder.build = function (avatar, scene, finish)
{ const root = Tootsville.Avatars.AvatarBuilder.baseAvatars [ avatar.avatar ];
  if (root)
  { Tootsville.Avatars.AvatarBuilder.build2 (avatar, root, scene, finish); }
  else
  { Tootsville.Avatars.AvatarBuilder.loadAvatarBase (avatar, scene, finish); } };

Tootsville.Avatars.AvatarBuilder.loadAvatarBase = function (avatar, scene, finish)
{ var assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  var loadTask = assetsManager.addMeshTask ("loading UltraToot", null, "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            avatar.avatar + ".babylon");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.TransformNode ("baseAvatar/" + avatar.avatar, scene, true);
    modelRoot.position = new BABYLON.Vector3 (0, -10, 0);
    var i;
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { task.loadedMeshes [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { task.loadedParticleSystems [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { task.loadedSkeletons [i].setParent (modelRoot); }
    Tootsville.Avatars.AvatarBuilder.baseAvatars [avatar.avatar] = modelRoot;
    console.debug ("Loaded base avatar " + avatar.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    Tootsville.Avatars.AvatarBuilder.build2 (avatar, modelRoot, scene, finish); };
  assetsManager.load (); };
