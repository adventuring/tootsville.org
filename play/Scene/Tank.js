/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/Tank.js is part of Tootsville
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

if (! ("Tank" in Tootsville)) { Tootsville.Tank = { avatars: {} }; }
if (! ("avatars" in Tootsville.Tank)) { Tootsville.Tank.avatars = {}; }

/**
 * Indicates whether the 2D overlay attachments need updating.
 *
 * When true, the scene has changed  in some way that may invalidate the
 * positions of things like speech balloons.
 *
 * XXX This is  currently ignored, and we always  update the attachments
 * on every frame.
 */
Tootsville.Tank.attachmentOverlaysNeedUpdateP = false;

/**
 * Initialize  the Over-The-Shoulder  camera.
 *
 * This is the main follow camera  for the game. This camera follows the
 * player's Toot through the scene.
 */
Tootsville.Tank.initOTSCamera = function ()
{ const camera = new BABYLON.FollowCamera (
    'otsCamera',
    new BABYLON.Vector3 (0, 10, -100),
    Tootsville.Tank.scene);
  camera.radius = 30; /* how closely to follow our Toot */
  camera.heightOffset = 4.5;
  camera.rotationOffset = 0;
  camera.cameraAcceleration = .0005;
  camera.maxCameraSpeed = 5;
  //  camera.attachControl (Tootsville.Tank.scene.
  camera.ellipsoid = new BABYLON.Vector3 (50,10,50);
  camera.checkCollisions = true;
  Tootsville.Tank.camera = camera;
  console.log ("Created camera", camera);
  Tootsville.Tank.camera.onProjectionMatrixChangedObservable.add (
      () =>
          { Tootsville.Tank.attachmentOverlaysNeedUpdateP = true; } );
  Tootsville.Tank.camera.onViewMatrixChangedObservable.add (
      () =>
          { Tootsville.Tank.onViewMatrixChangedObservable = true; } );
  return camera; };

/**
 *
 */
Tootsville.Tank.initArcCamera = function ()
{ const camera = new BABYLON.ArcRotateCamera (
    "Tootsville Camera", 0, 0, 10,
    new BABYLON.Vector3 (0, 0, 0), Tootsville.Tank.scene);
  camera.setPosition (new BABYLON.Vector3 (0, 0, -20));
  Tootsville.Tank.camera = camera;
  console.log ("Created camera", camera);
  Tootsville.Tank.camera.onProjectionMatrixChangedObservable.add (
      () =>
          { Tootsville.Tank.attachmentOverlaysNeedUpdateP = true; } );
  Tootsville.Tank.camera.onViewMatrixChangedObservable.add (
      () =>
          { Tootsville.Tank.onViewMatrixChangedObservable = true; } );
  return camera; };

/**
 * Find or create the CANVAS object onto which the 3D scene is rendered.
 */
Tootsville.Tank.getCanvas = function ()
{ if (! Tootsville.Tank.canvas)
  { Tootsville.Tank.canvas =
    document.getElementById ('tootsville3d'); }
  if (! Tootsville.Tank.canvas)
  { const canvas = document.createElement ('CANVAS');
    canvas.id = 'tootsville3d';
    canvas.touchAction = 'none';
    document.getElementsByTagName ('BODY')[0].appendChild (canvas);
    Tootsville.Tank.canvas = canvas; }
  return Tootsville.Tank.canvas; };


/**
 * Initialize the Babylon 3D scene object.
 */
Tootsville.Tank.initScene = function ()
{ console.log ("Initializing the Babylon Scene");
  Tootsville.Tank.scene = new BABYLON.Scene (Tootsville.Tank.engine);
  Tootsville.Tank.scene.registerAfterRender (Tootsville.Tank.afterRender);
  Tootsville.Tank.scene.collisionsEnabled = true;
  return Tootsville.Tank.scene; };


/**
 * This event  handler is called  whenever a frame  in the 3D  scene has
 * been rendered.
 */
Tootsville.Tank.afterRender = function ()
{ Tootsville.Tank.updateCamera (); };

/**
 * Initialize the 3D engine, including Babylon 3D.
 *
 * The main  entry point  is `Tootsville.Tank.start3D'  which eventually
 * invokes this.  This function actually  connects the 3D engine  to the
 * CANVAS object and 2D event system.
 */
Tootsville.Tank.init3DEngine = function ()
{ return new Promise (
    (finish) =>
        { Tootsville.Tank.getCanvas ();
          if (! Tootsville.Tank.engine)
          { console.log ("Initializing Babylon3D as graphics engine for tank");
            Tootsville.Tank.engine =
            new BABYLON.Engine (Tootsville.Tank.canvas,
                                true,
                                { preserveDrawingBuffer: true, stencil: true } ); }
          if (! Tootsville.Tank.scene)
          { Tootsville.Tank.initScene (); }
          // TODO confirm if this is engine or scene:
          Tootsville.Tank.engine.workerCollisions = true;
          Tootsville.Tank.canvas.addEventListener (
              'click', Tootsville.UI.HUD.convertCanvasEventTo3D);

          console.log ("init3DEngine: ready");
          finish (); }); };


/**
 * Initialize our local player's Toot object.
 *
 * We know that it, at least, will always exist.
 */
Tootsville.Tank.initPlayerToot = function ()
{ if (! Tootsville.character)
  { Tootsville.Login.start ();
    return;}
  if (! Tootsville.Tank.avatars [ Tootsville.character ])
  { Tootsville.Util.infinity ("finger", { 0: Tootsville.character.name });
    return; }
  Tootsville.AvatarBuilder.build (
      Tootsville.Tank.avatars [ Tootsville.character ], Tootsville.Tank.scene); };

/**
 * Update the avatar model for avatarName, by looking up its description
 * Tootsville.Tank.avatars.
 */
Tootsville.Tank.updateAvatarFor = function (avatarName)
{ let avatar = Tootsville.Tank.avatars [ avatarName ];
  if (! (avatar && avatar.avatarClass))
  { console.warn ("Can't build avatar without info for " + avatarName);
    Tootsville.Util.infinity ("finger", { updateAvatar: avatarName });
    return; }
  if (Tootsville.Tank.scene)
  { Tootsville.AvatarBuilder.build (avatar, Tootsville.Tank.scene); } };

/**
 * Create the  text scene with ground  plane and the player's  Toot with
 * a static light.
 */
Tootsville.Tank.createScene = function ()
{ console.log ("Initializing thet Babylon WebGL Scene.");
  try { Tootsville.SkyBuilder.build (Tootsville.activity.world); }
  catch (e) { console.error (e); }
  try { Tootsville.GroundBuilder.build (Tootsville.activity.x,
                                        Tootsville.activity.y,
                                        Tootsville.activity.z); }
  catch (e) { console.error (e); }
  try { Tootsville.Tank.initPlayerToot (); }
  catch (e) { console.error (e); }
  try { Tootsville.SceneBuilder.build (Tootsville.activity.x,
                                       Tootsville.activity.y,
                                       Tootsville.activity.z); }
  catch (e) { console.error (e); }

  /* Optimize display on lower-end systems at the expense of frame rate, if necessary */
  var optimize = BABYLON.SceneOptimizerOptions.ModerateDegradationAllowed();
  optimize.targetFrameRate = 25;
  BABYLON.SceneOptimizer.OptimizeAsync(Tootsville.Tank.scene, optimize);

  /* Make emissive objects glow */
  new BABYLON.GlowLayer("glow", Tootsville.Tank.scene);

  console.log ("Initialized scene is now", Tootsville.Tank.scene);
  return Tootsville.Tank.scene; };

/**
 * Start the 3D render loop running.
 */
Tootsville.Tank.startRenderLoop = function ()
{ console.log ("Starting render loop for scene ", Tootsville.Tank.scene,
               " with render function ", Tootsville.Tank.scene.render);
  Tootsville.Tank.CameraManager.updateCamera ();
  setTimeout (function ()
              { Tootsville.Tank.engine.runRenderLoop ( function ()
                                                       { try { Tootsville.Tank.scene.render (); }
                                                         catch (e) {} }); },
              1); };

/**
 * Prepare the libraries needed for the 3D scene (Babylon.js).
 *
 * We can load these hefty libraries asynchronously whilst the player is
 * busy signing in.
 */
Tootsville.Tank.prepareFor3D = function ()
{ return new Promise (
    finish =>
    { if ( (()=> { try { return BABYLON.Vector3; } catch (e) { return false; }})() )
          { finish (); }
          else
          { Tootsville.Util.loadScript ('https://cdn.babylonjs.com/babylon.js').then (
              () => { Tootsville.Util.loadScript ('https://cdn.babylonjs.com/loaders/babylonjs.loaders.min.js').then (
                  () => { Tootsville.Tank.prepareFor3D (); }); });}});};

/**
 * Enqueue some foley sound effects that will be used in the scene.
 */
Tootsville.Tank.loadUISounds = function ()
{ var squawk = new BABYLON.Sound (
    "parrot-squawk",
    "https://jumbo.tootsville.org/Assets/Voices/parrot-squawk.wav",
    Tootsville.Tank.scene,
    function ()
    { Tootsville.Gossip.Parrot.squawk = squawk; }); };

/**
 * Start the 3D engine, after doing any necessary preparatory work.
 * This is the main entry point for the 3D simulation engine.
 */
Tootsville.Tank.start3D = function ()
{ Tootsville.Tank.prepareFor3D ().then (Tootsville.Tank.start3DReal); };

/**
 * Ensure  that all  libraries  are  loaded and  actually  start the  3D
 * engine. Called by `Tootsville.Tank.start3D'
 */
Tootsville.Tank.start3DReal = function ()
{ BABYLON.SceneLoader.ShowLoadingScreen = false;
  Tootsville.Tank.init3DEngine ().then (
      () =>
          { console.log ("3D libraries loaded");
            Tootsville.Tank.createScene ();
            Tootsville.Login.loginDone ();
            console.log ("Created game scene; starting rendering loop");
            setTimeout ( () => { Tootsville.Tank.initOTSCamera ();
                                 Tootsville.Tank.scene.activeCamera = Tootsville.Tank.camera; }, 100);
            Tootsville.Tank.startRenderLoop ();
            /* FIXME this seems to miss many resize events? */
            window.addEventListener ('resize',
                                     (ev) => { Tootsville.Tank.engine.resize (); }); } );

  return true; };


// 


/**
 * Find the child mesh with the greatest volume.
 *
 * If there are no children, returns the parent
 * mesh. Otherwise. always returns a child. Uses the radius of the
 * bounding sphere as a proxy for volume computations.
 */
Tootsville.Tank.getLargestChildMesh = function (object)
{ const children = object.getChildMeshes ();
  if (0 === children.length) { return object; }
  let largest = null;
  let largestSize = 0;
  for (let i = 0; i < children.length; ++i)
  { let child = children[i];
    let radius = child.getBoundingInfo().boundingSphere.radiusWorld;
    if (radius > largestSize)
    { largest = child; } }
  if (! largest)
  { return object; }
  return largest; };

/**
 * The avatar for the active local player
 */
Tootsville.Tank.playerAvatar = function ()
{ if ( (! Tootsville.Tank.scene) ||
       (! Tootsville.Tank.avatars) )
  { return null; }
  return Tootsville.Tank.avatars [ Tootsville.character ]; };

/**
 * Reposition the camera as needed to track the player
 */
Tootsville.Tank.updateCamera = function ()
{ const playerAvatar = Tootsville.Tank.playerAvatar ();
  if  ((! Tootsville.Tank.camera) ||
       (! playerAvatar))
  { return; }
  Tootsville.Tank.CameraManager.updateCamera (Tootsville.Tank.camera,
                                              playerAvatar,
                                              Tootsville.Tank.CameraManager.ZOOM_MODE_GAME); };

/**
 * Destroy avatar and attachments.
 */
Tootsville.Tank.destroyAvatar = function (avatar)
{ delete Tootsville.Tank.avatars [ avatar.name ];
  if (avatar.speech)
  { avatar.speech.parentNode.removeChild (avatar.speech); }
  if (avatar.nameTag)
  { avatar.nameTag.parentNode.removeChild (avatar.nameTag); }
  if (avatar.model)
  { avatar.model.dispose (); } };

/**
 * Find an avatar by name; may return null if we don't know about that
 * avatar yet.
 */
Tootsville.Tank.findAvatar = function (avatarName)
{ if (Tootsville.Tank.avatars [ avatarName ])
  { return Tootsville.Tank.avatars [ avatarName ]; }
  if (! Tootsville.Tank.scene)
  { return null; }
  for (let i = 0; i < Tootsville.Tank.scene.meshes; ++i)
  { if (Tootsville.Tank.scene.meshes [ i ].name === "avatar/" + avatarName)
    { return Tootsville.Tank.scene.meshes [ i ]; } }
  return null; };

/**
 * Shut down the 3D environment cleanly.
 */
Tootsville.Tank.shutDown = function ()
{ if (Tootsville.Tank.camera)
  { Tootsville.Tank.camera.dispose ();
    Tootsville.Tank.camera = null; }
  if (Tootsville.Tank.scene)
  { Tootsville.Tank.scene.dispose ();
    Tootsville.Tank.scene = null; }
  if (Tootsville.Tank.engine)
  { Tootsville.Tank.engine.dispose ();
    Tootsville.Tank.engine = null;}};

/**
* 
*/
Tootsville.Tank.clearSceneExceptPlayer = function () {
    for (let avatar in Tootsville.Tank.avatars)
        if (avatar !== Tootsville.character) {
            const a = Tootsville.Tank.avatars[avatar];
            if (a.model)
                a.model.dispose ();
            if (a.speech) 
                a.speech.parentNode.removeChild (a.speech);
            if (a.nameTag)
                a.nameTag.parentNode.removeChild (a.nameTag);
            delete Tootsville.Tank.avatars[avatar]; }
    for (let item in Tootsville.Tank.scene.items)
        if (Tootsville.Tank.scene.items[item].model) {
            Tootsville.Tank.scene.items[item].model.dispose ();
            delete Tootsville.Tank.scene.items[item]; }
    if (Tootsville.Tank.ground) {
        Tootsville.Tank.ground.dispose ();
        Tootsville.Tank.ground = null; } };
