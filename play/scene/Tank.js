/* -*- js2 -*-*/

/**@license
 *
 * ./play/scene/tank.js is part of Tootsville
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

if (! ("Tank" in Tootsville)) { Tootsville.Tank = {}; }

/**
 * Accept a click in the scene and identify the next action to take.
 */
Tootsville.Tank.acceptClick = function (event, pickedP, distance,
                                        pickedMesh, pickedPoint)
{ if (! pickedP) { return; }
  if (Tootsville.pickTargetListener)
  { Tootsville.pickTargetListener (pickedMesh, pickedPoint); }
  else if (pickedMesh.acceptAction)
  { pickedMesh.acceptAction ({ method: 'click',
                               target: pickedMesh,
                               point: pickedPoint }); } };

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
  camera.radius = 5; /* how closely to follow our Toot */
  camera.heightOffset = 4.5;
  camera.rotationOffset = 0;
  camera.cameraAcceleration = .005;
  camera.maxCameraSpeed = 10;
  // camera.ellipsoid = new BABYLON.Vector3 (.5,1,.5);
  // camera.checkCollisions = true;
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
 * Initialize the Cannon physics engine in the scene.
 */
Tootsville.Tank.initPhysics = function (world, scene, physics)
{ if (!scene)
  { scene = Tootsville.Tank.scene; }
  if (!physics)
  { physics = Tootsville.Tank.physics; }
  const gravityVector =
        new BABYLON.Vector3 (0,
                             -Tootsville.Worlds[world].Gravity,
                             0);
  scene.enablePhysics (gravityVector, physics);
  console.log ("Physics enabled for world", world);};

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
 * Convert an event on the CANVAS object into a 3D event as appropriate.
 */
Tootsville.Tank.convertCanvasEventTo3D = function (event)
{ const picked = Tootsville.Tank.scene.pick (
    Tootsville.Tank.scene.pointerX,
    Tootsville.Tank.scene.pointerY);
  Tootsville.Tank.acceptClick (event, picked.hit,
                               picked.distance,
                               picked.pickedMesh,
                               picked.pickedPoint); };

/**
 * Initialize the Babylon 3D scene object.
 */
Tootsville.Tank.initScene = function ()
{ console.log ("Initializing the Babylon Scene");
  Tootsville.Tank.scene = new BABYLON.Scene (Tootsville.Tank.engine);
  Tootsville.Tank.scene.registerAfterRender (Tootsville.Tank.afterRender);
  return Tootsville.Tank.scene; };

/**
 * Update one 2D attachment object.
*
* These attachments  are used  for avatar  labels, speech  balloons, &c.
* and need to  be updated to keep  in sync with the  underlying 3D scene
* from time to time.
 */
Tootsville.Tank.updateAttachment = function (model, attachment)
{ const center = BABYLON.Vector3.Project (
    model.getAbsolutePosition (),
    BABYLON.Matrix.IdentityReadOnly,
    Tootsville.Tank.scene.getTransformMatrix (),
    Tootsville.Tank.camera.viewport.toGlobal (
        Tootsville.Tank.engine.getRenderWidth (),
        Tootsville.Tank.engine.getRenderHeight ()));
  attachment.style.left = center.x;
  /* FIXME: use bounding box height */
  if ('bottom' == attachment.slot)
  { attachment.style.top = center.y + 100; }
  else
  { attachment.style.top = center.y - 100; } };

/**
 * Update the 2D attachments for one avatar. 
 */
Tootsville.Tank.updateAttachmentsForAvatar = function (avatar)
{ if (avatar.label)
  { Tootsville.Tank.updateAttachment (avatar.model, avatar.label); }
  if (avatar.speech)
  { Tootsville.Tank.updateAttachment (avatar.model, avatar.speech); } };

/**
 * Update all 2D attachment overlays to follow the 3D scene.
 */
Tootsville.Tank.updateAttachmentOverlays = function ()
{ for (let i = 0; i < Tootsville.avatars.length; ++i)
  { Tootsville.Tank.updateAttachmentsForAvatar (Tootsville.avatars [i]); } };

/**
 * This event  handler is called  whenever a frame  in the 3D  scene has
 * been rendered.
 */
Tootsville.Tank.afterRender = function ()
{ if (true || Tootsville.Tank.attachmentOverlaysNeedUpdateP)
  { Tootsville.Tank.updateAttachmentOverlays ();
    Tootsville.Tank.attachmentOverlaysNeedUpdateP = false; } };

/**
 * Initialize the 3D engine, including Babylon 3D and Cannon physics.
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
                                true); }
          if (! Tootsville.Tank.physics)
          { console.log ("Enabling CannonJS physics engine for tank");
            Tootsville.Tank.physics = new BABYLON.CannonJSPlugin (); }
          if (! Tootsville.Tank.scene)
          { Tootsville.Tank.initScene (); }
          // TODO confirm if this is engine or scene:
          Tootsville.Tank.engine.workerCollisions = true;
          Tootsville.Tank.canvas.addEventListener (
              'click',
              Tootsville.Tank.convertCanvasEventTo3D);
          console.log ("init3DEngine: ready");
          finish (); }); };


/**
 * Initialize our local player's Toot object. 
 *
 * We know that it, at least, will always exist.
 */
Tootsville.Tank.initPlayerToot = function ()
{ if ( (! (Tootsville.character))
       ||
       (! (Tootsville.character.avatar)) ) { Tootsville.Login.start ();
                                             return;}
  Tootsville.AvatarBuilder.build (
      Tootsville.character, Tootsville.Tank.scene,
      model => { Tootsville.Tank.camera.lockedTarget = model; } ); };


/**
 * Create the  text scene with ground  plane and the player's  Toot with
 * a static light.
 */
Tootsville.Tank.createTestScene = function ()
{ console.log ("Creating a test scene with an over-the-shoulder camera, ground plane, light, and Toot.");
  console.log ("Babylon scene object is ", Tootsville.Tank.scene);
  Tootsville.Tank.initOTSCamera ();
  Tootsville.Tank.initPhysics ('Tootanga');
  Tootsville.SkyBuilder.build ('CHOR'); /* XXX: other worlds some day */
  Tootsville.GroundBuilder.build (0, 0, 0); /* TODO x, y, z */
  Tootsville.Tank.initPlayerToot ();
  Tootsville.SceneBuilder.build (0, 0, 0); /* TODO x, y, z */
  console.log ("Initialized scene is now", Tootsville.Tank.scene);
  return Tootsville.Tank.scene; };

/**
 * Start the 3D render loop running.
 */
Tootsville.Tank.startRenderLoop = function ()
{ console.log ("Starting render loop for scene ", Tootsville.Tank.scene,
               " with render function ", Tootsville.Tank.scene.render);
  setTimeout (function ()
              { Tootsville.Tank.engine.runRenderLoop ( function ()
                                                       { try { Tootsville.Tank.scene.render () }
                                                         catch (e) {} }); },
              1); };

/**
 * Prepare the libraries needed for the 3D scene (Babylon and Cannon).
 *
 * We can load these hefty libraries asynchronously whilst the player is
 * busy signing in.
 */
Tootsville.Tank.prepareFor3D = function ()
{ return new Promise (
    finish =>
        { if ( ("BABYLON" in window) &&
               ("CANNON" in window))
          { finish (); }
          else if ("BABYLON" in window)
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/cannon.js').then (
              Tootsville.Tank.prepareFor3D); }
          else
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/babylon.max.js'
                                        /*'https://cdn.babylonjs.com/babylon.js'*/).then (
              Tootsville.Tank.prepareFor3D); }}); };

/**
 * Enqueue some foley sound effects that will be used in the scene.
 */
Tootsville.Tank.loadUISounds = function ()
{ var squawk = new BABYLON.Sound (
    "parrot-squawk",
    "https://jumbo.tootsville.org/Assets/Voices/parrot-squawk.wav",
    Tootsville.scene,
    function ()
    { Tootsville.parrot.squawk = squawk; }); };

/**
 * Start  the 3D  engine, after  doing any  necessary preparatory  work.
 * This is the main entry point for the 3D simulation engine.
 */
Tootsville.Tank.start3D = function ()
{ Tootsville.Tank.prepareFor3D ().then (Tootsville.Tank.start3DIfReady); };

/**
 * Ensure  that all  libraries  are  loaded and  actually  start the  3D
 * engine. Called by `Tootsville.Tank.start3D'
 */
Tootsville.Tank.start3DIfReady = function ()
    { if ( (! ("BABYLON" in window)) || (! ("CANNON" in window)))
      { return Tootsville.Tank.prepareFor3D ().then (Tootsville.Tank.start3DifReady); }
      BABYLON.SceneLoader.ShowLoadingScreen = false;
      Tootsville.Tank.init3DEngine ().then (
          () =>
              { console.log ("3D libraries loaded");
                Tootsville.Tank.createTestScene ();
                console.log ("Created test scene; starting rendering loop");
                Tootsville.Tank.startRenderLoop ();
                window.addEventListener ('resize',
                                         Tootsville.Tank.engine.resize); } );

      return true; };