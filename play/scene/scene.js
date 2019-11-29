/* -*- js2 -*-*/

/*@license
 *
 * ./play/scene/scene.js is part of Tootsville
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

if (! ("tank" in Tootsville)) { Tootsville.tank = {}; }

/**
 * Accept a click in the scene and identify the next action to take.
 */
Tootsville.tank.acceptClick = function (event, pickedP, distance,
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
Tootsville.tank.attachmentOverlaysNeedUpdateP = false;

/**
 * Initialize  the Over-The-Shoulder  camera. 
 *
 * This is the main follow camera  for the game. This camera follows the
 * player's Toot through the scene.
 */
Tootsville.tank.initOTSCamera = function ()
{ const camera = new BABYLON.FollowCamera (
    'otsCamera',
    new BABYLON.Vector3 (0, 10, -100),
    Tootsville.tank.scene);
  camera.radius = 5; /* how closely to follow our Toot */
  camera.heightOffset = 4.5;
  camera.rotationOffset = 0;
  camera.cameraAcceleration = .005;
  camera.maxCameraSpeed = 10;
  // camera.ellipsoid = new BABYLON.Vector3 (.5,1,.5);
  // camera.checkCollisions = true;
  Tootsville.tank.camera = camera;
  console.log ("Created camera", camera);
  Tootsville.tank.camera.onProjectionMatrixChangedObservable.add (
      () =>
          { Tootsville.tank.attachmentOverlaysNeedUpdateP = true; } );
  Tootsville.tank.camera.onViewMatrixChangedObservable.add (
      () =>
          { Tootsville.tank.onViewMatrixChangedObservable = true; } );
  return camera; };

/**
 * Initialize the Cannon physics engine in the scene.
 */
Tootsville.tank.initPhysics = function (world)
{ const gravityVector =
        new BABYLON.Vector3 (0,
                             -Tootsville.Worlds[world].Gravity,
                             0);
  Tootsville.tank.scene.enablePhysics (gravityVector,
                                       Tootsville.tank.physics);
  console.log ("Physics enabled for world", world);};

/**
 * Find or create the CANVAS object onto which the 3D scene is rendered.
 */
Tootsville.tank.getCanvas = function ()
{ if (! Tootsville.tank.canvas)
  { Tootsville.tank.canvas =
    document.getElementById ('tootsville3d'); }
  if (! Tootsville.tank.canvas)
  { const canvas = document.createElement ('CANVAS');
    canvas.id = 'tootsville3d';
    canvas.touchAction = 'none';
    document.getElementsByTagName ('BODY')[0].appendChild (canvas);
    Tootsville.tank.canvas = canvas; }
  return Tootsville.tank.canvas; };

/**
 * Convert an event on the CANVAS object into a 3D event as appropriate.
 */
Tootsville.tank.convertCanvasEventTo3D = function (event)
{ const picked = Tootsville.tank.scene.pick (
    Tootsville.tank.scene.pointerX,
    Tootsville.tank.scene.pointerY);
  Tootsville.tank.acceptClick (event, picked.hit,
                               picked.distance,
                               picked.pickedMesh,
                               picked.pickedPoint); };

/**
 * Initialize the Babylon 3D scene object.
 */
Tootsville.tank.initScene = function ()
{ console.log ("Initializing the Babylon Scene");
  Tootsville.tank.scene = new BABYLON.Scene (Tootsville.tank.engine);
  Tootsville.tank.scene.registerAfterRender (Tootsville.tank.afterRender);
  return Tootsville.tank.scene; };

/**
 * Update one 2D attachment object.
*
* These attachments  are used  for avatar  labels, speech  balloons, &c.
* and need to  be updated to keep  in sync with the  underlying 3D scene
* from time to time.
 */
Tootsville.tank.updateAttachment = function (model, attachment)
{ const center = BABYLON.Vector3.Project (
    model.getAbsolutePosition (),
    BABYLON.Matrix.IdentityReadOnly,
    Tootsville.tank.scene.getTransformMatrix (),
    Tootsville.tank.camera.viewport.toGlobal (
        Tootsville.tank.engine.getRenderWidth (),
        Tootsville.tank.engine.getRenderHeight ()));
  attachment.style.left = center.x;
  /* FIXME: use bounding box height */
  if ('bottom' == attachment.slot)
  { attachment.style.top = center.y + 100; }
  else
  { attachment.style.top = center.y - 100; } };

/**
 * Update the 2D attachments for one avatar. 
 */
Tootsville.tank.updateAttachmentsForAvatar = function (avatar)
{ if (avatar.label)
  { Tootsville.tank.updateAttachment (avatar.model, avatar.label); }
  if (avatar.speech)
  { Tootsville.tank.updateAttachment (avatar.model, avatar.speech); } };

/**
 * Update all 2D attachment overlays to follow the 3D scene.
 */
Tootsville.tank.updateAttachmentOverlays = function ()
{ for (let i = 0; i < Tootsville.game.avatars.length; ++i)
  { Tootsville.tank.updateAttachmentsForAvatar (Tootsville.game.avatars [i]); } };

/**
 * This event  handler is called  whenever a frame  in the 3D  scene has
 * been rendered.
 */
Tootsville.tank.afterRender = function ()
{ if (true || Tootsville.tank.attachmentOverlaysNeedUpdateP)
  { Tootsville.tank.updateAttachmentOverlays ();
    Tootsville.tank.attachmentOverlaysNeedUpdateP = false; } };

/**
 * Initialize the 3D engine, including Babylon 3D and Cannon physics.
 *
 * The main  entry point  is `Tootsville.tank.start3D'  which eventually
 * invokes this.  This function actually  connects the 3D engine  to the
 * CANVAS object and 2D event system.
 */
Tootsville.tank.init3DEngine = function ()
{ return new Promise (
    (finish) =>
        { Tootsville.tank.getCanvas ();
          if (! Tootsville.tank.engine)
          { console.log ("Initializing Babylon3D as graphics engine for tank");
            Tootsville.tank.engine =
            new BABYLON.Engine (Tootsville.tank.canvas,
                                true); }
          if (! Tootsville.tank.physics)
          { console.log ("Enabling CannonJS physics engine for tank");
            Tootsville.tank.physics = new BABYLON.CannonJSPlugin (); }
          if (! Tootsville.tank.scene)
          { Tootsville.tank.initScene (); }
          // TODO confirm if this is engine or scene:
          Tootsville.tank.engine.workerCollisions = true;
          Tootsville.tank.canvas.addEventListener (
              'click',
              Tootsville.tank.convertCanvasEventTo3D);
          console.log ("init3DEngine: ready");
          finish (); }); };

/**
 * Initialize a default  fill light source for the  game, until in-world
 * light sources are working.
 *
 * XXX Remove thi once the game has its own lights.
 */
Tootsville.tank.initCrappyDefaultLight = function ()
{ var light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    Tootsville.tank.scene); };

/**
 * Initialize our local player's Toot object. 
 *
 * We know that it, at least, will always exist.
 */
Tootsville.tank.initPlayerToot = function ()
{ if ( (! (Tootsville.character))
       ||
       (! (Tootsville.character.name)) ) { Tootsville.login.start ();
                                           return;}
  Tootsville.Avatars.getAvatar
  (Tootsville.character.name).then (
      (toot) =>
          { Tootsville.Avatars.UltraTootBuilder.makeToot (
              toot
          ).then (
              (ultraToot) =>
                  { console.log ('loaded a Toot');
                    Tootsville.tank.camera.lockedTarget =
                    ultraToot; }); }); };

/**
 * Initialize the ground plane.
 *
 * TODO: have a height map across the groundplane.
 */
Tootsville.tank.initGroundPlane = function ()
{ const ground =
        BABYLON.Mesh.CreateGround ('ground',
                                   { height: 100, width: 100,
                                     subdivisions: 10 },
                                   Tootsville.tank.scene);
  ground.material =
  new BABYLON.StandardMaterial ('ground',
                                Tootsville.tank.scene);
  ground.material.diffuseColor =
  new BABYLON.Color3.FromHexString (interpretTootColor ('green'));
  ground.physicsImpostor =
  new BABYLON.PhysicsImpostor (
      ground,
      BABYLON.PhysicsImpostor.BoxImpostor,
      { mass: 0, restitution: 3 });
  ground.checkCollisions = true;
  console.log ("Ground plane is ", ground);
  return ground; };

/**
 * Create the  text scene with ground  plane and the player's  Toot with
 * a static light.
 */
Tootsville.tank.createTestScene = function ()
{ console.log ("Creating a test scene with an over-the-shoulder camera, ground plane, light, and Toot.");
  console.log ("Babylon scene object is ", Tootsville.tank.scene);
  Tootsville.tank.initOTSCamera ();
  Tootsville.tank.initPhysics ('Tootanga');
  Tootsville.tank.initCrappyDefaultLight ();
  Tootsville.tank.initPlayerToot ();
  Tootsville.tank.initGroundPlane ();
  console.log ("Initialized scene is now", Tootsville.tank.scene);
  return Tootsville.tank.scene; };

/**
 * Start the 3D render loop running.
 */
Tootsville.tank.startRenderLoop = function ()
{ console.log ("Starting render loop for scene ", Tootsville.tank.scene,
               " with render function " , Tootsville.tank.scene.render);
  Tootsville.tank.engine.runRenderLoop (Tootsville.tank.scene.render); };

/**
 * Prepare the libraries needed for the 3D scene (Babylon and Cannon).
 *
 * We can load these hefty libraries asynchronously whilst the player is
 * busy signing in.
 */
Tootsville.tank.prepareFor3D = function ()
{ return new Promise (
    finish =>
        { if ( ("BABYLON" in window) &&
               ("CANNON" in window))
          { finish (); }
          else if ("BABYLON" in window)
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/cannon.js').then (
              Tootsville.tank.prepareFor3D); }
          else
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/babylon.max.js'
                                        /*'https://cdn.babylonjs.com/babylon.js'*/).then (
              Tootsville.tank.prepareFor3D); }}); };

/**
 * Enqueue some foley sound effects that will be used in the scene.
 */
Tootsville.tank.loadUISounds = function ()
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
Tootsville.tank.start3D = function ()
{ Tootsville.tank.prepareFor3D ().then (Tootsville.tank.start3DIfReady); };

/**
 * Ensure  that all  libraries  are  loaded and  actually  start the  3D
 * engine. Called by `Tootsville.tank.start3D'
 */
Tootsville.tank.start3DIfReady = function ()
    { if ( (! ("BABYLON" in window)) || (! ("CANNON" in window)))
      { return Tootsville.tank.prepareFor3D ().then (Tootsville.tank.start3DifReady); }
      BABYLON.SceneLoader.ShowLoadingScreen = false;
      Tootsville.tank.init3DEngine ().then (
          () =>
              { console.log ("3D libraries loaded");
                Tootsville.tank.createTestScene ();
                console.log ("Created test scene; starting rendering loop");
                Tootsville.tank.startRenderLoop ();
                window.addEventListener ('resize',
                                         Tootsville.tank.engine.resize); } );

      return true; };
