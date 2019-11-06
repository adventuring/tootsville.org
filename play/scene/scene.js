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

if (! ("tank" in Tootsville)) {
    Tootsville.tank = {};
}

Tootsville.tank.acceptClick = function (event, pickedP, distance,
                                        pickedMesh, pickedPoint)
{ if (! pickedP) { return; }
  if (Tootsville.pickTargetListener)
  { Tootsville.pickTargetListener (pickedMesh, pickedPoint); }
  else if (pickedMesh.acceptAction)
  { pickedMesh.acceptAction ({ method: 'click',
                               target: pickedMesh,
                               point: pickedPoint }); } };

Tootsville.tank.attachmentOverlaysNeedUpdateP = false;

Tootsville.tank.initOTSCamera = function ()
{ var camera = new BABYLON.FollowCamera (
    'otsCamera',
    new BABYLON.Vector3 (0, 10, -100),
    this.scene);
  camera.radius = 5; /* how closely to follow our Toot */
  camera.heightOffset = 4.5;
  camera.rotationOffset = 0;
  camera.cameraAcceleration = .005;
  camera.maxCameraSpeed = 10;
  // camera.ellipsoid = new BABYLON.Vector3 (.5,1,.5);
  // camera.checkCollisions = true;
  this.camera = camera;
  console.log ("Created camera", camera);
  this.camera.onProjectionMatrixChangedObservable.add (
      () =>
          { this.attachmentOverlaysNeedUpdateP = true; } );
  this.camera.onViewMatrixChangedObservable.add (
      () =>
          { this.onViewMatrixChangedObservable = true; } );
  return camera; };

Tootsville.tank.initPhysics = function (world)
{ var gravityVector =
      new BABYLON.Vector3 (0,
                           -Tootsville.Worlds[world].Gravity,
                           0);
  this.scene.enablePhysics (gravityVector,
                            this.physics);
  console.log ("Physics enabled for world", world);};

Tootsville.tank.getCanvas = function ()
{ if (! this.canvas)
  { this.canvas =
    document.getElementById ('tootsville3d'); }
  if (! this.canvas)
  { var canvas = document.createElement ('CANVAS');
    canvas.id = 'tootsville3d';
    canvas.touchAction = 'none';
    document.getElementsByTagName ('BODY')[0].appendChild (canvas);
    this.canvas = canvas; }
  return this.canvas; };

Tootsville.tank.convertCanvasEventTo3D = function (event)
{ var picked = this.scene.pick (
    this.scene.pointerX,
    this.scene.pointerY);
  this.acceptClick (event, picked.hit,
                               picked.distance,
                               picked.pickedMesh,
                               picked.pickedPoint); };

Tootsville.tank.initScene = function ()
{ console.log ("Initializing the Babylon Scene");
  this.scene = new BABYLON.Scene (this.engine);
  this.scene.registerAfterRender (this.afterRender);
  return this.scene; };

Tootsville.tank.updateAttachment = function (model, attachment)
{ const center = BABYLON.Vector3.Project (
    model.getAbsolutePosition (),
    BABYLON.Matrix.IdentityReadOnly,
    this.scene.getTransformMatrix (),
    this.camera.viewport.toGlobal (
        this.engine.getRenderWidth (),
        this.engine.getRenderHeight ()));
  attachment.style.left = center.x;
  /* FIXME: use bounding box height */
  if ('bottom' == attachment.slot)
  { attachment.style.top = center.y + 100; }
  else
  { attachment.style.top = center.y - 100; } };

Tootsville.tank.updateAttachmentsForAvatar = function (avatar)
{ if (avatar.label)
  { this.updateAttachment (avatar.model, avatar.label); }
  if (avatar.speech)
  { this.updateAttachment (avatar.model, avatar.speech); } };

Tootsville.tank.updateAttachmentOverlays = function ()
{ for (let i = 0; i < Tootsville.game.avatars.length; ++i)
  { this.updateAttachmentsForAvatar (Tootsville.game.avatars [i]); } };

Tootsville.tank.afterRender = function ()
{ if (true || this.attachmentOverlaysNeedUpdateP)
  { this.updateAttachmentOverlays ();
    this.attachmentOverlaysNeedUpdateP = false; } };

Tootsville.tank.init3DEngine = function ()
{ return new Promise (
    (finish) =>
        { this.getCanvas ();
          if (! this.engine)
          { console.log ("Initializing Babylon3D as graphics engine for tank");
            this.engine =
            new BABYLON.Engine (this.canvas,
                                true); }
          if (! this.physics)
          { console.log ("Enabling CannonJS physics engine for tank");
            this.physics = new BABYLON.CannonJSPlugin (); }
          if (! this.scene)
          { this.initScene (); }
          // TODO confirm if this is engine or scene:
          this.engine.workerCollisions = true;
          this.canvas.addEventListener (
              'click',
              this.convertCanvasEventTo3D);
          console.log ("init3DEngine: ready");
          finish (); }); };

Tootsville.tank.initCrappyDefaultLight = function ()
{ var light = new BABYLON.HemisphericLight (
    'uplight',
    new BABYLON.Vector3 (0,1,0),
    this.scene); };

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
                    this.camera.lockedTarget =
                    ultraToot; }); }); };

Tootsville.tank.initGroundPlane = function ()
{ var ground =
      BABYLON.Mesh.CreateGround ('ground',
                                 { height: 100, width: 100,
                                   subdivisions: 10 },
                                 this.scene);
  ground.material =
  new BABYLON.StandardMaterial ('ground',
                                this.scene);
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

Tootsville.tank.createTestScene = function ()
{ console.log ("Creating a test scene with an over-the-shoulder camera, ground plane, light, and Toot.");
  console.log ("Babylon scene object is ", this.scene);
  this.initOTSCamera ();
  this.initPhysics ('Tootanga');
  this.initCrappyDefaultLight ();
  this.initPlayerToot ();
  this.initGroundPlane ();
  console.log ("Initialized scene is now", this.scene);
  return this.scene; };

Tootsville.tank.startRenderLoop = function ()
{ console.log ("Starting render loop for scene ", this.scene,
               " with render function " , this.scene.render);
  this.engine.runRenderLoop (
      function ()
      { this.scene.render (); } ); };

Tootsville.tank.prepareFor3D = function ()
{ return new Promise (
    finish =>
        { if ( ("BABYLON" in window) &&
               ("CANNON" in window))
          { finish (); }
          else if ("BABYLON" in window)
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/cannon.js').then (
              this.prepareFor3D); }
          else
          { Tootsville.util.loadScript ('https://cdn.babylonjs.com/babylon.js').then (
              this.prepareFor3D); }}); };

Tootsville.tank.loadUISounds = function ()
{ var squawk = new BABYLON.Sound (
    "parrot-squawk",
    "https://jumbo.tootsville.org/Assets/Voices/parrot-squawk.wav",
    Tootsville.scene,
    function ()
    { Tootsville.parrot.squawk = squawk; }); };

Tootsville.tank.start3D = function ()
{ this.prepareFor3D ().then (this.start3DIfReady);
  this.loadUISounds (); };

Tootsville.tank.start3DIfReady = function ()
{ if ( (! ("BABYLON" in window)) || (! ("CANNON" in window)))
  { return this.prepareFor3D ().then (this.start3DifReady); }
  BABYLON.SceneLoader.ShowLoadingScreen = false;

  this.init3DEngine ().then (
      () =>
          { console.log ("3D libraries loaded");
            console.log ("Creating test scene");
            this.createTestScene ();
            console.log ("Starting render loop");
            this.startRenderLoop ();
            console.log ("Setting tank resize event handler");
            window.addEventListener ('resize',
                                     this.engine.resize); }); };
