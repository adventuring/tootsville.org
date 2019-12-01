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

if (!('Tootsville' in window)) { Tootsville = { Avatars: { UltraTootBuilder: {}, Viewer: {}  } }; }
if (!('Avatars' in Tootsville)) { Tootsville.Avatars = { UltraTootBuilder: {}, Viewer: {}  }; }
if (!('UltraTootBuilder' in Tootsville.Avatars)) { Tootsville.Avatars.UltraTootBuilder = {}; }
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
 * The UltraToot model object is re-used for each Toot object.
 */
Tootsville.Avatars.UltraTootBuilder.model = null;

/**
 * Create a Toot-like proxy object until the UltraToot model is available.
 */
Tootsville.Avatars.UltraTootBuilder.addProxyToot = function (modelRoot, scene)
{ // XXX: private
    const proxyHead = BABYLON.MeshBuilder.CreateSphere ('UltraToot.proxy.head',
                                                        { segments: 12, diameter: 1 },
                                                        scene);
    const proxyBody = BABYLON.MeshBuilder.CreateSphere ('UltraToot.proxy.body',
                                                        { segments: 12, diameter: 1 },
                                                        scene);
    proxyHead.position.z = 1.5;
    proxyHead.material = new BABYLON.StandardMaterial ('head', scene);
    proxyHead.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('violet'));
    proxyBody.position.z = .5;
    proxyBody.material = new BABYLON.StandardMaterial ('body', scene);
    proxyBody.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('violet'));
    proxyBody.setParent (modelRoot); };

/**
 * Buld the UltraToot model from the meshes loaded from the model file.
 */
Tootsville.Avatars.UltraTootBuilder.addMeshesToModelRoot = function (meshes, modelRoot, scene)
{ try
  { if (meshes.length == 0)
    { Tootsville.warn ("Empty container returned for UltraToot");
      Tootsville.Avatars.UltraTootBuilder.addProxyToot (modelRoot); } else
    { /* Expecting 13 meshes for current UltraToot */
        if (10 != meshes.length)
        { Tootsville.warn ("Loading Ultratoot got " + meshes.length + " meshes, was expecting 10"); }
        for (let i = 0; i < meshes.length; ++i)
        { meshes[i].setParent (modelRoot); } } }
  catch (e)
  { Tootsville.warn ("Error adding meshes to model root for  UltraToot", e);
    Tootsville.Avatars.UltraTootBuilder.addProxyToot (modelRoot); }
  modelRoot.position.y = -Infinity;
  scene.addTransformNode (modelRoot); // TODO: is this necessary?
  Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
  return Tootsville.Avatars.UltraTootBuilder.model; };

/**
 * Import the UltraToot model object into the scene.
 */
Tootsville.Avatars.UltraTootBuilder.importUltraToot = function (finish, meshes, particles, skeletons, scene)
{ // XXX: private
    Tootsville.trace ("Got UltraToot meshes", meshes);
    const modelRoot = new BABYLON.TransformNode ('UltraToot', scene, true);
    console.debug ("Adding meshes to model root");
    Tootsville.Avatars.UltraTootBuilder.addMeshesToModelRoot (meshes, modelRoot, scene);
    /* XXX: do something with particles and skeletons? */
    console.debug ("Moving model to Y=-Infinity");
    modelRoot.position.y = -Infinity;
    console.debug ("Adding model root to scene");
    scene.addTransformNode (modelRoot); // TODO: is this necessary?
    console.debug ("Saving model as base for other Toots");
    Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
    if (finish) { console.debug ("Calling importUltraToot finish function");
                  finish (Tootsville.Avatars.UltraTootBuilder.model); }
    console.debug ("Returning model");
    return Tootsville.Avatars.UltraTootBuilder.model; };

/**
 * Load the UltraToot base model from the resource on Jumbo.
 */
Tootsville.Avatars.UltraTootBuilder.getBaseModel = function (scene)
{ if (!scene)
  { scene = Tootsville.tank.scene; }
  return new Promise (
    (finish) =>
        { if (Tootsville.Avatars.UltraTootBuilder.model)
          { finish (Tootsville.Avatars.UltraTootBuilder.model);
            return; }
          else
          { BABYLON.SceneLoader.ImportMesh ("", /* import all meshes */
                                            "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            "UltraToot.babylon",
                                            scene,
                                            (meshes, particles, skeletons) =>
                                            { Tootsville.Avatars.UltraTootBuilder.importUltraToot (
                                                finish, meshes, particles, skeletons, scene); },
                                            null, /* onprogress */
                                            (scene, errorMessage) =>
                                            { console.log (errorMessage); });
            return; } }); };

/**
 * Set the colors of the Toot avatar.
 *
 * This colorizes the  Toot's skin and pad colors, as  well as the black
 * eyes and eyebrows.
 *
 * @code{node} is the 3D node containing the Toot meshes.
 *
 * @code{avatar} is an avatar object in standard form, as per `TOOT-INFO'
 */
Tootsville.Avatars.UltraTootBuilder.setColors = function (node, avatar, scene)
{ const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor,
                                                     scene);
  skinMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.baseColor));
  // TODO apply pattern texture map to the skin
  const padMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  padMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.baseColor));
  const eyeMaterial = new BABYLON.StandardMaterial ("eye", scene);
  eyeMaterial.diffuseColor = new BABYLON.Color3.FromHexString ('#000000');
  const meshes = node.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes[i];
    mesh.material = (
        mesh.name == 'Left Eyebrow' ? eyeMaterial :
            mesh.name == 'Right Eyebrow' ? eyeMaterial :
            mesh.name == 'Left Eye' ? eyeMaterial :
            mesh.name == 'Right Eye' ? eyeMaterial :
            mesh.name == 'Skin' ? skinMaterial :
            padMaterial ); } };

/**
 * Add clothes onto the avatar mesh
 */
Tootsville.Avatars.UltraTootBuilder.addClothes = function (node, avatar)
{ // TODO
};

/**
 * Enable physics imposot for the Toot
 */
Tootsville.Avatars.UltraTootBuilder.enablePhysics = function (node, avatar, scene)
{ node.physicsImpostor = new BABYLON.PhysicsImpostor (node,
                                                      BABYLON.PhysicsImpostor.SphereImpostor,
                                                      { mass: 1, restitution: 0.9 },
                                                      scene); };

/**
 * Create an UltraToot which is described by @code{avatar}.
 *
 * See `TOOT-INFO' for a description of the format of @code{avatar}.
 */
Tootsville.Avatars.UltraTootBuilder.makeToot = function (avatar, scene)
{ if (! (scene) )
  { scene = Tootsville.tank.scene; }
  return new Promise ( (finish) =>
                       { if (avatar.avatar == 'UltraToot')
                         { Tootsville.Avatars.UltraTootBuilder.getBaseModel (scene).then
                           ( (model) =>
                             { var toot = model.clone ();
                               console.debug ("Constructing avatar for " + avatar.name);
                               toot.name = 'avatar/' + avatar.name;
                               try { Tootsville.Avatars.UltraTootBuilder.enablePhysics (toot, avatar, scene); }
                               catch (e) { console.warn ("Enabling physics: ", e); }
                               Tootsville.Avatars.UltraTootBuilder.setColors (toot, avatar, scene);
                               Tootsville.Avatars.UltraTootBuilder.addClothes (toot, avatar, scene);
                               console.debug ("Finished constructing avatar for " + avatar.name);
                               finish (toot);
                               return; }); }
                         else
                         { Tootsville.warn ("Avatar is not UltraToot: " + avatar.toSource ());
                           /* TODO  try to load non-UltraToot avatar */
                           var proxy = BABYLON.MeshBuilder.CreateSphere ('proxy for ' + avatar.name,
                                                                         { segments: 8, diameter: .25},
                                                                         scene);
                           proxy.material = new BABYLON.StandardMaterial ('red', scene);
                           proxy.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('red'));
                           finish (proxy);
                           return; } }); };
/**
 * Move a Toot by @code{δv}
 */
Tootsville.Avatars.UltraTootBuilder.moveToot = function (toot, δv)
{ //         var forwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
    // forwards.negate ();
    // character.moveWithCollisions (forwards);
    // // or
    // var backwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, -gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
    // character.moveWithCollisions (backwards);
};

/**
 *
 */
Tootsville.Avatars.Viewer.createScene = function (canvas)
{ canvas.engine = new BABYLON.Engine (canvas, true);
  canvas.scene = new BABYLON.Scene (canvas.engine); };

/**
 *
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
{ Tootsville.Avatars.UltraTootBuilder.makeToot (
    toot, canvas.scene
          ).then (
              ultraToot =>
                  { console.info ('loaded a Toot into Viewer', canvas);
                    canvas.camera.lockedTarget = ultraToot; }); };

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
