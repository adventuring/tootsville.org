/* -*- js2 -*-*/

/**@license
 *
 * ./play/scene/avatar-builder.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { AvatarBuilder: {} }; }
if (!('AvatarBuilder' in Tootsville)) { Tootsville.AvatarBuilder = { }; }

/**
 *
 */
Tootsville.AvatarBuilder.colorize = function (avatar, node, scene, finish)
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
            eyeMaterial ); }
  // console.debug ("Colorized " + meshes.length + " meshes for avatar "+ avatar.avatar + ' ' + avatar.userName); 
  if (finish) { finish (node); } };

/**
 * Adds a nametag to an avatar. (Only in the main scene, for now.)
 */
Tootsville.AvatarBuilder.addNameLabel = function (avatar, model, scene)
{ if (scene !== Tootsville.Tank.scene) { return; } /* XXX Labels in other contexts */
  var label = document.createElement ('DIV');
  label.innerHTML = avatar.userName; /* Note this may start with ◆ */
  label.className = 'name-tag';
  document.getElementById('hud').append (label);
  scene.avatars [avatar.name].label = label;
  Tootsville.Tank.updateAttachmentsForAvatar (avatar); };


/**
 * Enable the object to cast shadows in the scene
 */
Tootsville.AvatarBuilder.enableShadows = function (object, scene)
{
  // if (Tootsville.Tank.shadowGenerator)
    // { Tootsville.Tank.shadowGenerator.addShadowCaster (object);
};

/**
 * Add the avatar to the global list of avatars in the scene
 */
Tootsville.AvatarBuilder.rememberAvatar = function (avatar, object, scene)
{ if (!('avatars' in scene)) { scene.avatars = {}; }
  scene.avatars [avatar.name] = Object.assign ({}, avatar);
  scene.avatars [avatar.name].model = object; };

/**
 * Create a physics impostor for the object
 */
Tootsville.AvatarBuilder.enablePhysics = function (avatar, object, scene)
{ let skinMesh = object.getChildMeshes().filter( mesh => mesh.name.indexOf ('Skin') >= 0 )[0];
  if (!(skinMesh))
  { console.error ("Avatar has no skin layer?");
    return; }
  object.physicsImpostor = new BABYLON.PhysicsImpostor (skinMesh,
                                                        BABYLON.PhysicsImpostor.SphereImpostor,
                                                        { mass: 6000, restitution: .05 },
                                                        scene); };


/**
 * Actually build the (cloned meshes) avatar.
 *
 * Don't call this directly, call `Tootsville.AvatarBuilder.build'.
 */
Tootsville.AvatarBuilder.build2 = function (avatar, model, scene, finish)
{ console.debug ("Building " + avatar.avatar + " " + avatar.userName);
  // TODO set scaling
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.addNameLabel (avatar, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.enablePhysics (avatar, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.colorize (avatar, model, scene, finish); } catch (e) { console.error (e); } };

/**
 * Load the base avatar model from Jumbo.
 */
Tootsville.AvatarBuilder.loadAvatarBase = function (avatar, scene, finish)
{ var assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  var loadTask = assetsManager.addMeshTask ("loading " + avatar.avatar, null,
                                            "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            avatar.avatar + ".babylon");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.TransformNode ("avatar/" + avatar.name, scene, true);
    modelRoot.position = BABYLON.Vector3.Zero (); /* TODO */
    var i;
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { task.loadedMeshes [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { task.loadedParticleSystems [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { task.loadedSkeletons [i].setParent (modelRoot); }
    console.debug ("Loaded base avatar " + avatar.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    Tootsville.AvatarBuilder.build2 (avatar, modelRoot, scene, finish); };
  assetsManager.load (); };

/**
 *
 */
Tootsville.AvatarBuilder.update = function (avatar, model, scene, finish)
{ try { Tootsville.AvatarBuilder.colorize (avatar, model, scene, finish); } catch (e) { console.error (e); }
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  finish (model); };

/**
 * Build an avatar based upon the description passed in.
 *
 * The  structure  of   the  avatar  description  is   as  explained  at
 * `TOOT-INFO'.
 *
 * A duplicate of an existing avatar will not be created, but it may be updated.
 */
Tootsville.AvatarBuilder.build = function (avatar, scene, finish)
{ if (scene.avatars && scene.avatars [avatar.name] && scene.avatars [avatar.name].model)
  { Tootsville.AvatarBuilder.update (avatar, scene.avatars [avatar.name].model, scene, finish); }
  else
  { Tootsville.AvatarBuilder.loadAvatarBase (avatar, scene, finish); } };

