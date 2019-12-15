/* -*- js2 -*-*/

/**@license
 *
 * ./play/scene/AvatarBuilder.js is part of Tootsville
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

if (!('AvatarBuilder' in Tootsville)) { Tootsville.AvatarBuilder = { }; }


/**
 * Colorize an Avatar and apply their pattern
 */
Tootsville.AvatarBuilder.colorize = function (avatar, model, scene, finish)
{ const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  skinMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.baseColor));
  /* TODO apply pattern texture map to the skin */
  const padMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  padMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.padColor));
  const eyeMaterial = new BABYLON.StandardMaterial ("eye", scene);
  eyeMaterial.diffuseColor = new BABYLON.Color3.FromHexString ('#000000');
  const meshes = model.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes[i];
    mesh.material = (
        mesh.name.indexOf ('Eye') >= 0  ? eyeMaterial :
            mesh.name.indexOf ('Skin') >= 0 ? skinMaterial :
            mesh.name.indexOf ('Pad') >= 0 ? padMaterial :
            eyeMaterial ); }
  // console.debug ("Colorized " + meshes.length + " meshes for avatar "+ avatar.avatar + ' ' + avatar.userName); 
  if (finish) { finish (model); } };

/**
 * Adds a nametag to an avatar. (Only in the main scene, for now.)
 */
Tootsville.AvatarBuilder.addNameTag = function (avatar, model, scene)
{ if (scene !== Tootsville.Tank.scene) { return; }
  var nameTag = document.createElement ('DIV');
  nameTag.innerHTML = avatar.userName; /* Note this may start with ◆ */
  nameTag.className = 'name-tag';
  document.getElementById('hud').append (nameTag);
  scene.avatars [avatar.name].nameTag = nameTag;
  Tootsville.UI.HUD.refreshNameTagAttachment (model, nameTag); };


/**
 * Enable the object to cast shadows in the scene
 */
Tootsville.AvatarBuilder.enableShadows = function (object, scene)
{ if (Tootsville.Tank.shadowGenerator)
  { /* Tootsville.Tank.shadowGenerator.addShadowCaster (object); */
      Tootsville.Tank.shadowGenerator.getShadowMap ().renderList.push (
          Tootsville.Tank.getLargestChildMesh (object)); }; };

/**
 * Add the avatar to the global list of avatars in the scene
 */
Tootsville.AvatarBuilder.rememberAvatar = function (avatar, object, scene)
{ if (!('avatars' in scene)) { scene.avatars = {}; }
  scene.avatars [avatar.name] = Object.assign ((scene.avatars [avatar.name] || {}), avatar);
  scene.avatars [avatar.name].model = object; };

/**
 * Create a physics impostor for the object
 */
Tootsville.AvatarBuilder.enablePhysics = function (avatar, object, scene)
{ let largestChild = Tootsville.Tank.getLargestChildMesh (object);
  largestChild.physicsImpostor =
  new BABYLON.PhysicsImpostor (largestChild,
                               BABYLON.PhysicsImpostor.BoxImpostor,
                               { mass: 6000, restitution: .05 },
                               scene);
  const otherMeshes = object.getChildMeshes().filter (mesh => mesh !== largestChild);
  for (let i = 0; i < otherMeshes.length; ++i)
  { otherMeshes [i].physicsImpostor =
    new BABYLON.PhysicsImpostor (otherMeshes [i],
                                 BABYLON.PhysicsImpostor.NoImpostor,
                                 { mass: 10, restitution: 0 },
                                 scene); }
  object.physicsImpostor =
  new BABYLON.PhysicsImpostor (object,
                               BABYLON.PhysicsImpostor.NoImpostor,
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
  try { Tootsville.AvatarBuilder.addNameTag (avatar, model, scene); } catch (e) { console.error (e); }
  // try { Tootsville.AvatarBuilder.enablePhysics (avatar, model, scene); } catch (e) { console.error (e); }
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
  { const modelRoot = new BABYLON.Mesh ("avatar/" + avatar.name, scene);
    modelRoot.position = BABYLON.Vector3.Zero (); /* TODO */
    let i;
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { modelRoot.addChild (task.loadedMeshes [i]);
      task.loadedMeshes [i].renderOutline = true;
      task.loadedMeshes [i].outlineColor = BABYLON.Color3.Black (); }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { modelRoot.addChild (task.loadedParticleSystems [i]); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { modelRoot.addChild (task.loadedSkeletons [i]); }
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

