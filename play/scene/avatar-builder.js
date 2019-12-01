/* -*- js2 -*-*/

/*@license
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
if (!('AvatarBuilder' in Tootsville)) { Tootsville.AvatarBuilder = { baseAvatars: {} }; }
if (!('baseAvatars' in Tootsville.AvatarBuilder))
{ Tootsville.AvatarBuilder.baseAvatars = {}; }

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
            eyeMaterial );
    console.debug ("Colorized mesh named " + mesh.name + " with material ", mesh.material.name); }
  if (finish) { finish (node); } };

/**
 * Actually build the (cloned meshes) avatar. Don't call this directly, call `Tootsville.AvatarBuilder.build'.
 */
Tootsville.AvatarBuilder.build2 = function (avatar, root, scene, finish)
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
  Tootsville.AvatarBuilder.colorize (avatar, root, scene, finish); };

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
  { const modelRoot = new BABYLON.TransformNode ("baseAvatar/" + avatar.avatar, scene, true);
    modelRoot.position = new BABYLON.Vector3 (0, -10, 0);
    var i;
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { task.loadedMeshes [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { task.loadedParticleSystems [i].setParent (modelRoot); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { task.loadedSkeletons [i].setParent (modelRoot); }
    Tootsville.AvatarBuilder.baseAvatars [avatar.avatar] = modelRoot;
    console.debug ("Loaded base avatar " + avatar.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    Tootsville.AvatarBuilder.build2 (avatar, modelRoot, scene, finish); };
  assetsManager.load (); };


/**
 * Build an avatar based upon the description passed in.
 *
 * The  structure  of   the  avatar  description  is   as  explained  at
 * `TOOT-INFO'.
 */
Tootsville.AvatarBuilder.build = function (avatar, scene, finish)
{ const root = Tootsville.AvatarBuilder.baseAvatars [ avatar.avatar ];
  if (root)
  { Tootsville.AvatarBuilder.build2 (avatar, root, scene, finish); }
  else
  { Tootsville.AvatarBuilder.loadAvatarBase (avatar, scene, finish); } };

