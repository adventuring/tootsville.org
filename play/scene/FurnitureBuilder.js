/* -*- js2 -*-*/

/**@license
 *
 * ./play/scene/sky-builder.js is part of Tootsville
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

if (!('FurnitureBuilder' in Tootsville)) { Tootsville.FurnitureBuilder = { itemTemplates: {} }; }
if (!('itemTemplates' in Tootsville.FurnitureBuilder))
{ Tootsville.FurnitureBuilder.itemTemplates = {}; }

Tootsville.FurnitureBuilder.colorize = function (item, model, scene, finish)
{ const baseMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  baseMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (item.baseColor));
  const altMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  altMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (item.altColor));
  const meshes = model.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes [i];
    if (mesh.name.indexOf ('Base') >= 0)
    { mesh.material = baseMaterial; }
    else if (mesh.name.indexOf ('Alt') >= 0)
    { mesh.material = altMaterial; }
    else if (mesh.name.indexOf ('LightSource') >= 0)
    { mesh.material = mesh.material.clone ();
      if (! mesh.material.emissiveColor)
      { mesh.material.emissiveColor = mesh.material.diffuseColor; }
      const light = new BABYLON.PointLight (
          item.template.name + " " + item.uuid + ' Light', mesh.position.clone (), scene);
      mesh.addChild (light); }
  if (finish) { finish (model); } };

Tootsville.FurnitureBuilder.enablePhysics = function (avatar, object, scene)
{ let skinMesh = object.getChildMeshes ().filter( mesh => mesh.name.indexOf ('Base') >= 0 )[0];
  if (!(skinMesh))
  { skinMesh = object.getChildMeshes ()[ 0 ];
    return; }
  skinMesh.physicsImpostor =
  new BABYLON.PhysicsImpostor (skinMesh,
                               BABYLON.PhysicsImpostor.BoxImpostor,
                               { mass: 6000, restitution: .05 },
                               scene);
  const otherMeshes = object.getChildMeshes().filter (mesh => mesh !== skinMesh);
  for (let i = 0; i < otherMeshes.length; ++i)
  { otherMeshes [i].physicsImpostor =
    new BABYLON.PhysicsImpostor (otherMeshes [i],
                                 BABYLON.PhysicsImpostor.NoImpostor,
                                 { mass: item.template.weight / object.getChildMeshes ().length, restitution: 0 },
                                 scene); }
  object.physicsImpostor =
    new BABYLON.PhysicsImpostor (object,
                                 BABYLON.PhysicsImpostor.NoImpostor,
                                 { mass: item.template.weight, restitution: 0 },
                                 scene); }; };

Tootsville.FurnitureBuilder.build2 = function (item, model, scene, finish)
{ console.debug ("Building " + item.template.name + " " + item.uuid);
  if (item.avatarScaling)
  { model.scaling = new BABYLON.Vector3 (item.avatarScaling, item.avatarScaling, item.avatarScaling);  }
  try {Tootsville.FurnitureBuilder.rememberAvatar (item, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.enablePhysics (item, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.colorize (item, model, scene, finish); } catch (e) { console.error (e); } };

Tootsville.FurnitureBuilder.loadItemTemplate = function (item, scene, finish)
{ let assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  console.debug ("fixing to load item template " + item.template.name);
  let loadTask = assetsManager.addMeshTask ("loading " + item.template.name, null,
                                            "https://jumbo.tootsville.org/Assets/Models/5/",
                                            item.template.avatar + ".babylon");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.Mesh ("item/" + item.template.name + '#' + item.uuid, scene);
    let i;
    modelRoot.position = new BABYLON.Vector3 (item.position.x, item.position.y, item.position.z);
    modelRoot.rotate (BABYLON.Axis.Y, modelRoot.facing);
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { modelRoot.addChild (task.loadedMeshes [i]);
      task.loadedMeshes [i].renderOutline = true;
      task.loadedMeshes [i].outlineColor = BABYLON.Color3.Black ();  }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { modelRoot.addChild (task.loadedParticleSystems [i]); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { modelRoot.addChild (task.loadedSkeletons [i]); }
    console.debug ("Loaded item template avatar " + item.template.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    Tootsville.FurnitureBuilder.build2 (avatar, modelRoot, scene, finish); };
  assetsManager.load (); };

/**
 * Given an item's description, load, colorize, and position its model.
 *
 * Furniture items are described differently than avatars; the canonical
 * description is at `ITEM-INFO', qv., and `ITEM-TEMPLATE-INFO'.
 *
 */
Tootsville.FurnitureBuilder.build = function (item, scene, finish)
{ Tootsville.FurnitureBuilder.loadItemTemplate (item, scene, finish); };

