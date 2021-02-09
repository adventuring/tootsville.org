/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/FurnitureBuilder.js is part of Tootsville
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

if (!('FurnitureBuilder' in Tootsville)) { Tootsville.FurnitureBuilder = { itemTemplates: {} }; }

if (!('itemTemplates' in Tootsville.FurnitureBuilder)) { Tootsville.FurnitureBuilder.itemTemplates = {}; }

/**
 * Colorize a furniture item
 */
Tootsville.FurnitureBuilder.colorize = function (item, model, scene, finish)
{ const baseMaterial = new BABYLON.StandardMaterial (item.baseColor, scene);
  baseMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (item.baseColor));
  const altMaterial = new BABYLON.StandardMaterial (item.padColor, scene);
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
    if (finish) { finish (model); } } };

/**
 * Enable the object to cast shadows in the scene
 */
Tootsville.FurnitureBuilder.enableShadows = function (object, scene)
{ if (Tootsville.Tank.shadowGenerator)
  { /* Tootsville.Tank.shadowGenerator.addShadowCaster (object); */
      Tootsville.Tank.shadowGenerator.getShadowMap ().renderList.push (
          Tootsville.Tank.getLargestChildMesh (object)); }; };

/**
 * Stash a reference to the item it the scene.items object.
 */
Tootsville.FurnitureBuilder.rememberItem = function (item, model, scene)
{ if (!('items' in scene)) { scene.items = {}; }
  scene.items [item.uuid] = Object.assign ({}, item);
  scene.items [item.uuid].model = model; };

/**
 * Finish construction  of the object  after it  has been loaded  by the
 * asset manager.
 */
Tootsville.FurnitureBuilder.build2 = function (item, model, scene, finish)
{ console.debug ("Building furniture " + item.template.name + " " + item.uuid);
  model.scaling = new BABYLON.Vector3 (parseFloat(item.scale.x),
                                       parseFloat(item.scale.y),
                                       parseFloat(item.scale.z));
  model.checkCollisions = true;
  try { Tootsville.FurnitureBuilder.rememberItem (item, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.colorize (item, model, scene, finish); } catch (e) { console.error (e); }
  if (item.growth)
  { try { Tootsville.Game.GrowthSystem.register (item); } catch (e) { console.error (e); } }
};

/**
 * Load an item template avatar from the assets server.
 */
Tootsville.FurnitureBuilder.loadItemTemplate = function (item, scene, finish)
{ let assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  console.debug ("fixing to load item template " + item.template.name);
  let loadTask = assetsManager.addMeshTask (
      "loading " + item.template.name, null,
      "https://jumbo.tootsville.org/Assets/Models/5/" + item.template.avatar + "/",
      item.template.avatar + ".glb");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.Mesh ("item/" + item.template.name + '#' + item.uuid, scene);
    let i;
    modelRoot.position = new BABYLON.Vector3 (parseFloat(item.position.x),
                                              parseFloat(item.position.y),
                                              parseFloat(item.position.z));
    modelRoot.rotate (BABYLON.Axis.Y, parseFloat(item.facing));
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
    Tootsville.FurnitureBuilder.build2 (item, modelRoot, scene, finish); };
  assetsManager.load (); };

/**
 * WRITEME … TODO update furniture …
 */
Tootsville.FurnitureBuilder.update = function (item, model, scene, finish)
{ /* TODO */
    if (finish) { finish (model); } };

/**
 * Given an item's description, load, colorize, and position its model.
 *
 * Furniture items are described differently than avatars; the canonical
 * description is at `ITEM-INFO', qv., and `ITEM-TEMPLATE-INFO'.
 *
 */
Tootsville.FurnitureBuilder.build = function (item, scene, finish)
{ if (!scene) scene = Tootsville.Tank.scene;
  if (!scene) { console.error ("Can't build furniture without a scene", item);
                return; }
  if (scene.items && scene.items [item.uuid] && ('model' in scene.items [item.uuid]))
  { Tootsville.FurnitureBuilder.update (item, scene.items [item.uuid].model, scene, finish); }
  else
  { Tootsville.FurnitureBuilder.loadItemTemplate (item, scene, finish); } };
