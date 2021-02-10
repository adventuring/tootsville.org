/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/AvatarBuilder.js is part of Tootsville
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

if (!('AvatarBuilder' in Tootsville)) { Tootsville.AvatarBuilder = { }; }



/**
 *
 */
Tootsville.AvatarBuilder.getPathForPattern = function (pattern) {
    /* FIXME:  spots  are  irregular,   as  are  patches  and  sparkles.
     * They need to randomly choose from among several paths. */
    return new Path2D (Tootsville.AvatarBuilder.patterns [ pattern.toLowerCase () ] ||
                       Tootsville.AvatarBuilder.patterns.spots ); };

/**
* Pick a random color that does not match the avatar's skin
*/
Tootsville.AvatarBuilder.rainbowColor = function (baseColor)
{ const patternColors = [ "Black", "Cyan", "Indigo", "Orange", "Pink", "Turquoise", "Violet", "White", "Yellow" ];
  let color = patternColors [ Math.floor (Math.random () * patternColors.length) ];
  if (color === baseColor)
  { return Tootsville.AvatarBuilder.rainbowColor (baseColor); }
  else
  { return color; } };

Tootsville.AvatarBuilder.drawPatternOnCanvas = function (avatar, canvas)
{ canvas.fillStyle = Tootsville.UI.interpretTootColor (avatar.baseColor);;
  canvas.fillRect (0, 0, 1024, 1024);
  let i = 0;
  for (let x = 0; x < 1023; x += 192)
  { for (let y = 0; y < 1023; y += 192)
    { ++i;
      canvas.setTransform (1, 0, 0, 1, x + x % 171, y + y % 53);
      // XXX rotate only  once all the patterns have  been recentered on
      // the origin properly: // canvas.rotate ( (x + y) / 256 );
      if ('rainbow' === avatar.patternColor.toLowerCase ())
      { canvas.fillStyle = Tootsville.AvatarBuilder.rainbowColor (avatar.baseColor); }
      else
      { canvas.fillStyle = Tootsville.UI.interpretTootColor (avatar.patternColor); }
      let path = Tootsville.AvatarBuilder.getPathForPattern (avatar.pattern);
      if (Array.isArray (path))
      { path = path [ i % path.length ]; }
      canvas.fill ( path ); } } };

/**
 * Colorize an Avatar and apply their pattern
 */
Tootsville.AvatarBuilder.colorize = function (avatar, model, scene, finish)
{ if (! model) { return; }
  const patternTexture = new BABYLON.DynamicTexture (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     1024 /* resolution */,
                                                     scene);
  const canvas = patternTexture.getContext ();
  Tootsville.AvatarBuilder.drawPatternOnCanvas (avatar, canvas);
  patternTexture.update ();
  const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  skinMaterial.diffuseTexture = patternTexture;
  
  const padMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  padMaterial.diffuseColor = new BABYLON.Color3.FromHexString (Tootsville.UI.interpretTootColor (avatar.padColor));
  const eyeMaterial = new BABYLON.StandardMaterial ("eye", scene);
  eyeMaterial.diffuseColor = new BABYLON.Color3.FromHexString ('#000000');
  const meshes = model.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes[i];
    if (mesh.name.indexOf ('Eye') >= 0)
    { mesh.material = eyeMaterial; }
    else if (mesh.name.indexOf ('Skin') >= 0)
    { mesh.material = skinMaterial; }
    else if (mesh.name.indexOf ('Pad') >= 0)
    { mesh.material = padMaterial; } }
  if (finish) { finish (model); } };

/**
 * Adds a nametag to an avatar. (Only in the main scene, for now.)
 */
Tootsville.AvatarBuilder.addNameTag = function (avatar, model, scene)
{ if (scene !== Tootsville.Tank.scene) { return; }
  var nameTag = document.createElement ('DIV');
  nameTag.innerHTML = avatar.userName; /* Note this may start with ◆ */
  nameTag.tootName = avatar.name;
  nameTag.className = 'name-tag';
  nameTag.addEventListener ('click', Tootsville.UI.HUD.nameTagClicked);
  document.getElementById('hud').append (nameTag);
  Tootsville.Tank.findAvatar (avatar.name).nameTag = nameTag;
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
{ if (Tootsville.Tank.avatars [ avatar.name ].model)
  { console.error ("Already remembered a model for avatar " + avatar.name);
    if (Tootsville.Tank.avatars [ avatar.name ].model !== object)
    { object.dispose (); }
    return; }
  Tootsville.Tank.avatars [ avatar.name ] = Object.assign ((Tootsville.Tank.avatars [avatar.name] || {}), avatar);
  console.log ("Remembering model ", object, " for avatar ", avatar.name);
  Tootsville.Tank.avatars [avatar.name].model = object; };

/**
 * Actually build the avatar.
 *
 * Don't call this directly, call `Tootsville.AvatarBuilder.build'.
 */
Tootsville.AvatarBuilder.postBuild = function (avatar, model, scene)
{ console.debug ("Building " + avatar.avatar + " " + avatar.userName);
  let existing = Tootsville.Tank.findAvatar (avatar.name);
  if (existing.model)
  { console.warn ("Canceling AvatarBuilder.postBuild for " + avatar.userName + " because found one existing");
    return; }
  // TODO set scaling
  try { Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  let a = Tootsville.Tank.avatars [avatar.name];
  if (!a) { return; }
  try { Tootsville.AvatarBuilder.addNameTag (a, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.Game.GravitySystem.register (a, model); } catch (e) { console.error (e); }
  if (avatar.npc)
  { try { Tootsville.Game.NPCSystem.register (a); } catch (e) { console.error (e); } } };

/**
 * After loading the avatar in ``task'', set it up and colorize it.
 */
Tootsville.AvatarBuilder.afterLoading = function (task, avatar, scene, finish)
  { console.log ("Success with " + task.name);
    const modelRoot = new BABYLON.Mesh ("avatar/" + avatar.name, scene);
    modelRoot.position = BABYLON.Vector3.Zero (); /* TODO */
    if (task.loadedMeshes.length > 0)
        for (let i = 0; i < task.loadedMeshes.length; ++i) {
            modelRoot.addChild (task.loadedMeshes [i]);
            task.loadedMeshes [i].renderOutline = true;
            task.loadedMeshes [i].outlineColor = BABYLON.Color3.Black (); }
    // if (task.loadedParticleSystems.length > 0)
    // for (let i = 0; i < task.loadedParticleSystems.length; ++i)
    // { modelRoot.addChild (task.loadedParticleSystems [i]); }
    // if (task.loadedSkeletons.length > 0)
    //     for (let i = 0; i < task.loadedSkeletons.length; ++i)
    // { modelRoot.addChild (task.loadedSkeletons [i]); }
    console.debug ("Loaded base avatar " + avatar.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    if (scene === Tootsville.Tank.scene)
    { Tootsville.AvatarBuilder.postBuild (avatar, modelRoot, scene); }
    modelRoot.rotationQuaternion = undefined;
    modelRoot.rotation = BABYLON.Vector3.Zero ();
    Tootsville.AvatarBuilder.colorize (avatar, modelRoot, scene, finish); };

/**
 *
 */
Tootsville.AvatarBuilder.assetProgress = function (left, total, last) {
    console.info (`Loading ${left} more assets of ${total}, last ${last}`);
};

/**
 * Load the base avatar model from Jumbo.
 */
Tootsville.AvatarBuilder.loadAvatarBase = function (avatar, scene, finish)
{ var assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene);
    assetsManager.useDefaultLoadingScreen = false;
    assetsManager.onProgress = (left, total, last) => { Tootsville.AvatarBuilder.assetProgress (left, total, last); }; }
  if (scene === Tootsville.Tank.scene)
  { if (Tootsville.Tank.avatars [ avatar.name ] && Tootsville.Tank.avatars [ avatar.name ].model)
    { console.warn ("Almost re-loaded avatar model for " + avatar.name);
      return; /* XXX finish? */ } }
  BABYLON.GLTFFileLoader.IncrementalLoading = false;
  var loadTask = assetsManager.addMeshTask ("loading " + avatar.avatar, null,
                                            "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            avatar.avatar + ".glb");
  loadTask.onError = function (task, message, e)
  { console.error ("Error " + task.name + ": " + message + ": " +
                   e.message + " in " + e.filename + ":"  + e.lineNumber, task, e); };
  loadTask.onSuccess = task => { Tootsville.AvatarBuilder.afterLoading (task, avatar, scene, finish); };
  assetsManager.load (); };

/**
 *
 */
Tootsville.AvatarBuilder.update = function (avatar, model, scene, finish)
{ console.log ("Update avatar " + avatar.name + " as ", avatar, " model ", model);
  try { Tootsville.AvatarBuilder.colorize (avatar, model, scene, finish); } catch (e) { console.error (e); }
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  if (finish) { finish (model); } };

/**
 * Build an avatar based upon the description passed in.
 *
 * The  structure  of   the  avatar  description  is   as  explained  at
 * `TOOT-INFO'.
 *
 * A duplicate of an existing avatar will not be created, but it may be updated.
 */
Tootsville.AvatarBuilder.build = function (avatar, scene=null, finish=null)
{ if (!scene) { scene = Tootsville.Tank.scene; }
  if (scene === Tootsville.Tank.scene)
  { if (Tootsville.Tank.avatars &&
        Tootsville.Tank.avatars [avatar.name] &&
        Tootsville.Tank.avatars [avatar.name].model)
    { Tootsville.AvatarBuilder.update (avatar, Tootsville.Tank.avatars [avatar.name].model, scene, finish);
      return; }
    let existing = Tootsville.Tank.findAvatar (avatar.name);
    if (existing && existing.model)
    { console.warn ("Found forgotten avatar model for " + avatar.name);
      Tootsville.Tank.avatars [ avatar.name ].model = existing.model;
      Tootsville.AvatarBuilder.update (avatar, Tootsville.Tank.avatars [avatar.name].model, scene, finish);
      return; } }
  Tootsville.AvatarBuilder.loadAvatarBase (avatar, scene, finish); };


/*----------------------------------------*/

/**
 * Make a colorizer function for a material for ``avatar''.
 */
Tootsville.AvatarBuilder.makeAvatarColorizeMaterial = function (avatar) {
    return function (material) {
        if (!(material.name)) return;
        if ('base' == material.name.toLower ())
            Tootsville.AvatarBuilder.assignPatternToMaterial (material, avatar);
        else if ('pad' == material.name.toLower ())
            Tootsville.ModelLoader.setMaterialColor (material, avatar.padColor); }; };

/**
 * Make a colorizer function for ``avatar'' for `Tootsville.ModelLoader.loadAndColorize'
 */
Tootsville.AvatarBuilder.makeAvatarColorizer = function (avatar) {
    let colorizeMaterial = Tootsville.AvatarBuilder.makeAvatarColorizeMaterial (avatar);
    return function (node) {
        if (!(node.materials)) return;
        for (let i = 0; i < node.materials.length; ++i)
            colorizeMaterial (node.materials [i]); }; };

/**
 *
 */
Tootsville.AvatarBuilder.buildNew = function (avatar, scene=null, finish=null)
{ if (!scene) { scene = Tootsville.Tank.scene; }
  let colorizer = Tootsville.AvatarBuilder.makeAvatarColorizer (avatar);
  Tootsville.ModelLoader.loadAndColorize ('Avatars', avatar.avatar, colorizer,
                                          scene);
  /* TODO call ModelLoader with an avatar colorizer function */  };
