/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/AvatarBuilder.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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
Tootsville.AvatarBuilder.patterns = {
    lightning: 'm 165.49363,109.39907 49.02812,-11.852952 3.23262,56.032142 -14.00803,1.07754 12.93049,34.48131 -12.93049,3.7714 5.92648,21.55082 -26.39976,-33.94254 8.08156,-1.07755 -22.62836,-31.78746 17.77942,-4.31016 z',
    flowers: 'm 60.768457,15.872906 a 17.906584,17.906584 0 0 0 -15.49711,8.97991 17.906583,17.906583 0 0 0 -3.0334,17.27884 17.906583,17.906583 0 0 0 0.0641,0.15333 17.906584,17.906584 0 0 0 -2.50572,-0.19673 17.906584,17.906584 0 0 0 -11.293695,4.0266 17.906583,17.906583 0 0 0 -8.651763,21.84227 17.906583,17.906583 0 0 0 22.540388,11.01691 17.906584,17.906584 0 0 0 -3.60726,10.73177 17.906584,17.906584 0 0 0 6.93216,14.138724 17.906583,17.906583 0 0 0 18.31824,3.94855 17.906583,17.906583 0 0 0 11.91295,-16.086914 17.906583,17.906583 0 0 0 20.529265,6.48096 17.906583,17.906583 0 0 0 11.577238,-13.38883 17.906584,17.906584 0 0 0 0.91476,-5.57855 17.906584,17.906584 0 0 0 -10.039238,-16.06221 17.906583,17.906583 0 0 0 4.813928,-3.5738 17.906584,17.906584 0 0 0 6.97318,-14.15305 17.906584,17.906584 0 0 0 -17.906458,-17.90647 17.906584,17.906584 0 0 0 -12.895805,5.51801 17.906583,17.906583 0 0 0 -1.25485,1.05813 17.906584,17.906584 0 0 0 0.016,-0.32098 17.906584,17.906584 0 0 0 -17.90686,-17.90647 z m 6.9911,31.3395 a 13.947631,13.947631 0 0 1 13.94756,13.94756 13.947631,13.947631 0 0 1 -13.94756,13.94756 13.947631,13.947631 0 0 1 -13.94756,-13.94756 13.947631,13.947631 0 0 1 13.94756,-13.94756 z',
    hearts: 'm 100.25258,66.538316 q 11.47502,0 18.03218,13.283896 2.37414,5.257031 2.4872,8.026865 h 0.16958 q 1.97845,-9.157409 7.17895,-15.205821 6.10494,-6.10494 13.73612,-6.10494 11.81419,0 18.93661,12.662097 1.80888,4.691759 1.80888,8.818245 0,14.131802 -11.75767,26.963482 l -29.90289,35.89478 h -0.33917 L 88.834079,111.92967 q -9.496573,-11.58808 -9.496573,-23.911012 0,-11.927242 11.248916,-18.936617 4.748286,-2.543725 9.666158,-2.543725 z',
    spots: 'm 57.215385,196.89556 c 14.16871,-24.21061 1.717258,-38.50789 17.542087,-57.92546 15.824829,-19.41758 45.663718,-28.29696 69.769138,-23.74938 24.10542,4.54759 76.65345,-29.70492 82.03089,22.08967 5.37742,51.79459 -54.464,54.58715 -71.3929,70.80937 -16.9289,16.22223 -30.27675,66.61784 -59.105076,70.14046 -28.828314,3.52262 -66.252802,-45.7833 -52.566003,-65.1775 13.6868,-19.3942 13.721864,-16.18716 13.721864,-16.18716 z'
};

/**
 *
 */
Tootsville.AvatarBuilder.getPathForPattern = function (pattern) {
    /* FIXME:  spots  are  irregular,   as  are  patches  and  sparkles.
     * They need to randomly choose from among several paths. */
    return new Path2D (Tootsville.AvatarBuilder.patterns [ pattern.toLowerCase () ] ||
                       Tootsville.AvatarBuilder.patterns.spots ); };

/**
 * Colorize an Avatar and apply their pattern
 */
Tootsville.AvatarBuilder.colorize = function (avatar, model, scene, finish)
{ if (! model) { return; }
  const patternTexture = new BABYLON.DynamicTexture (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     256 /* resolution */,
                                                     scene);
  const canvas = patternTexture.getContext ();
  canvas.fillStyle = interpretTootColor (avatar.baseColor);;
  canvas.fillRect (0, 0, 256, 256);
  for (let x = 0; x < 256; x += 64)
  { for (let y = 0; y < 256; y += 64)
    { canvas.setTransform (1, 0, 0, 1, x, y);
      canvas.fillStyle = interpretTootColor (avatar.patternColor); /* FIXME rainbow */
      canvas.fill (Tootsville.AvatarBuilder.getPathForPattern (avatar.pattern)); } }
  patternTexture.update ();
  const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  skinMaterial.diffuseTexture = patternTexture;
  
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
    if (Tootsville.Tank.avatars [ avatar.name ].model != object)
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
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  let a = Tootsville.Tank.avatars [avatar.name];
  if (!a) { return; }
  try { Tootsville.AvatarBuilder.addNameTag (a, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.Game.GravitySystem.register (a, model); } catch (e) { console.error (e); }
  if (avatar.npc)
  { try { Tootsville.Game.NPCSystem.register (a); } catch (e) { console.error (e); } } };

/**
 * Load the base avatar model from Jumbo.
 */
Tootsville.AvatarBuilder.loadAvatarBase = function (avatar, scene, finish)
{ var assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  if (scene == Tootsville.Tank.scene)
  { if (Tootsville.Tank.avatars [ avatar.name ] && Tootsville.Tank.avatars [ avatar.name ].model)
    { console.warn ("Almost re-loaded avatar model for " + avatar.name);
      return; /* XXX finish? */ } }
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
    if (scene == Tootsville.Tank.scene)
    { Tootsville.AvatarBuilder.postBuild (avatar, modelRoot, scene); }
    Tootsville.AvatarBuilder.colorize (avatar, modelRoot, scene, finish); };
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
Tootsville.AvatarBuilder.build = function (avatar, scene, finish)
{ if (!scene) { scene = Tootsville.Tank.scene; }
  if (scene == Tootsville.Tank.scene)
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
