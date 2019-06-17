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

if (!('Tootsville' in window)) { Tootsville = { Avatars: { UltraTootBuilder: {} } }; }
if (!('Avatars' in Tootsville)) { Tootsville.Avatars = { UltraTootBuilder: {} }; }
if (!('UltraTootBuilder' in Tootsville.Avatars)) { Tootsville.Avatars.UltraTootBuilder = {}; }

Tootsville.Avatars.getAvatar = function (character)
{ if (!character) { return new Promise ( () => {} ); }
  var xhr = new XMLHttpRequest;
  xhr.open ('GET', Tootsville.host.game + '/toots/' + character + "/avatar");
  return new Promise (
      (finish) =>
          { xhr.onload =
            () =>
            { var avatar = JSON.parse (xhr.response).avatar;
              finish (avatar);
              return; };
            xhr.send (); }); };

Tootsville.Avatars.UltraTootBuilder.model = null;
Tootsville.Avatars.UltraTootBuilder.addProxyToot = function (modelRoot) 
{ // XXX: private
    var proxyHead = BABYLON.MeshBuilder.CreateSphere ('UltraToot.proxy.head',
                                                      { segments: 12, diameter: 1 },
                                                      Tootsville.tank.scene);
    var proxyBody = BABYLON.MeshBuilder.CreateSphere ('UltraToot.proxy.body',
                                                      { segments: 12, diameter: 1 },
                                                      Tootsville.tank.scene);
    proxyHead.position.z = 1.5;
    proxyHead.material = new BABYLON.StandardMaterial ('head', Tootsville.tank.scene);
    proxyHead.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('violet'));
    proxyBody.position.z = .5;
    proxyBody.material = new BABYLON.StandardMaterial ('body', Tootsville.tank.scene);
    proxyBody.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('violet'));
    proxyBody.setParent (modelRoot); };
Tootsville.Avatars.UltraTootBuilder.addMeshesToModelRoot = function (meshes, modelRoot) 
{ try 
  { if (meshes.length == 0) 
    { Tootsville.warn ("Empty container returned for UltraToot");
      Tootsville.Avatars.UltraTootBuilder.addProxyToot (modelRoot); } else 
    { /* Expecting 13 meshes for current UltraToot */
        if (13 != meshes.length) 
        { Tootsville.warn ("Loading Ultratoot got " + meshes.length + " meshes, was expecting 13"); }
        for (var i = 0; i < meshes.length; ++i) 
        { meshes[i].setParent (modelRoot); } } }
  catch (e) 
  { Tootsville.warn ("Error adding meshes to model root for  UltraToot", e);
    Tootsville.Avatars.UltraTootBuilder.addProxyToot (modelRoot); }
  modelRoot.position.y = -Infinity;
  Tootsville.tank.scene.addTransformNode (modelRoot); // TODO: is this necessary?
  Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
  finish (Tootsville.Avatars.UltraTootBuilder.model);
  return; };
Tootsville.Avatars.UltraTootBuilder.importUltraToot = function (finish, meshes, foo, bar) 
{ // XXX: private
    try 
    { Tootsville.trace ("Got UltraToot meshes", meshes);
      var modelRoot = new BABYLON.TransformNode ('UltraToot', Tootsville.tank.scene, true);
      Tootsville.Avatars.addMeshesToModelRoot (meshes, modelRoot);
      modelRoot.position.y = -Infinity;
      Tootsville.tank.scene.addTransformNode (modelRoot); // TODO: is this necessary?
      Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
      finish (Tootsville.Avatars.UltraTootBuilder.model);
      return; }
    catch (e) 
    { Tootsville.error ("Error in importUltraToot: ", e); } };
Tootsville.Avatars.UltraTootBuilder.getBaseModel = function () 
{ return new Promise (
    (finish) => 
        { if (Tootsville.Avatars.UltraTootBuilder.model) 
          { finish (Tootsville.Avatars.UltraTootBuilder.model);
            return; }
          else 
          { BABYLON.SceneLoader.ImportMesh ("UltraToot.mesh",
                                            "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            "UltraToot.babylon",
                                            Tootsville.tank.scene,
                                            (meshes, foo, bar) => { Tootsville.Avatars.importUltraToot (finish, meshes, foo, bar); },
                                            null, /* onprogress */
                                            (scene, errorMessage) => 
                                            { console.log (errorMessage); });
            return; } }); };
Tootsville.Avatars.UltraTootBuilder.setColors = function (node, avatar) 
{ var skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "+" + avatar.patternColor + "×" + avatar.pattern,
                                                   Tootsville.tank.scene);
  skinMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.baseColor));
  var meshes = node.getChildMeshes ();
  // TODO: filter out better
  for (var i = 0; i < meshes.length; ++i) 
  { meshes[i].material = skinMaterial; } };
Tootsville.Avatars.UltraTootBuilder.addClothes = function (node, avatar) 
{ // TODO
};
Tootsville.Avatars.UltraTootBuilder.addBillboards = function (node, avatar) 
{ // TODO: add speech bubble/emote placeholder(s)
    // TODO: add player name box
    // TODO: add child/sensitive player diamond
};
Tootsville.Avatars.UltraTootBuilder.enablePhysics = function (node, avatar) 
{ // TODO — BABYLON.PhysicsJoint.BallAndSocketJoint;
    node.physicsImpostor = new BABYLON.PhysicsImpostor (node,
                                                        BABYLON.PhysicsImpostor.SphereImpostor,
                                                        { mass: 1, restitution: 0.9 },
                                                        Tootsville.tank.scene); };
Tootsville.Avatars.UltraTootBuilder.makeToot = function (avatar) 
{ return new Promise ( (finish) => 
                       { if (avatar == 'UltraToot') 
                         { Tootsville.Avatars.UltraTootBuilder.getBaseModel ().then ( (model) => 
                                                                                      { var toot = model.clone ();
                                                                                        toot.name = 'avatar/' + avatar.name;
                                                                                        Tootsville.Avatars.UltraTootBuilder.enablePhysics (toot, avatar);
                                                                                        Tootsville.Avatars.UltraTootBuilder.setColors (toot, avatar);
                                                                                        Tootsville.Avatars.UltraTootBuilder.addClothes (toot, avatar);
                                                                                        finish (toot);
                                                                                        return; }); } else 
                         { Tootsville.warn ("Avatar is not UltraToot: " + avatar.toSource ());
                           /* TODO  try to load non-UltraToot avatar */
                           var proxy = BABYLON.MeshBuilder.CreateSphere ('proxy for ' + avatar.name,
                                                                         { segments: 8, diameter: .25},
                                                                         Tootsville.tank.scene);
                           proxy.material = new BABYLON.StandardMaterial ('red', Tootsville.tank.scene);
                           proxy.material.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor ('red'));
                           finish (proxy);
                           return; } }); };
Tootsville.Avatars.UltraTootBuilder.moveToot = function (toot, δv) 
{ //         var forwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
    // forwards.negate ();
    // character.moveWithCollisions (forwards);
    // // or
    // var backwards = new BABYLON.Vector3 (parseFloat (Math.sin (character.rotation.y)) / speedCharacter, -gravity, parseFloat (Math.cos (character.rotation.y)) / speedCharacter);
    // character.moveWithCollisions (backwards);
};
