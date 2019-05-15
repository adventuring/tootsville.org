if (! Tootsville.Avatars) {
    Tootsville.Avatars = {};
}

Tootsville.Avatars.getAvatar = function (character)
{ if (!character) { return; }
  var xhr = new XMLHttpRequest;
  xhr.open('GET', Tootsville.host.users + '/toots/' + character + "/avatar");
  return new Promise(
      (finish) =>
          { xhr.onload =
            () =>
            { var avatar = JSON.parse(xhr.response).avatar;
              finish(avatar);
              return; }
            xhr.send(); }); };

Tootsville.Avatars.UltraTootBuilder = {
    model: null,
    addProxyToot: function (modelRoot) {
        // XXX: private
        var proxyHead = BABYLON.MeshBuilder.CreateSphere('UltraToot.proxy.head',
                                                         { segments: 12, diameter: 1 },
                                                         Tootsville.tank.scene);
        var proxyBody = BABYLON.MeshBuilder.CreateSphere('UltraToot.proxy.body',
                                                         { segments: 12, diameter: 1 },
                                                         Tootsville.tank.scene);
        proxyHead.position.y = 1.5;
        proxyBody.position.y = .5;
        proxyHead.setParent(modelRoot);
        proxyBody.setParent(modelRoot);
    },
    addMeshesToModelRoot: function(meshes) {
        try {
            if (meshes.length == 0) {
                Tootsville.warn("Empty container returned for UltraToot");
                addProxyToot(modelRoot);
            } else {
                /* Expecting 13 meshes for current UltraToot */
                if (13 != meshes.length) {
                    Tootsville.warn("Loading Ultratoot got " + meshes.length + " meshes, was expecting 13");
                }
                for (var i = 0; i < meshes.length; ++i) {
                    meshes[i].setParent(modelRoot);
                }
            }
        } catch (e) {
            Tootsville.warn("Error importing UltraToot: " + e.toString());
            console.warn(e);
            addProxyToot(modelRoot);
        }
        modelRoot.position.y = -Infinity;
        Tootsville.tank.scene.addTransformNode(modelRoot); // TODO: is this necessary?
        Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
        finish(Tootsville.Avatars.UltraTootBuilder.model);
        return;
    },
    importUltraToot: function(finish, meshes, foo, bar) {
        // XXX: private
        try {
            var modelRoot = new BABYLON.TransformNode('UltraToot', Tootsville.tank.scene, true);
            Tootsville.Avatars.addMeshesToModelRoot(meshes, modelRoot);
            modelRoot.position.y = -Infinity;
            Tootsville.tank.scene.addTransformNode(modelRoot); // TODO: is this necessary?
            Tootsville.Avatars.UltraTootBuilder.model = modelRoot;
            finish(Tootsville.Avatars.UltraTootBuilder.model);
            return;
        } catch (e) {
            Tootsville.error("Error in importUltraToots: ", e);
        }
    },
    getBaseModel: function () {
        return new Promise((finish) => {
            if (Tootsville.Avatars.UltraTootBuilder.model) {
                finish(Tootsville.Avatars.UltraTootBuilder.model);
                return;
            } else {
                BABYLON.SceneLoader.ImportMesh("UltraToot.mesh",
                                               "https://jumbo.tootsville.org/Assets/Avatars/Violet-Volts/Assets/",
                                               "UltraToot.babylon",
                                               Tootsville.tank.scene,
                                               (meshes, foo, bar) => { Tootsville.Avatars.importUltraToot(finish, meshes, foo, bar); },
                                               null, /* onprogress */
                                               (scene, errorMessage) => {
                                                   console.log(errorMessage);
                                               });
                return;
            }
        });
    },
    setColors: function (node, avatar) {
        var skinMaterial = new BABYLON.StandardMaterial(avatar.baseColor + "+" + avatar.patternColor + "×" + avatar.pattern,
                                                        Tootsville.tank.scene);
        skinMaterial.diffuseColor = new BABYLON.Color3.FromHexString(interpretTootColor(avatar.baseColor));
        var meshes = node.getChildMeshes();
        // TODO: filter out better
        for (var i = 0; i < meshes.length; ++i) {
            meshes[i].material = skinMaterial;
        }
    },
    addClothes: function (node, avatar) {
        // TODO
    },
    addBillboards: function (node, avatar) {
        // TODO: add speech bubble/emote placeholder(s)
        // TODO: add player name box
        // TODO: add child/sensitive player diamond
    },
    enablePhysics: function (node, avatar) {
        // TODO — BABYLON.PhysicsJoint.BallAndSocketJoint;
        node.physicsImpostor = new BABYLON.PhysicsImpostor(node,
                                                           BABYLON.PhysicsImpostor.SphereImpostor,
                                                           { mass: 1, restitution: 0.9 },
                                                           Tootsville.tank.scene);
    },
    makeToot: function (avatar) {
        return new Promise((finish) => {
            if (avatar.avatar == 'UltraToot') {
                Tootsville.Avatars.UltraTootBuilder.getBaseModel().then( (model) => {
                    var toot = model.clone();
                    toot.name = 'avatar/' + avatar.name;
                    Tootsville.Avatars.UltraTootBuilder.enablePhysics(toot, avatar);
                    Tootsville.Avatars.UltraTootBuilder.setColors(toot, avatar);
                    Tootsville.Avatars.UltraTootBuilder.addClothes(toot, avatar);
                    finish(toot);
                    return;
                });
            } else {
                Tootsville.warn("Avatar is not UltraToot: " + avatar.toSource());
                var proxy = BABYLON.MeshBuilder.CreateSphere('proxy for ' + avatar.name,
                                                             { segments: 8, diameter: .25},
                                                             Tootsville.tank.scene);
                proxy.material = new BABYLON.StandardMaterial('red', Tootsville.tank.scene);
                proxy.material.diffuseColor = new BABYLON.Color3.FromHexString(interpretTootColor('red'));
                finish(proxy);
                return;
            }
        });
    },
    moveToot: function (toot, δv) {
        //         var forwards = new BABYLON.Vector3(parseFloat(Math.sin(character.rotation.y)) / speedCharacter, gravity, parseFloat(Math.cos(character.rotation.y)) / speedCharacter);
        // forwards.negate();
        // character.moveWithCollisions(forwards);
        // // or
        // var backwards = new BABYLON.Vector3(parseFloat(Math.sin(character.rotation.y)) / speedCharacter, -gravity, parseFloat(Math.cos(character.rotation.y)) / speedCharacter);
        // character.moveWithCollisions(backwards);
    }
};
