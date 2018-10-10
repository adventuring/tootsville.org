if (!Tootsville.tank) {
    Tootsville.tank = {};
}

Tootsville.tank.acceptClick = function (event, pickedP, distance,
                                        pickedMesh, pickedPoint)
{
    if (! pickedP) { return; }
    if (Tootsville.pickTargetListener) {
        Tootsville.pickTargetListener(pickedMesh, pickedPoint);
    } else if (pickedMesh.acceptAction) {
        pickedMesh.acceptAction({ method: 'click',
                                  target: pickedMesh,
                                  point: pickedPoint });
    }
};

Tootsville.tank.initOTSCamera = function () {
    var camera = new BABYLON.FollowCamera(
        'otsCamera',
        new BABYLON.Vector3(0, 10, -100),
        Tootsville.tank.scene);
    camera.radius = 5; // how closely to follow our Toot
    camera.heightOffset = 4.5;
    camera.rotationOffset = 0;
    camera.cameraAcceleration = .005;
    camera.maxCameraSpeed = 10;
    // camera.ellipsoid = new BABYLON.Vector3(.5,1,.5);
    // camera.checkCollisions = true;
    Tootsville.tank.camera = camera;
};

Tootsville.tank.initPhysics = function (world) {
    var gravityVector =
        new BABYLON.Vector3(0,
                            -Tootsville.Worlds[world].Gravity,
                            0);
    Tootsville.tank.scene.enablePhysics(gravityVector,
                                        Tootsville.tank.physics);
};

Tootsville.tank.getCanvas = function () {
    if (! Tootsville.tank.canvas) {
        Tootsville.tank.canvas =
            document.getElementById('tootsville3d');
    }
    if (! Tootsville.tank.canvas) {
        var canvas = document.createElement('CANVAS');
        document.getElementsByTagName('BODY').appendChild(canvas);
        Tootsville.tank.canvas = canvas;
    }
    return Tootsville.tank.canvas;
};

Tootsville.tank.convertCanvasEventTo3D = function (event) {
    var picked = Tootsville.tank.scene.pick(
        Tootsville.tank.scene.pointerX,
        Tootsville.tank.scene.pointerY);
    Tootsville.tank.acceptClick(event, picked.hit,
                                picked.distance,
                                picked.pickedMesh,
                                picked.pickedPoint);
};

Tootsville.tank.init3DEngine = function () {
    return new Promise( (finish) => {
        Tootsville.tank.getCanvas();
        if (! Tootsville.tank.engine) {
            Tootsville.tank.engine =
                new BABYLON.Engine(Tootsville.tank.canvas,
                                   true);
        }
        if (! Tootsville.tank.physics) {
            Tootsville.tank.physics = new BABYLON.CannonJSPlugin();
        }
        if (! Tootsville.tank.scene) {
            Tootsville.tank.scene =
                new BABYLON.Scene(Tootsville.tank.engine);
        }
        // TODO confirm if this is engine or scene:
        Tootsville.tank.engine.workerCollisions = true;
        Tootsville.tank.canvas.addEventListener(
            'click',
            Tootsville.tank.convertCanvasEventTo3D);
        finish();
    });
};

Tootsville.tank.initCrappyDefaultLight = function () {
    var light = new BABYLON.HemisphericLight(
        'uplight',
        new BABYLON.Vector3(0,1,0),
        Tootsville.tank.scene);
};

Tootsville.tank.initPlayerToot = function () {
    Tootsville.Avatars.getAvatar(Tootsville.character && Tootsville.character.name).then(
        (toot) => {
            Tootsville.Avatars.UltraTootBuilder.makeToot(
                toot
            ).then(
                (ultraToot) => {
                    console.log('loaded a Toot');
                    Tootsville.tank.camera.lockedTarget =
                        ultraToot;
                });
        });
};

Tootsville.tank.initGroundPlane = function () {
    var ground =
        BABYLON.Mesh.CreateGround('ground',
                                  { height: 100, width: 100,
                                    subdivisions: 10 },
                                  Tootsville.tank.scene);
    ground.material =
        new BABYLON.StandardMaterial('ground',
                                     Tootsville.tank.scene);
    ground.material.diffuseColor =
        new BABYLON.Color3.FromHexString(interpretTootColor('green'));
    ground.physicsImpostor =
        new BABYLON.PhysicsImpostor(
            ground,
            BABYLON.PhysicsImpostor.BoxImpostor,
            { mass: 0, restitution: 3 });
    ground.checkCollisions = true;
};

Tootsville.tank.createTestScene = function () {
    Tootsville.tank.initOTSCamera();
    Tootsville.tank.initPhysics('Tootanga');
    Tootsville.tank.initCrappyDefaultLight();
    Tootsville.tank.initPlayerToot();
    Tootsville.tank.initGroundPlane();
    return Tootsville.tank.scene;
};

Tootsville.tank.startRenderLoop = function () {
    Tootsville.tank.engine.runRenderLoop(
        Tootsville.tank.scene.render);
};

Tootsville.tank.start3D = function () {
    Tootsville.util.loadScript('https://cdn.babylonjs.com/babylon.js').then(Tootsville.tank.start3DIfReady);
    Tootsville.util.loadScript('https://cdn.babylonjs.com/cannon.js').then(Tootsville.tank.start3DIfReady);
};

Tootsville.tank.start3DIfReady = function () {
    if ((! window.BABYLON) || (! window.CANNON)) { return; }
    BABYLON.SceneLoader.ShowLoadingScreen = false;

    Tootsville.tank.init3DEngine().then(() => {
        Tootsville.tank.createTestScene();
        Tootsville.tank.startRenderLoop();
        window.addEventListener('resize', Tootsville.tank.engine.resize);
    });
};
