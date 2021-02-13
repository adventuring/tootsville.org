/* -*- js2 -*- */

/**@license
 *
 * play/Game/Nav.js is part of Tootsville
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
 * You  should have  received a  copy of  the GNU  Affero General  Public
 * License     along    with     this     program.     If    not,     see
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

if (!('Game' in Tootsville)) { Tootsville.Game = {Nav: {}}; }
if (!('Nav' in Tootsville.Game)) { Tootsville.Game.Nav= {}; }
if (!('now' in Tootsville.Game)) { Tootsville.Game.now = 0; }


/**
 * The walk speed of a Toot. Currently a constant.
 */
Tootsville.Game.Nav.WALK_SPEED = .025;

/**
 * The run speed of a Toot. Currently a constant.
 */
Tootsville.Game.Nav.RUN_SPEED = .04;

/**
 * The speed at which the camera moves
 */
Tootsville.Game.Nav.CAMERA_MOVE_SPEED = Tootsville.Game.Nav.RUN_SPEED * 4;

/**
 * Set   the  course   for  the   given  avatar   to  lead   toward  the
 * given destinationPoint.
 *
 * TODO Allow directing a vehicle when mounted as its driver.
 *
 * TODO Restrict movement when riding a vehicle.
 */
Tootsville.Game.Nav.walkTheLine = function (avatar, destinationPoint, speed=0.1)
{ if (! avatar) { console.warn ("nobody can't walk"); return; }
  if (! avatar.model) { Tootsville.Gossip.Parrot.say ("Something is awry",
                                                      "You need a body to walk");
                        Tootsville.Tank.updateAvatarFor (avatar.name); }
  if (! avatar.model) { console.warn ("No AvatarBuilder body made");
                        return; }
  if (! avatar.model.position) { console.warn ("No avatar.model.position");
                                 return; }
  if (! destinationPoint) { console.warn ("Nowhere to go");
                            return; }
  avatar.course = { startPoint: avatar.model.position,
                    endPoint: destinationPoint,
                    startTime: Tootsville.Game.now + Tootsville.Game.lag,
                    speed: speed || Tootsville.Game.Nav.WALK_SPEED,
                    walkΔ: destinationPoint.subtract (avatar.model.position) };
  avatar.facing = Math.atan2 (avatar.course.walkΔ.x, avatar.course.walkΔ.z);
  if (avatar.facing > 2*Math.PI) { avatar.facing -= 2 * Math.PI; }
  Tootsville.Game.Nav.sendWTL ();
  Tootsville.Game.Nav.gamepadMovementP = false; };

/**
 * Build a Walk The Line packet for the player's avatar
 */
Tootsville.Game.Nav.buildWTL = function ()
{ const avatar = Tootsville.Tank.avatars [ Tootsville.character ];
  let course, facing;
  if (avatar)
  { course = avatar.course;
    facing = avatar.facing;
    if (course)
    { course = { startPoint: { x: course.startPoint.x,
                               y: course.startPoint.y,
                               z: course.startPoint.z },
                 endPoint: { x: course.endPoint.x,
                             y: course.endPoint.y,
                             z: course.endPoint.z },
                 startTime: course.startTime,
                 speed: course.speed }; }
    else
    { let point = { x: avatar.model.position.x,
                    y: avatar.model.position.y,
                    z: avatar.model.position.z };
      course = { startPoint: point,
                 endPoint: point,
                 startTime: Tootsville.Game.now + Tootsville.Game.lag,
                 speed: .1 }; } }
  if (!(facing)) facing = 0;
  return { course: course, facing: facing }; };

/**
 *
 */
Tootsville.Game.Nav.quiesce = function ()
{ Tootsville.Util.infinity ("quiesce",
                            { latitude: Tootsville.activity.lat,
                              longitude: Tootsville.activity.long,
                              altitude: Tootsville.activity.alt,
                              world: Tootsville.activity.world,
                              wtl: Tootsville.Game.Nav.buildWTL (),
                              d3: {},
                              emotion: "" }); };


/**
 * Send a @samp{wtl} packet for the current player
 */
Tootsville.Game.Nav.sendWTL = function ()
{ Tootsville.Util.infinity ("wtl",
                            Tootsville.Game.Nav.buildWTL ()); };

/**
 * Run to ``destinationPoint''
 */
Tootsville.Game.Nav.runTo = function (avatar, destinationPoint)
{ Tootsville.Game.Nav.walkTheLine (avatar, destinationPoint, Tootsville.Game.Nav.RUN_SPEED);};

/**
 * Detect a collision between ``model'' on the line from ``start'' to ``end''
 */
Tootsville.Game.Nav.collisionP = function (model, start, end)
{ // const forward = BABYLON.Vector3.TransformCoordinates ( new BABYLON.Vector3 (0,0,1),
  //                                                      model.getWorldMatrix () );
  if (!Tootsville.Tank.scene) return null;
    const step = end.subtract (start);
    const direction = BABYLON.Vector3.Normalize (step) ;
    const ray = new BABYLON.Ray (start, direction, step.length);
    const hit = Tootsville.Tank.scene.pickWithRay (ray);
    return hit.pickedMesh; };

/**
 * Move an entity  along a course, until its movement  is interrupted by
 * colliding with something else.
 *
 * returns true when the course has been completed
 */
Tootsville.Game.Nav.moveEntityOnCourse = function (entity, course)
{ if (course.startTime > Tootsville.Game.now) { return false; }
  if (! course.endPoint)
  { console.debug (entity.name + " done walking, no endpoint");
    return true; }
  if (! course.endPoint.subtract)
  { course.endPoint = new BABYLON.Vector3 (course.endPoint.x,
                                           course.endPoint.y,
                                           course.endPoint.z);
    course.startPoint = new BABYLON.Vector3 (course.startPoint.x,
                                             course.startPoint.y,
                                             course.startPoint.z); }
  if (! (course.walkΔ && course.walkΔ.subtract))
  { course.walkΔ = course.endPoint.subtract (course.startPoint); }
  if ((! (course.endTime)) || isNaN(course.endTime))
  { course.endTime = course.startTime + course.walkΔ.length () / course.speed; }
  if (course.endTime < Tootsville.Game.now)
  { console.debug (entity.name + " done walking, time is up");
    entity.model.position = new BABYLON.Vector3 (course.endPoint.x, course.endPoint.y, course.endPoint.z);
    return true; }
  if (!entity.model)
  { console.debug (entity.name + " not gonna walk, no model present");
    return true; }

  const goalPosition = course.startPoint.
        add (course.walkΔ.scale ((Tootsville.Game.now - course.startTime)
                                 / (course.endTime - course.startTime)));

  if (isNaN(goalPosition.x) || isNaN(goalPosition.y) || isNaN(goalPosition.z))
  { console.error ("Course fail, ", entity.course, " yields ", goalPosition);
    return true; }

  // goalPosition.subtract (entity.model.position);
  const hit = Tootsville.Game.Nav.collisionP (entity.model, entity.model.position, goalPosition);
  if (hit && "ground" != hit.name)
  { entity.course = null;
    console.debug (entity.name + " ran into an obstacle, stopping due to " + hit.name);
    return true; }

  entity.model.position = new BABYLON.Vector3 (goalPosition.x, goalPosition.y, goalPosition.z);

  if (entity.nameTag || entity.speech)
  { Tootsville.UI.HUD.refreshAttachmentsForAvatar (entity); }

  return false; };

/**
 * Update the avatar's facing direction to match desired direction.
 */
Tootsville.Game.Nav.updateFacing = function (avatar)
{ avatar.model.rotationQuaternion = undefined;
  let δRotation = avatar.model.rotation.y - avatar.facing;
  if (δRotation >= Math.PI)
  { δRotation -= 2 * Math.PI; }
  else if (δRotation <= -Math.PI)
  { δRotation += 2 * Math.PI; }
  avatar.model.rotation.y -= δRotation / 8; };

/**
 * Update avatar's rotation & position.
 */
Tootsville.Game.Nav.updateAvatar = function (avatar)
{ if (Tootsville.Tank.scene)
  { if (! Tootsville.Tank.avatars )
    { Tootsville.Tank.avatars = {}; }
    if (avatar.model)
    { if (Math.abs (avatar.model.rotation.y - avatar.facing) > .01)
      { Tootsville.Game.Nav.updateFacing (avatar); }
      if (avatar.course)
      { let done = Tootsville.Game.Nav.moveEntityOnCourse (avatar, avatar.course);
        if (done) { console.debug (avatar.name + " finished course ", avatar.course, " at " + Tootsville.Game.now );
                    if (avatar.course)
                    { Tootsville.Tank.avatars [ avatar.name ].model.position =
                      new BABYLON.Vector3 (avatar.course.endPoint.x,
                                           avatar.course.endPoint.y,
                                           avatar.course.endPoint.z); }
                    delete avatar['course'];
                    if (avatar.name === Tootsville.character)
                    { Tootsville.Game.Nav.sendWTL (); }}}}}};

/**
 * Merge keys of an object safely
 */
Tootsville.Game.Nav.mergeObjects = function (into, from)
{ for (let key in from)
  { if (from.hasOwnProperty (key))
    { into [ key ] = from [ key ] ; } } };

/**
 * Update the position & rotation of every avatar
 */
Tootsville.Game.Nav.updateAvatars = function ()
{ if ((!Tootsville.Tank.scene) || (!Tootsville.Tank.avatars)) { return; }
  const avatars = Object.values(Tootsville.Tank.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { try { Tootsville.Game.Nav.updateAvatar (avatars [i]); } catch (e) { console.error (e); } } };

Tootsville.Game.Nav.CAMERA_DOLLY_SPEED = 4;
Tootsville.Game.Nav.CAMERA_TRUCK_SPEED = .4;

/**
 * Update the camera's dolly position (foreward/back)
 */
Tootsville.Game.Nav.updateCameraDolly = function (model, cameraPosition)
{ const δPosition = Tootsville.Tank.camera.position.subtract (model.position);
  const absZ = Math.abs (δPosition.z);
  if (absZ > 100)
      cameraPosition = cameraPosition.add (new BABYLON.Vector3 (0, 0, Tootsville.Game.Nav.CAMERA_DOLLY_SPEED));
  else if (absZ < 50)
      cameraPosition = cameraPosition.subtract (new BABYLON.Vector3 (0, 0, Tootsville.Game.Nav.CAMERA_DOLLY_SPEED));
  return cameraPosition; };

/**
 * Update the camera's truck position (left/right)
 */
Tootsville.Game.Nav.updateCameraTruck = function (model, cameraPosition)
{ const renderWidth = Tootsville.Tank.engine.getRenderWidth ();
  const renderHeight = Tootsville.Tank.engine.getRenderHeight ();
  if (! (Tootsville.Tank.camera && Tootsville.Tank.camera.viewport) )
      return cameraPosition;
  const abs = BABYLON.Vector3.Project (
      model.getAbsolutePosition (),
      BABYLON.Matrix.IdentityReadOnly,
      Tootsville.Tank.scene.getTransformMatrix (),
      Tootsville.Tank.camera.viewport.toGlobal (
          renderWidth, renderHeight));
  const relX = abs.x / renderWidth;
  if (relX < 0 || relX > 1)
      cameraPosition = model.position.subtract (new BABYLON.Vector3 (0, -10, 100));
  else if (relX < 1/4)
      cameraPosition = cameraPosition.subtract (new BABYLON.Vector3 (Tootsville.Game.Nav.CAMERA_TRUCK_SPEED, 0, 0)); 
  else if (relX > 3/4) 
      cameraPosition = cameraPosition.add (new BABYLON.Vector3 (Tootsville.Game.Nav.CAMERA_TRUCK_SPEED, 0, 0));
  return cameraPosition; };

/**
 * Update the camera's position
 */
Tootsville.Game.Nav.updateCamera = function ()
{ if (!(Tootsville.Tank.avatars [Tootsville.character])) return;
  const model = Tootsville.Tank.avatars [Tootsville.character].model;
  const camera = Tootsville.Tank.camera;
  if (!(camera) || !(model)) return;
  let cameraPosition = camera.position;
  cameraPosition = Tootsville.Game.Nav.updateCameraDolly (model, cameraPosition);
  cameraPosition = Tootsville.Game.Nav.updateCameraTruck (model, cameraPosition);
  Tootsville.Tank.camera.position = cameraPosition; };

/**
 * Make the character position be precisely x, y, z local
 */
Tootsville.Game.Nav.positionTootAt = function (x, y, z) {
    const avatar = Tootsville.Tank.avatars [ Tootsville.character ];
    avatar.course = { startPoint: new BABYLON.Vector3 (x, y, z),
                      startTime: Tootsville.Game.now,
                      endPoint: new BABYLON.Vector3 (x, y, z),
                      endTime: Tootsville.Game.now,
                      speed: 1 };
    Tootsville.Game.Nav.sendWTL ();
    Tootsville.Game.Nav.gamepadMovementP = false; };

/**
 * Enter a new latitude, longitude, altitude area at local x, y, z in world world
 */
Tootsville.Game.Nav.enterArea = function (latitude, longitude, altitude, world, x, y, z) {
    Tootsville.Util.infinity ('join', { lat: latitude, long: longitude,
                                        alt: altitude, world: world });
    Tootsville.Tank.clearSceneExceptPlayer ();
    Tootsville.Game.Nav.positionTootAt (x, y, z);
};
