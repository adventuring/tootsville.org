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
                 speed: Tootsville.Game.Nav.WALK_SPEED }; } }
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
Tootsville.Game.Nav.collisionP = function (model, end)
{ // const forward = BABYLON.Vector3.TransformCoordinates ( new BABYLON.Vector3 (0,0,1),
  //                                                      model.getWorldMatrix () );
  if (!Tootsville.Tank.scene) return null;
    const highStart = new BABYLON.Vector3 (model.position.x,
                                           model.getBoundingInfo().boundingSphere.centerWorld.y,
                                           model.position.z);
    const highEnd = new BABYLON.Vector3 (end.x,
                                       highStart.y,
                                       end.z);
    const step = highEnd.subtract (highStart);
    const direction = BABYLON.Vector3.Normalize (step) ;
    const ray = new BABYLON.Ray (highStart, direction, step.length);
    const hit = Tootsville.Tank.scene.pickWithRay (ray);
    if (hit.distance < step.length ())
        return hit.pickedMesh;
    else return null};

/**
 * Ensure that the course data seems sane. 
 *
 * Calculate missing bits.
 */
Tootsville.Game.Nav.validateCourse = function (course, entity) {
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
    return course;
};

Tootsville.Game.Nav.SECTOR_SIZE = 512; /* length of each side = 2 × this value */

/**
 * The player has entered a new sector.
 *
 * Update the lat & long and call `Tootsville.Game.Nav.enterArea' to
 * get the new room vars.
 */
Tootsville.Game.Nav.moveToNextSector = function (position) {
    let lat = Tootsville.activity.lat;
    let long = Tootsville.activity.long;
    if (position.x < -Tootsville.Game.Nav.SECTOR_SIZE) {
        position.x += 2 * Tootsville.Game.Nav.SECTOR_SIZE;
        --lat;
    } else if (position.x > Tootsville.Game.Nav.SECTOR_SIZE) {
        position.x -= 2 * Tootsville.Game.Nav.SECTOR_SIZE;
        ++lat;
    }
    if (position.z < -Tootsville.Game.Nav.SECTOR_SIZE) {
        position.z += 2* Tootsville.Game.Nav.SECTOR_SIZE;
        ++long;
    } else if (position.z > Tootsville.Game.Nav.SECTOR_SIZE) {
        position.z -= 2 * Tootsville.Game.Nav.SECTOR_SIZE;
        --long;
    }
    Tootsville.Game.Nav.enterArea (lat, long, 0, Tootsville.activity.world,
                                   position.x, position.y, position.z);
    return position;
};

/**
 * Has the entity left the sector?
 */
Tootsville.Game.Nav.leftSectorP = function (goalPosition) {
    return (goalPosition.x < -Tootsville.Game.Nav.SECTOR_SIZE) ||
        (goalPosition.x > Tootsville.Game.Nav.SECTOR_SIZE) ||
        (goalPosition.z < -Tootsville.Game.Nav.SECTOR_SIZE) ||
        (goalPosition.z > Tootsville.Game.Nav.SECTOR_SIZE); };

/**
 * Compute one step along the movement line of the course
 */
Tootsville.Game.Nav.takeAStep = function (course) {
    return course.startPoint.
        add (course.walkΔ.scale ((Math.min(Tootsville.Game.now, course.endTime) - course.startTime)
                                 / (course.endTime - course.startTime))); };

/**
 * Detect whether the coördinates passed are invalid
 * 
 * Currently just checks for NaN axes
 */
Tootsville.Game.Nav.invalidCoordsP = function (goalPosition) {
    return (isNaN(goalPosition.x) ||
            isNaN(goalPosition.y) ||
            isNaN(goalPosition.z)); };

/**
 * Move an entity  along a course
 *
 * Stop if its movement is interrupted by colliding with something
 * else.
 *
 * returns true when the course has been completed
 */
Tootsville.Game.Nav.moveEntityOnCourse = function (entity, course)
{/* First make sure that the course data is sane.*/
    entity.course = course = Tootsville.Game.Nav.validateCourse (course, entity);
    if (course.startTime > Tootsville.Game.now) { return false; }
    if (! course.endPoint)
    { console.debug (entity.name + " done walking, no endpoint");
      return true; }
    if (!entity.model)
    { console.debug (entity.name + " not gonna walk, no model present");
      return true; }

    if (0 === course.walkΔ.length ()) return true;

    /* Where should we be after this step? */
    let goalPosition = Tootsville.Game.Nav.takeAStep (course);

    if (Tootsville.Game.Nav.invalidCoordsP (goalPosition))
    { console.error ("Course fail, ", entity.course, " yields ", goalPosition);
      return true; }

    const hit = Tootsville.Game.Nav.collisionP (entity.model,
                                                goalPosition);
    if (hit && "ground" != hit.name)
    { entity.course = null;
      console.debug (entity.name + " ran into an obstacle, stopping due to " +
                     hit.name);
      return true; }

    if (Tootsville.Game.Nav.leftSectorP (goalPosition))
    { if (entity === Tootsville.Tank.avatars[Tootsville.character])
      { goalPosition = Tootsville.Game.Nav.moveToNextSector (goalPosition);
        course.startPosition = course.endPosition = goalPosition;
        course.startTime = course.endTime = Tootsville.Game.now;
      } else {
          /* TODO what about other entities */
      }
    }

    if (course.endTime < Tootsville.Game.now)
    { console.debug (entity.name + " done walking, time is up");
      entity.model.position = new BABYLON.Vector3 (course.endPoint.x,
                                                   course.endPoint.y,
                                                   course.endPoint.z);
      return true; }

    entity.model.position = new BABYLON.Vector3 (goalPosition.x,
                                                 goalPosition.y,
                                                 goalPosition.z);

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
 * Finish moving
 */
Tootsville.Game.Nav.finishMovingAvatar = function (avatar) 
{ console.debug (avatar.name + " finished course ",
                 avatar.course,
                 " at " + Tootsville.Game.now );
  if (avatar.course)
  { Tootsville.Tank.avatars [ avatar.name ].model.position =
    new BABYLON.Vector3 (avatar.course.endPoint.x,
                         avatar.course.endPoint.y,
                         avatar.course.endPoint.z); }
  delete avatar['course'];
  if (avatar.name === Tootsville.character)
      Tootsville.Game.Nav.sendWTL (); };

/**
 * Update avatar's rotation & position.
 */
Tootsville.Game.Nav.updateAvatar = function (avatar)
{ if (!(Tootsville.Tank.scene)) return;
  if (! Tootsville.Tank.avatars )
      Tootsville.Tank.avatars = {};
  if (avatar.model)
  { if (Math.abs (avatar.model.rotation.y - avatar.facing) > .01)
      Tootsville.Game.Nav.updateFacing (avatar);
    if (avatar.course)
    { const done =
            Tootsville.Game.Nav.moveEntityOnCourse (avatar, avatar.course);
      if (done)
          Tootsville.Game.Nav.finishMovingAvatar (avatar); }}};

/**
 * Merge keys of an object safely
 *
 * This really belongs in a utility namespace and may not even be as
 * correct as it could be under extreme conditions.
 *
 * XXX unclear if this is even used any more?
 */
Tootsville.Game.Nav.mergeObjects = function (into, from)
{ for (let key in from)
    if (from.hasOwnProperty (key))
        into [ key ] = from [ key ]; };

/**
 * Update the position & rotation of every avatar
 */
Tootsville.Game.Nav.updateAvatars = function ()
{ if ((!Tootsville.Tank.scene) || (!Tootsville.Tank.avatars)) { return; }
  const avatars = Object.values(Tootsville.Tank.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { try { Tootsville.Game.Nav.updateAvatar (avatars [i]); }
    catch (e) { console.error (e); } } };

/**
 * Make the character position be precisely x, y, z local
 *
 * Eliminates movement and sends a WTL packet
 */
Tootsville.Game.Nav.positionTootAt = function (x, y, z) {
    const avatar = Tootsville.Tank.avatars [ Tootsville.character ];
    avatar.course = { startPoint: new BABYLON.Vector3 (x, y, z),
                      startTime: Tootsville.Game.now,
                      endPoint: new BABYLON.Vector3 (x, y, z),
                      endTime: Tootsville.Game.now,
                      speed: Tootsville.Game.Nav.WALK_SPEED };
    Tootsville.Game.Nav.sendWTL ();
    Tootsville.Game.Nav.gamepadMovementP = false; };

/**
 * Enter a new sector of the game world
 *
 * ...at latitude, longitude, altitude at local x, y, z in world world
 */
Tootsville.Game.Nav.enterArea = function (latitude, longitude, altitude,
                                          world, x, y, z) {
    Tootsville.Util.infinity ('join', { lat: latitude, long: longitude,
                                        alt: altitude, world: world });
    Tootsville.Tank.clearSceneExceptPlayer ();
    Tootsville.Game.Nav.positionTootAt (x, y, z); };
