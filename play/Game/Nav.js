/* -*- js2 -*- */

/**@license
 *
 * play/Game/Nav.js is part of Tootsville
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
Tootsville.Game.Nav.walkTheLine = function (avatar, destinationPoint)
{ avatar.course = { startPoint: avatar.model.position,
                    endPoint: destinationPoint,
                    startTime: Tootsville.Game.now + Tootsville.Game.lag,
                    speed: Tootsville.Game.Nav.WALK_SPEED,
                    walkΔ: destinationPoint.subtract (avatar.model.position) };
  avatar.facing = Math.PI + Math.atan2 (avatar.course.walkΔ.x, avatar.course.walkΔ.z);
  if (avatar.facing > 2*Math.PI) { avatar.facing -= 2 * Math.PI; }
  Tootsville.Game.Nav.gamepadMovementP = false; };

/**
*
*/
Tootsville.Game.Nav.runTo = function (avatar, destinationPoint)
{ Tootsville.Game.Nav.walkTheLine (avatar, destinationPoint);
  avatar.course.speed = Tootsville.Game.Nav.RUN_SPEED; };

/**
 *
 */
Tootsville.Game.Nav.collisionP = function (model, start, end)
{  // const forward = BABYLON.Vector3.TransformCoordinates ( new BABYLON.Vector3 (0,0,1),
    //                                                      model.getWorldMatrix () );
    if (!Tootsville.Tank.scene) { return null; }
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
  if (! course.walkΔ)
  { course.walkΔ = course.endPoint.substract (course.startPoint); }
  if (! course.endTime)
  { course.endTime = Tootsville.Game.lag * 2 + course.startTime + course.walkΔ.length () / course.speed; }
  if (course.endTime < Tootsville.Game.now) { return true; }
  if (!entity.model) { return true; }

  const goalPosition = course.startPoint.
        add (course.walkΔ.scale ((Tootsville.Game.now - course.startTime)
                                 / (course.endTime - course.startTime)));

  if (isNaN(goalPosition.x) || isNaN(goalPosition.y) || isNaN(goalPosition.z))
  { console.error ("Course fail, ", entity.course, " yields ", goalPosition);
    return true; }

   // goalPosition.subtract (entity.model.position);
  const hit = Tootsville.Game.Nav.collisionP (entity.model, entity.model.position, goalPosition);
  if (hit)
  { entity.course = null; return true; }
  
  entity.model.position = goalPosition;
  
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
{ if (! avatar.model) { return; }
  if (Tootsville.Login.toots [ avatar.name ])
  { Tootsville.Login.toots [ avatar.name ] = avatar;
    Tootsville.Login.populateTootsList (); };
  if (avatar.uuid == Tootsville.characterUUID)
  { Tootsville.character = avatar; }
  if (Math.abs (avatar.model.rotation.y - avatar.facing) > .01)
  { Tootsville.Game.Nav.updateFacing (avatar); }
  if (! Tootsville.Tank.scene.avatars )
  { Tootsville.Tank.scene.avatars = {}; }
  Tootsville.Tank.scene.avatars [ avatar.name ] = avatar;
  if (avatar.course)
  { let done = Tootsville.Game.Nav.moveEntityOnCourse (avatar, avatar.course);
    if (done) { delete avatar['course']; } } };

/**
 * Update the position & rotation of every avatar
 */
Tootsville.Game.Nav.updateAvatars = function ()
{ if ((!Tootsville.Tank.scene) || (!Tootsville.Tank.scene.avatars)) { return; }
  const avatars = Object.values(Tootsville.Tank.scene.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { try { Tootsville.Game.Nav.updateAvatar (avatars [i]); } catch (e) { console.error (e); } } };

