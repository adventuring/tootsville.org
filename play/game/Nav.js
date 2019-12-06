/* -*- js2 -*- */

/**@license
 *
 * ./play/game/Nav.js is part of Tootsville
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
 * 
 */
Tootsville.Game.Nav.walkTheLine = function (avatar, destinationPoint)
{ avatar.course = { startPoint: avatar.model.position,
                    endPoint: destinationPoint,
                    startTime: Tootsville.Game.now + Tootsville.Game.lag,
                    speed: Tootsville.Game.Nav.WALK_SPEED,
                    walkΔ: destinationPoint.subtract (avatar.model.position) };
  avatar.facing = Math.PI + Math.atan2 (avatar.course.walkΔ.x, avatar.course.walkΔ.z); };

/**
 *
 *
 * returns true when the course has been completed
 */
Tootsville.Game.Nav.updateWalk = function (avatar, course)
{ if (course.startTime > Tootsville.Game.now) { return false; }
  if (! course.walkΔ)
  { course.walkΔ = course.endPoint.substract (course.startPoint); }
  if (! course.endTime)
  { course.endTime = Tootsville.Game.lag * 2 + course.startTime + course.walkΔ.length () / course.speed; }
  if (course.endTime < Tootsville.Game.now) { return true; }

  const position = course.startPoint.
        add (course.walkΔ.scale ((Tootsville.Game.now - course.startTime)
                                 / (course.endTime - course.startTime)));

  if (isNaN(position.x) || isNaN(position.y) || isNaN(position.z))
  { console.error ("Course fail, ", avatar.course, " yields ", position);
    return true; }

  avatar.model.moveWithCollisions (position.subtract (avatar.model.position));
  
  Tootsville.UI.HUD.refreshAttachmentsForAvatar (avatar);

  return false; };

/**
 *
 */
Tootsville.Game.Nav.updateFacing = function (avatar)
{ return;

  // This makes him spastic … FIXME #25
  let δRotation = avatar.model.rotationQuaternion.y - avatar.facing;
  if (δRotation >= Math.PI)
  { δRotation -= Math.PI * 2; }
  else if (δRotation <= -Math.PI)
  { δRotation += Math.PI * 2; }
  let rotationSpeed = Math.abs(δRotation/8);
  if (δRotation > 0)
  { avatar.model.rotate(BABYLON.Axis.Y, -rotationSpeed); }
  else
  { avatar.model.rotate(BABYLON.Axis.Y, rotationSpeed); } };

/**
 *
 */
Tootsville.Game.Nav.updateAvatar = function (avatar)
{ if (! avatar.model) { return; }
  if (! avatar.model.rotationQuaternion) {
      // WtH?
      avatar.model.rotationQuaternion = new BABYLON.Quaternion (0, avatar.facing, 0, 0); }
  if (Math.abs (avatar.model.rotationQuaternion.y - avatar.facing) > .01)
  { Tootsville.Game.Nav.updateFacing (avatar); }
  if (avatar.course)
  { let done = Tootsville.Game.Nav.updateWalk (avatar, avatar.course);
    if (done) { delete avatar['course']; } } };

/**
 *
 */
Tootsville.Game.Nav.updateAvatars = function ()
{ if ((!Tootsville.Tank.scene) || (!Tootsville.Tank.scene.avatars)) { return; }
  const avatars = Object.values(Tootsville.Tank.scene.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { try { Tootsville.Game.Nav.updateAvatar (avatars [i]); } catch (e) { console.error (e); } } };

