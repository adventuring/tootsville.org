/* -*- js2 -*-*/

/**@license
 *
 * ./play/scene/CameraManager.js is part of Tootsville
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

if (! ("Tank" in Tootsville)) { Tootsville.Tank = { CameraManager: {} }; }
if (! ("CameraManager" in Tootsville.Tank)) { Tootsville.Tank.CameraManager = {}; }

Tootsville.Tank.CameraManager.ZOOM_MODE_GAME = undefined;
Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_VIEWER = 1;
Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_CLOSEUP = 2;

Tootsville.Tank.CameraManager.updateCamera = function (camera, avatar, zoomMode)
{ if (zoomMode == Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_VIEWER)
  { Tootsville.Tank.CameraManager.positionCameraForAvatarViewer (camera, avatar); }
  else if (zoomMode == Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_CLOSEUP)
  { Tootsville.Tank.CameraManager.positionCameraForAvatarCloseUp (camera, avatar); }
  else // Tootsville.Tank.CameraManager.ZOOM_MODE_GAME
  { Tootsville.Tank.CameraManager.positionCameraForGameBoard (camera, avatar); } };

Tootsville.Tank.CameraManager.positionCameraForAvatarViewer = function (camera, avatar)
{ console.error ("Unimplemented: positionCameraForAvatarViewer"); };
Tootsville.Tank.CameraManager.positionCameraForAvatarCloseUp = function (camera, avatar)
{ console.error ("Unimplemented: positionCameraForAvatarCloseUp"); };
Tootsville.Tank.CameraManager.positionCameraForGameBoard = function (camera, avatar)
{ const worldMatrix = avatar.model.getWorldMatrix();
  const transformMatrix = camera.getScene().getTransformMatrix();
  const avatarSphere = avatar.model.getBoundingInfo ().boundingSphere;
  const viewport = camera.viewport;
  const screenCenter = BABYLON.Vector3.Project (avatarSphere.centerWorld, worldMatrix, transformMatrix, viewport);
  const screenRadius = BABYLON.Vector3.Project (avatarSphere.radiusWorld, worldMatrix, transformMatrix, viewport);


}
        
