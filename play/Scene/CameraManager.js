/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/CameraManager.js is part of Tootsville
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

if (! ("Tank" in Tootsville)) { Tootsville.Tank = { CameraManager: {} }; }
if (! ("CameraManager" in Tootsville.Tank)) { Tootsville.Tank.CameraManager = {}; }

Tootsville.Tank.CameraManager.ZOOM_MODE_GAME = undefined;
Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_VIEWER = 1;
Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_CLOSEUP = 2;

Tootsville.Tank.CameraManager.updateCamera = function (camera, avatar, zoomMode)
{ if (zoomMode === Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_VIEWER)
  { Tootsville.Tank.CameraManager.positionCameraForAvatarViewer (camera, avatar); }
  else if (zoomMode === Tootsville.Tank.CameraManager.ZOOM_MODE_AVATAR_CLOSEUP)
  { Tootsville.Tank.CameraManager.positionCameraForAvatarCloseUp (camera, avatar); }
  else // Tootsville.Tank.CameraManager.ZOOM_MODE_GAME
  { Tootsville.Tank.CameraManager.positionCameraForGameBoard (camera, avatar); } };

/**
 * Position the camera to enclose the avatar completely.
 */
Tootsville.Tank.CameraManager.positionCameraForAvatarViewer = function (camera, avatar)
{ console.error ("Unimplemented: positionCameraForAvatarViewer"); };

/**
 * Position the camera to have the avatar about ¼ the screen width.
 */
Tootsville.Tank.CameraManager.positionCameraForAvatarCloseUp = function (camera, avatar)
{ console.error ("Unimplemented: positionCameraForAvatarCloseUp"); };

/**
 * The speed at which the camera moves
 */
Tootsville.Tank.CameraManager.CAMERA_MOVE_SPEED = Tootsville.Game.Nav.RUN_SPEED * 4;

Tootsville.Tank.CameraManager.CAMERA_DOLLY_SPEED = 4;
Tootsville.Tank.CameraManager.CAMERA_TRUCK_SPEED = .4;

/**
 * Update the camera's dolly position (foreward/back)
 */
Tootsville.Tank.CameraManager.updateCameraDolly = function (model, cameraPosition)
{ const δPosition = Tootsville.Tank.camera.position.subtract (model.position);
  const absZ = Math.abs (δPosition.z);
  if ((absZ < 400) || (absZ > 400))
      cameraPosition = model.position.subtract (new BABYLON.Vector3 (0, -10, 100));
  else if (absZ > 100)
      cameraPosition = cameraPosition.add (new BABYLON.Vector3 (0, 0,
                                                                Tootsville.Tank.CameraManager.CAMERA_DOLLY_SPEED));
  else if (absZ < 50)
      cameraPosition = cameraPosition.subtract (new BABYLON.Vector3 (0, 0,
                                                                     Tootsville.Tank.CameraManager.CAMERA_DOLLY_SPEED));
  return cameraPosition; };

/**
 * Update the camera's truck position (left/right)
 */
Tootsville.Tank.CameraManager.updateCameraTruck = function (model, cameraPosition)
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
      cameraPosition = cameraPosition.subtract (new BABYLON.Vector3 (Tootsville.Tank.CameraManager.CAMERA_TRUCK_SPEED,
                                                                     0, 0)); 
  else if (relX > 3/4) 
      cameraPosition = cameraPosition.add (new BABYLON.Vector3 (Tootsville.Tank.CameraManager.CAMERA_TRUCK_SPEED,
                                                                0, 0));
  return cameraPosition; };

/**
 * Update the camera's position
 */
Tootsville.Tank.CameraManager.updateCamera = function ()
{ if (!(Tootsville.Tank.avatars [Tootsville.character])) return;
  const model = Tootsville.Tank.avatars [Tootsville.character].model;
  const camera = Tootsville.Tank.camera;
  if (!(camera) || !(model)) return;
  let cameraPosition = camera.position;
  cameraPosition = Tootsville.Tank.CameraManager.updateCameraDolly (model, cameraPosition);
  cameraPosition = Tootsville.Tank.CameraManager.updateCameraTruck (model, cameraPosition);
  Tootsville.Tank.camera.position = cameraPosition; };

/**
 * Position the camera to have the avatar about 1⁄20 the screen width.
 */
Tootsville.Tank.CameraManager.positionCameraForGameBoard = function (camera, avatar)
{ // const worldMatrix = avatar.model.getWorldMatrix();
  // const transformMatrix = camera.getScene().getTransformMatrix();
  // const avatarSphere = avatar.model.getBoundingInfo ().boundingSphere;
  // const viewport = camera.viewport;
  // const screenCenter = BABYLON.Vector3.Project (avatarSphere.centerWorld, worldMatrix, transformMatrix, viewport);
  // const screenRadius = BABYLON.Vector3.Project (avatarSphere.radiusWorld, worldMatrix, transformMatrix, viewport);


};

/*
  BABYLON.Vector3.Project (
  model.getAbsolutePosition (),
  BABYLON.Matrix.IdentityReadOnly,
  Tootsville.Tank.scene.getTransformMatrix (),
  Tootsville.Tank.camera.viewport.toGlobal (
  renderWidth, renderHeight)).divide (
  {x: renderWidth, y: renderHeight, z: 1}).multiply (
  {x: document.getElementById('tootsville3d').offsetWidth,
  y: document.getElementById('tootsville3d').offsetHeight,
  z: 1});
*/
