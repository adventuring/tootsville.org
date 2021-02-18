/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/gamepad.js is part of Tootsville
 *
 * Copyright  © 2018-2021  The Corporation  for Inter-World  Tourism and
 * Adventuring (ciwta.org).
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
 *
 *
 * based  upon: Gamepad  API  Test  Written in  2013  by Ted  Mielczarek
 * <ted@mielczarek.org>
 *
 * To the  extent possible under law,  the author (s) had  dedicated all
 * copyright and related and neighboring  rights to that software to the
 * public  domain  worldwide.  That  software  was  distributed  without
 * any warranty.
 *
 */

if (! ('Tootsville' in window)) { Tootsville={UI:{Gamepad:{}}}; }
if (! ('UI' in Tootsville)) { Tootsville.UI={Gamepad:{}}; }
if (! ('Gamepad' in Tootsville.UI)) { Tootsville.UI.Gamepad={}; }

/**
 * All connected gamepad controllers.
 */
Tootsville.UI.Gamepad.controllers = {};
/**
 * Gamepad controller state data.
 *
 * TODO, document format
 */
Tootsville.UI.Gamepad.controllerState = {};

/**
 * Event handler for gamepad connections
 */
Tootsville.UI.Gamepad.connectHandler = function (ev)
{ Tootsville.UI.Gamepad.addGamepad (ev.gamepad); };

/**
 * Add a gamepad and initialize state data.
 */
Tootsville.UI.Gamepad.addGamepad = function (gamepad)
{ Tootsville.UI.Gamepad.controllers [gamepad.index] = gamepad;
  Tootsville.UI.Gamepad.controllerState [gamepad.index] = {buttons: [], axes: []};
  for (var i = 0; i < gamepad.buttons.length; ++i)
  { Tootsville.UI.Gamepad.controllerState [gamepad.index].buttons [i] = null; }
  for (i = 0; i < gamepad.axes.length; ++i)
  { Tootsville.UI.Gamepad.controllerState [gamepad.index].axes [i] = 0; }
  Tootsville.trace ("New gamepad detected", gamepad); };

/**
 * Event handler for gamepad disconnections.
 */
Tootsville.UI.Gamepad.disconnectHandler = function (e)
{ Tootsville.UI.Gamepad.removeGamepad (e.gamepad); };

/**
 * Remove a gamepad from the active state.
 */
Tootsville.UI.Gamepad.removeGamepad = function (gamepad)
{ delete Tootsville.UI.Gamepad.controllers [gamepad.index];
  delete Tootsville.UI.Gamepad.controllerState [gamepad.index]; };

/**
 *
 */
Tootsville.UI.Gamepad.ROTATION_SPEED = .05;

/**
 *
 */
Tootsville.UI.Gamepad.axisUpdate = function (controllerIndex)
{ const leftRight = Tootsville.UI.Gamepad.controllerState [controllerIndex].axes [0];
  const forwardBack = - Tootsville.UI.Gamepad.controllerState [controllerIndex].axes [1];
  if (Math.abs (leftRight) > .1 || Math.abs (forwardBack) > .1)
  { Tootsville.Tank.playerAvatar ().facing = Math.atan2 (leftRight, forwardBack) - Math.PI / 2;
    if (Tootsville.Tank.playerAvatar ().facing < 0)
    { Tootsville.Tank.playerAvatar ().facing += 2 * Math.PI; }
    Tootsville.Game.Nav.walkTheLine (Tootsville.Tank.playerAvatar (),
                                     new BABYLON.Vector3 (
                                         Tootsville.Tank.playerAvatar ().model.position.x + leftRight * 1000,
                                         0,
                                         Tootsville.Tank.playerAvatar ().model.position.z + forwardBack * 1000));
    Tootsville.Game.Nav.gamepadMovementP = true; }
  else if (Tootsville.Game.Nav.gamepadMovementP)
  { Tootsville.Tank.playerAvatar ().course = null; } };

/**
 *
 */
Tootsville.UI.Gamepad.buttonEvent = function (controllerIndex, buttonIndex, value)
{ if (value > .5)
  { switch (buttonIndex)
    { case 0: // Y
      Tootsville.UI.interact (null);
      break;

      case 1: // B
      Tootsville.UI.useActiveItem (null);
      break;

      case 2: // A
      Tootsville.UI.HUD.switchActiveItem ();
      break;

      case 3: // X
      Tootsville.UI.HUD.toggleTalkBox ();
      break;

      case 8: // -
      Tootsville.UI.HUD.toggleHUDPanel ('mobile');
      break;

      case 9: // +
      Tootsville.UI.HUD.openPaperdoll ();
      break;

      case 10: // L stick click
      Tootsville.Tank.playerAvatar ().facing = Math.PI;
      break;

      default:
      console.debug ("Unmapped button ", buttonIndex); }; } };

/**
 * Update gamepad status.
 */
Tootsville.UI.Gamepad.updateStatus = function ()
{const gamepads = (navigator.getGamepads
                   ? navigator.getGamepads ()
                   : (navigator.webkitGetGamepads
                      ? navigator.webkitGetGamepads ()
                      : []));
 if (! gamepads) { return; }
 for (let j = 0; j < gamepads.length; ++j)
 { let controller = gamepads[j];
   if (controller)
   { if (! Tootsville.UI.Gamepad.controllerState [j])
     { Tootsville.UI.Gamepad.controllerState [j] = { buttons: [], axes: [] }; }
     for (let i = 0; i < controller.buttons.length; ++i)
     { let val = controller.buttons [i];
       if ('object' === typeof (val))
       { val = val.value; }
       if (Math.abs (Tootsville.UI.Gamepad.controllerState [j].buttons [i] - val) > 0.01)
       { Tootsville.UI.Gamepad.controllerState [j].buttons [i] = val;
         Tootsville.UI.Gamepad.buttonEvent (j, i, val); }}
     for (let i = 0; i < controller.axes.length; i ++)
     { let val = controller.axes [i];
       if (Math.abs (Tootsville.UI.Gamepad.controllerState [j].axes [i] - val) > 0.01)
       { Tootsville.UI.Gamepad.controllerState [j].axes [i] = val;
         Tootsville.UI.Gamepad.axisUpdate (j); }}}}};

/**
 * Scan gamepads for updates
 */
Tootsville.UI.Gamepad.scanGamepads = function ()
{ const gamepads = (navigator.getGamepads
                    ? navigator.getGamepads ()
                    : (navigator.webkitGetGamepads
                       ? navigator.webkitGetGamepads ()
                       : []));
  for (var i = 0; i < gamepads.length; i++)
  { if (gamepads [i])
    { if (! (gamepads [i].index in Tootsville.UI.Gamepad.controllers))
      { Tootsville.UI.Gamepad.addGamepad (gamepads [i]); }
      else
      { Tootsville.UI.Gamepad.controllers [gamepads [i].index] = gamepads [i]; }}}};

(function ()
 { if ('GamepadEvent' in window)
   { window.addEventListener ("gamepadconnected", Tootsville.UI.Gamepad.connectHandler);
     window.addEventListener ("gamepaddisconnected", Tootsville.UI.Gamepad.disconnectHandler); }
   else if ('WebKitGamepadEvent' in window)
   { window.addEventListener ("webkitgamepadconnected", Tootsville.UI.Gamepad.connectHandler);
     window.addEventListener ("webkitgamepaddisconnected", Tootsville.UI.Gamepad.disconnectHandler); }
   else
   { setInterval (Tootsville.UI.Gamepad.scanGamepads, 333); } }) ();
