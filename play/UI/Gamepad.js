/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/gamepad.js is part of Tootsville
 *
 * Copyright  © 2018,2019  The Corporation  for Inter-World  Tourism and
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
Tootsville.UI.Gamepad.forwardPoint = function (amount)
{ const horizon = new BABYLON.Vector3 (Math.cos (Tootsville.Tank.playerAvatar ().facing) * amount * 500,
                                       0,
                                       Math.sin (Tootsville.Tank.playerAvatar ().facing) * amount * 500);
  // console.debug ("horizon", horizon);
  return horizon; };

/**
*
*/
Tootsville.UI.Gamepad.axisUpdate = function (controllerIndex)
{ Tootsville.Tank.playerAvatar ().facing += (Tootsville.UI.Gamepad.controllerState [controllerIndex].axes [0] *
                                             Tootsville.UI.Gamepad.ROTATION_SPEED);
  Tootsville.Game.Nav.walkTheLine (Tootsville.Tank.playerAvatar (),
                                   Tootsville.UI.Gamepad.forwardPoint (
                                       Tootsville.UI.Gamepad.controllerState [controllerIndex].axes [1])); };

/**
*
*/
Tootsville.UI.Gamepad.buttonEvent = function (controllerIndex, buttonIndex)
{ console.log ("button event on controller " + controllerIndex + " button " + buttonIndex); };

/**
 * Update gamepad status.
 */
Tootsville.UI.Gamepad.updateStatus = function ()
{ const gamepads = navigator.getGamepads();
  if (! gamepads) { return; }
  for (let j = 0; j < gamepads.length; ++j)
  { let controller = gamepads[j];
    if (controller)
    { if (! Tootsville.UI.Gamepad.controllerState [j])
      { Tootsville.UI.Gamepad.controllerState [j] = { buttons: [], axes: [] }; }
    for (let i = 0; i < controller.buttons.length; ++i)
    { let val = controller.buttons [i];
      if (typeof (val) == "object")
      { val = val.value; }
      if (Math.abs (Tootsville.UI.Gamepad.controllerState [j].buttons [i] - val) > 0.01)
      { Tootsville.UI.Gamepad.controllerState [j].buttons [i] = val;
        Tootsville.UI.Gamepad.buttonEvent (j, i); }}
    for (let i = 0; i < controller.axes.length; i ++)
    { let val = controller.axes [i];
      if (Math.abs (Tootsville.UI.Gamepad.controllerState [j].axes [i] - val) > 0.01)
      { Tootsville.UI.Gamepad.controllerState [j].axes [i] = val; }}
      Tootsville.UI.Gamepad.axisUpdate (j); }}};

/**
 * Scan gamepads for updates
 */
Tootsville.UI.Gamepad.scanGamepads = function ()
{ var gamepads = (navigator.getGamepads
                  ? navigator.getGamepads ()
                  : (navigator.webkitGetGamepads
                     ? navigator.webkitGetGamepads ()
                     : []));
  for (var i = 0; i < gamepads.length; i++)
  { if (gamepads [i])
    { if (! (gamepads [i].index in controllers))
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
