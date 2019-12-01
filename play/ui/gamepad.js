/* -*- js2 -*-*/

/*@license
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

if (! ('Tootsville' in window)) { Tootsville={ui:{gamepad:{}}}; }
if (! ('ui' in Tootsville)) { Tootsville.UI={gamepad:{}}; }
if (! ('gamepad' in Tootsville.UI)) { Tootsville.UI.gamepad={}; }

/**
 * All connected gamepad controllers.
 */
Tootsville.UI.gamepad.controllers = {};
/**
 * Gamepad controller state data.
 *
 * TODO, document format
 */
Tootsville.UI.gamepad.controllerState = {};

/**
 * Event handler for gamepad connections
 */
Tootsville.UI.gamepad.connectHandler = function (ev)
{ Tootsville.UI.gamepad.addGamepad (ev.gamepad); };

/**
 * Add a gamepad and initialize state data.
 */
Tootsville.UI.gamepad.addGamepad = function (gamepad)
{ Tootsville.UI.gamepad.controllers [gamepad.index] = gamepad;
  Tootsville.UI.gamepad.controllerState [gamepad.index] = {buttons: [], axes: []};
  for (var i=0; i < gamepad.buttons.length; i++)
  { Tootsville.UI.gamepad.controllerState [gamepad.index].buttons [i] = null; }
  for (i=0; i<gamepad.axes.length; i++)
  { Tootsville.UI.gamepad.controllerState [gamepad.index].axes [i] = 0; }
  Tootsville.trace ("New gamepad detected", gamepad);
  window.requestAnimationFrame (Tootsville.UI.gamepad.updateStatus); };

/**
 * Event handler for gamepad disconnections.
 */
Tootsville.UI.gamepad.disconnectHandler = function (e)
{ Tootsville.UI.gamepad.removeGamepad (e.gamepad); };

/**
 * Remove a gamepad from the active state.
 */
Tootsville.UI.gamepad.removeGamepad = function (gamepad)
{ delete Tootsville.UI.gamepad.controllers [gamepad.index];
  delete Tootsville.UI.gamepad.controllerState [gamepad.index]; };

/**
 * Update gamepad status.
 */
Tootsville.UI.gamepad.updateStatus = function ()
{ for (j in Tootsville.UI.gamepad.controllers)
  { var controller = Tootsville.UI.gamepad.controllers [j];
    for (var i=0; i<controller.buttons.length; i++)
    { var val = controller.buttons [i];
      if (typeof (val) == "object")
      { val = val.value; }
      if (Math.abs (Tootsville.UI.gamepad.controllerState [j].buttons [i] - val) > 0.01)
      { Tootsville.UI.gamepad.controllerState [j].buttons [i] = val;
        Tootsville.UI.gamepad.buttonEvent (j, i); }}
    for (var i = 0; i < controller.axes.length; i ++)
    { var val = controller.axes [i];
      if (Math.abs (Tootsville.UI.gamepad.controllerState [j].axes [i] - val) > 0.01)
      { Tootsville.UI.gamepad.controllerState [j].axes [i] = val;
        Tootsville.UI.gamepad.axisEvent (j, i); }}}
  window.requestAnimationFrame (Tootsville.UI.gamepad.updateStatus); };

/**
 * Scan gamepads for updates
 */
Tootsville.UI.gamepad.scanGamepads = function ()
{ var gamepads = (navigator.getGamepads
                  ? navigator.getGamepads ()
                  : (navigator.webkitGetGamepads
                     ? navigator.webkitGetGamepads ()
                     : []));
  for (var i = 0; i < gamepads.length; i++)
  { if (gamepads [i])
    { if (! (gamepads [i].index in controllers))
      { Tootsville.UI.gamepad.addGamepad (gamepads [i]); }
      else
      { Tootsville.UI.gamepad.controllers [gamepads [i].index] = gamepads [i]; }}}};

(function ()
 { if ('GamepadEvent' in window)
   { window.addEventListener ("gamepadconnected", Tootsville.UI.gamepad.connectHandler);
     window.addEventListener ("gamepaddisconnected", Tootsville.UI.gamepad.disconnectHandler); }
   else if ('WebKitGamepadEvent' in window)
   { window.addEventListener ("webkitgamepadconnected", Tootsville.UI.gamepad.connectHandler);
     window.addEventListener ("webkitgamepaddisconnected", Tootsville.UI.gamepad.disconnectHandler); }
   else
   { setInterval (Tootsville.UI.gamepad.scanGamepads, 333); } }) ();
