/*
 *
 * based  upon:  Gamepad API  Test  Written  in  2013 by  Ted  Mielczarek
 * <ted@mielczarek.org>
 *
 * To the  extent possible under  law, the author(s) have  dedicated all
 * copyright and related and neighboring  rights to this software to the
 * public  domain  worldwide.  This   software  is  distributed  without
 * any warranty.
 *
 * You should have  received a copy of the CC0  Public Domain Dedication
 * along      with       this      software.      If       not,      see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 *
 */

if (!('Tootsville' in window)) { Tootsville={ui:{gamepad:{}}}; }
if (!('ui' in Tootsville)) { Tootsville.ui={gamepad:{}}; }
if (!('gamepad' in Tootsville.ui)) { Tootsville.ui.gamepad={}; }

Tootsville.ui.gamepad.controllers = {};
Tootsville.ui.gamepad.controllerState = {};

Tootsville.ui.gamepad.connectHandler = function (ev)
{ Tootsville.ui.gamepad.addGamepad(ev.gamepad); };

Tootsville.ui.gamepad.addGamepad = function (gamepad)
{ Tootsville.ui.gamepad.controllers[gamepad.index] = gamepad;
  Tootsville.ui.gamepad.controllerState[gamepad.index] = {buttons:[], axes:[]};
  for (var i=0; i < gamepad.buttons.length; i++)
  { Tootsville.ui.gamepad.controllerState[gamepad.index].buttons[i] = null; }
  for (i=0; i<gamepad.axes.length; i++)
  { Tootsville.ui.gamepad.controllerState[gamepad.index].axes[i] = 0; }
  window.requestAnimationFrame(Tootsville.ui.gamepad.updateStatus); };

Tootsville.ui.gamepad.disconnectHandler = function (e)
{ Tootsville.ui.gamepad.removeGamepad(e.gamepad); };

Tootsville.ui.gamepad.removeGamepad = function (gamepad)
{ delete Tootsville.ui.gamepad.controllers[gamepad.index];
  delete Tootsville.ui.gamepad.controllerState[gamepad.index]; };

Tootsville.ui.gamepad.updateStatus = function ()
{ for (j in Tootsville.ui.gamepad.controllers)
  { var controller = Tootsville.ui.gamepad.controllers[j];
    for (var i=0; i<controller.buttons.length; i++)
    { var val = controller.buttons[i];
      if (typeof(val) == "object")
      { val = val.value; }
      if (Math.abs(Tootsville.ui.gamepad.controllerState[j].buttons[i] - val) > 0.01)
      { Tootsville.ui.gamepad.controllerState[j].buttons[i] = val;
        Tootsville.ui.gamepad.buttonEvent(j, i); }}
    for (var i=0; i<controller.axes.length; i++)
    { var val = controller.axes[i];
      if (Math.abs(Tootsville.ui.gamepad.controllerState[j].axes[i] - val) > 0.01)
      { Tootsville.ui.gamepad.controllerState[j].axes[i] = val;
        Tootsville.ui.gamepad.axisEvent(j, i); }}}
  window.requestAnimationFrame(Tootsville.ui.gamepad.updateStatus); };

Tootsville.ui.gamepad.scanGamepads = function ()
{ var gamepads = (navigator.getGamepads
                  ? navigator.getGamepads()
                  : (navigator.webkitGetGamepads
                     ? navigator.webkitGetGamepads()
                     : []));
  for (var i = 0; i < gamepads.length; i++)
  { if (gamepads[i])
    { if (!(gamepads[i].index in controllers))
      { Tootsville.ui.gamepad.addGamepad(gamepads[i]); }
      else
      { Tootsville.ui.gamepad.controllers[gamepads[i].index] = gamepads[i]; }}}};

(function ()
 { if ('GamepadEvent' in window)
   { window.addEventListener("gamepadconnected", Tootsville.ui.gamepad.connectHandler);
     window.addEventListener("gamepaddisconnected", Tootsville.ui.gamepad.disconnectHandler); }
   else if ('WebKitGamepadEvent' in window)
   { window.addEventListener("webkitgamepadconnected", Tootsville.ui.gamepad.connectHandler);
     window.addEventListener("webkitgamepaddisconnected", Tootsville.ui.gamepad.disconnectHandler); }
   else
   { setInterval(Tootsville.ui.gamepad.scanGamepads, 333); } })();
