/* -*- js2 -*-*/

/**@license
 *
 * play/Game/Start.js is part of Tootsville
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

/**
 *
 */
window.onGoogleYoloLoad = function ()
{ Tootsville.Login.startSignIn ();
  Tootsville.Login.googleYoloSignIn (); };

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }

/**
 * Pull up the game by its own bootstraps
 *
 * Start up the game systems, end the loader phase, and start up the login
 * process. This is the main "entry point" for the game.
 */
Tootsville.Game.bootstrap = function ()
{
    setTimeout (function () {
        document.getElementById ('control-panel-icon').style.width = '7vw';
    }, 1000);

    console.info
    (`\n\n
%cThis version of Tootsville is version: ${Tootsville.version} 
Build ID: @@BUILD@@%c\n\n\n`,
     `font-family: Acme, sans-serif; font-size: 2rem; color: navy; 
border: 1pt solid navy; margin: 3rem; padding: 1rem`,
                  '');

    window.addEventListener ('click', Tootsville.UI.onFirstClick);
    document.addEventListener ('fullscreenchange',
                               Tootsville.UI.setFullscreenFromNavigator);
    /* TODO detect navigator going offline */
    Tootsville.Game.hideWhenGameReady ();
    Tootsville.Game.stopSlowLoadingWatchdogs ();

    const keepWaitingDialog = document.querySelector('dialog#keep-waiting');
    if (keepWaitingDialog) keepWaitingDialog.parentNode.removeChild (keepWaitingDialog);

    Tootsville.Login.start ();

    let savedVolume = parseInt(window.localStorage.getItem('volume'));
    if (! isNaN(savedVolume))
        Tootsville.UI.Audio.currentVolume = savedVolume;
    else
        Tootsville.UI.Audio.currentVolume = 100;

    let savedHistory = window.localStorage.getItem('textHistory');
    if (savedHistory && 0 < savedHistory.length)
        Tootsville.UI.textHistory = savedHistory.split("\n");

    Tootsville.UI.setBackgroundMusic ("bensound-adventure");

    Tootsville.UI.HUD.initHUD ();
    setInterval (Tootsville.updateClock, 250);

    setInterval (Tootsville.Game.update, 1/50);

    document.addEventListener('keydown', Tootsville.UI.Keys.onKeyDown);

    Tootsville.Tank.prepareFor3D ();

    Tootsville.Util.ensureServersReachable ();

    Tootsville.UI.WaWa.prepareForWaWa ();

    if ('GamepadEvent' in window)
    { window.addEventListener ("gamepadconnected",
                               Tootsville.UI.Gamepad.connectHandler);
      window.addEventListener ("gamepaddisconnected",
                               Tootsville.UI.Gamepad.disconnectHandler); }
    else
        setInterval (Tootsville.UI.Gamepad.scanGamepads, 333);
};

window.addEventListener ('load', Tootsville.Game.bootstrap);

Tootsville.Game.warnUnload = function (event) {
    if (Tootsville.character) {
        event.preventDefault ();
        event.returnValue = "If you leave this page, you'll leave Tootsville";
        return "If you leave this page, you'll leave Tootsville"; }
    else return null; };

window.addEventListener ('beforeunload', Tootsville.Game.warnUnload);
