/* -*- js2 -*-*/

/**@license
 *
 * ./play/game/start.js is part of Tootsville
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

/**
 *
 */
window.onGoogleYoloLoad = function ()
{ Tootsville.Login.startSignIn ();
  Tootsville.Login.googleYoloSignIn (); };

/**
 * 
 */
Tootsville.bootstrap = function ()
{
    setTimeout (function () {
        document.getElementById ('control-panel-icon').style.width = '7vw';
    }, 1000);
    
    console.info ("\n\n\n This version of Tootsville is Build ID: @@BUILD@@\n ——————————————————————————————————————————————————————\n\n");

    window.addEventListener ('click', Tootsville.UI.onFirstClick);
    document.addEventListener ('fullscreenchange',
                               Tootsville.UI.setFullscreenFromNavigator);
    /* TODO detect navigator going offline */
    Tootsville.Game.hideWhenGameReady ();
    Tootsville.Game.stopSlowLoadingWatchdogs ();

    Tootsville.Login.start ();

    Tootsville.UI.setBackgroundMusic ("bensound-adventure");
    
    Tootsville.UI.HUD.initHUD ();
    setInterval (Tootsville.updateClock, 250);

    setInterval (Tootsville.Game.update, 33);

    document.addEventListener('keydown', Tootsville.UI.Keys.onKeyDown);

    Tootsville.Tank.prepareFor3D ();
    
    Tootsville.util.ensureServersReachable ();

};


window.addEventListener ('load', Tootsville.bootstrap);

