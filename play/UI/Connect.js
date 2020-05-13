/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/connect.js is part of Tootsville
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

(function () {
    var clickHandlers = {
        'control-panel-icon': (event) => { Tootsville.UI.HUD.toggleHUDPanel('control-panel');
                                           event.stopPropagation (); },
        'toolbox-mobile-phone': (event) => { Tootsville.UI.HUD.showHUDPanel('mobile');
                                             event.stopPropagation ();  },
        'paperdoll-mini': (event) => { Tootsville.UI.HUD.openPaperdoll ();
                                       event.stopPropagation (); },
        'troubleshooting-icon': (event) => { Tootsville.UI.HUD.toggleHUDPanel('troubleshooting');
                                             event.stopPropagation ();  },
        'talk-emoji-menu-close': event => {
            document.getElementById ('talk-emoji-menu').style.display = 'none';
        },
        'talk-loud-shout': event => {
            if (event.target.checked)
            { Tootsville.Game.Speech.loudness = 'shout'; } },
        'talk-loud-talk': event => {
            if (event.target.checked)
            { Tootsville.Game.Speech.loudness = 'talk'; } },
        'talk-loud-whisper': event => {
            if (event.target.checked)
            { Tootsville.Game.Speech.loudness = 'whisper'; } }
    };
    for (var id in clickHandlers) {
        document.getElementById(id).addEventListener('click', clickHandlers[id]);
    }

    document.getElementById('hud').addEventListener (
    'contextmenu', (event) => { Tootsville.UI.HUD.toggleHUDPanel('control-panel');
                                event.stopPropagation ();
                                event.preventDefault (); });
    
    let emojis = document.getElementsByClassName('emoji');
    for (let i = 0; i < emojis.length; i++)
    { emojis [ i ].addEventListener ('click', event => {Tootsville.UI.insertEmoji (event);}); };
})();




