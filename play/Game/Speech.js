/* -*- js2 -*- */

/**@license
 *
 * ./play/game/Speech.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {Speech: {}}; }
if (!('Speech' in Tootsville.Game)) { Tootsville.Game.Speech = {}; }

/**
 *
 */
Tootsville.Game.Speech.say = function (words, extraClass)
{ const balloon = document.createElement ('DIV');
  balloon.className = 'speech ' + (extraClass || '');
  balloon.innerText = words;
  balloon.endTime = Tootsville.Game.now + 5000 + words.length * 100;
  const avatar = Tootsville.Tank.scene.avatars [Tootsville.character.name];
  avatar.speech = balloon;
  Tootsville.UI.HUD.refreshSpeechAttachment (avatar.model, balloon);
  document.getElementById('hud').append (balloon); };

Tootsville.Game.Speech.removeSpeech = function (balloon)
{ balloon.parentNode.removeChild (balloon); };

Tootsville.Game.Speech.updateSpeech = function ()
{ if ( (!Tootsville.Tank.scene) || (!Tootsville.Tank.scene.avatars) ) { return; }
  const avatars = Object.values(Tootsville.Tank.scene.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { let balloon = avatars [i].speech;
    if (balloon && Tootsville.Game.now > balloon.endTime)
    { Tootsville.Game.Speech.removeSpeech (balloon);
      delete avatars[i]['speech']; } } };
