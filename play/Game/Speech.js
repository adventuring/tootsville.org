/* -*- js2 -*- */

/**@license
 *
 * play/Game/Speech.js is part of Tootsville
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
 * Create a speech balloon
 */
Tootsville.Game.Speech.createBalloon = function (words, extraClass)
{ const balloon = document.createElement ('DIV');
  balloon.className = 'speech ' + (extraClass || '');
  balloon.innerText = words;
  return balloon; };

/**
 * Someone (maybe us) has spoken, so put up a speech balloon.
 */
Tootsville.Game.Speech.say = function (words, extraClass, speaker)
{ if (! speaker) { speaker = Tootsville.character;
                   if (!extraClass)
                   { extraClass = Tootsville.Game.Speech.loudness || 'talk'; } }
  if ((! words) || (0 == words.length)) { return; }
  let balloon = Tootsville.Game.Speech.createBalloon (words, extraClass);
  const avatar = Tootsville.Tank.avatars [ speaker ];
  if (! avatar)
  { console.warn ("Surprised to hear from " + speaker);
    Tootsville.Util.infinity ("finger", { talkie: speaker });
    return; }
  if (avatar.speech)
  { avatar.speech.parentNode.removeChild (avatar.speech); }
  avatar.speech = balloon;
  Tootsville.UI.WaWa.build (words, () => { balloon.parentNode.removeChild (balloon); } );
  Tootsville.UI.HUD.refreshSpeechAttachment (avatar.model, balloon);
  document.getElementById('hud').append (balloon); };


/**
 * The time has passed; remove a speech balloon.
 */
Tootsville.Game.Speech.removeSpeech = function (balloon)
{ if (balloon && balloon.parentNode)
  { balloon.parentNode.removeChild (balloon); } };

/**
 * Update speech baloons, expiring any that have aged out.
 */
Tootsville.Game.Speech.updateSpeech = function ()
{ if ( (!Tootsville.Tank.scene) || (!Tootsville.Tank.avatars) ) { return; }
  const avatars = Object.values(Tootsville.Tank.avatars);
  for (let i = 0; i < avatars.length; ++i)
  { let balloon = avatars [i].speech;
    if (balloon && balloon.endTime && Tootsville.Game.now > balloon.endTime)
    { Tootsville.Game.Speech.removeSpeech (balloon);
      delete avatars[i]['speech']; } } };

/**
 * Dispatch local ~ commands.
 *
 * @table @code
 * @item ~ua
 * Displays the user agent information
 * @item ~lag
 * Provides the user's estimated lag in msec
 * @end table
 */
Tootsville.Game.Speech.dispatchCommand = function (commandLine)
{ let words = commandLine.split (' ');
  switch (words[0])
  { case '~ua':
    Tootsville.Game.Speech.say ("My UA is " + navigator.userAgent, 'whisper');
    return;
    case "~ping":
    Tootsville.Util.infinity ("ping");
    return;
    case "~d20":
    let roll = (Math.floor (Math.random () * 20) + 1);
    Tootsville.UI.say ("I rolled a " + roll + " on a d20." + (roll == 1 ? " Critical failure!" : roll == 20 ? " Critical success!" : ""),
                       ( (roll == 1 || roll == 20) ? 'shout' : 'talk' ));
    break;;
    case "~credits":
    Tootsville.UI.confirmPretty("Credits",
                                "<p>The Tootsville V web application is by Bruce-Robert Pocock.</p><p>For server credits, try “,credits”.</p><p>For a random dice roll, try “~d20”.</p>", "O.K.");
    break;;
    case '~lag':
    Tootsville.Game.Speech.say ("My lag is " + Tootsville.Game.lag / 1000 + "s",
                                'whisper');
    return;
    default:
    Tootsville.Game.Speech.say ("I don't recognize " + words[0] + " as a magic word.",
                               'whisper'); }; };
