/* -*- js2 -*-*/

/*@license
 *
 * ./play/ui/audio-control.js is part of Tootsville
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

if (!('UI' in Tootsville)) { Tootsville.UI = { Audio: {} }; }
if (!('Audio' in Tootsville.UI)) { Tootsville.UI.Audio = {}; }

/**
 * Raise the volume by 10%, up to a maximum of 100%.
 */
Tootsville.UI.Audio.volumeUp = function ()
{ Tootsville.UI.Audio.setVolume(Math.min(100, 10 + Tootsville.UI.Audio.currentVolume));};

/**
 * Lower the volume by 10%, down to a minumum of 0.
 */
Tootsville.UI.Audio.volumeDown = function () 
{ Tootsville.UI.Audio.setVolume(Math.max(0, (Tootsville.UI.Audio.currentVolume - 10)));};

/**
 * Temporarily mute or unmute the volume.
 */
Tootsville.UI.Audio.volumeMute = function () 
{ if (Tootsville.UI.Audio.currentVolume < 9) 
  { Tootsville.UI.Audio.setVolume(Tootsville.UI.Audio.savedVolume); }
  else 
  { Tootsville.UI.Audio.savedVolume = Tootsville.UI.Audio.currentVolume;
    Tootsville.UI.Audio.setVolume(0); } };

/**
 * Update the volume controls UI in the control panel menu
 */
Tootsville.UI.Audio.updateVolumeUI = function ()
{ Tootsville.UI.Audio.updateVolumeSlider();
  Tootsville.UI.Audio.updateVolumeMuteIcon(); };

/**
 * Set the volume to ``newVolume''%
 */
Tootsville.UI.Audio.setVolume = function (newVolume) 
{ Tootsville.UI.Audio.currentVolume = newVolume;
  Tootsville.UI.Audio.gainNode.gain.setValueAtTime(0, Tootsville.UI.Audio.context.currentTime);
  Tootsville.UI.Audio.updateVolumeUI(); };

/**
 * Update the volume slider in the control panel.
 */
Tootsville.UI.Audio.updateVolumeSlider = function() 
{ var slider = document.getElementById('volume-slider');
  if (slider) 
  { slider.value = Tootsville.UI.Audio.currentVolume;
    slider.disabled = false; } };

/**
 * Update the volume Mute indicator in the control panel
 */
Tootsville.UI.Audio.updateVolumeMuteIcon = function() 
{ var muteIcon = document.getElementById('mute-icon');
  if (Tootsville.UI.Audio.currentVolume < 9) 
  { muteIcon.style.color = 'red'; } else 
  { muteIcon.style.color = 'black'; } };

/**
 *
 */
Tootsville.UI.Audio.context = new (window.AudioContext || window.webkitAudioContext)();

/**
 *
 */
Tootsville.UI.Audio.gainNode = Tootsville.UI.Audio.context.createGain();

// if (navigator.mediaDevices.getUserMedia) 
// { navigator.mediaDevices.getUserMedia (
//     // constraints - only audio needed for this app
//     { audio: true },
//     // Success callback
//     stream =>
//         { // var source = audioCtx.createMediaStreamSource(stream);
//           // source.connect(Tootsville.UI.Audio.gainNode);
//           // Tootsville.UI.Audio.gainNode.connect(Tootsville.UI.Audio.context.destination);
//         },
//     // Error callback
//     err =>
//         { Tootsville.error('getUserMedia error occured: ' + err); }
// ); } else 
// { Tootsville.warn('getUserMedia not supported on your browser!');
//   // XXX hide volume controls
// }

