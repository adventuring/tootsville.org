/* -*- js2 -*-*/

/*@license
 *
 * ./play/ui/audio-control.js is part of Tootsville
 *
 * Copyright   ©  2016,2017   Bruce-Robert  Pocock;   ©  2018,2019   The
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
 */Tootsville.audio = {
     currentVolume: 100,
     savedVolume: 100,
     volumeUp: function() {
         setVolume(Math.min(100, 10 + Tootsville.audio.currentVolume));
     },
     volumeDown: function() {
         setVolume(Math.max(0, (Tootsville.audio.currentVolume - 10)));
     },

     volumeMute: function() {
         if (Tootsville.audio.currentVolume < 9) {
             Tootsville.audio.setVolume(Tootsville.audio.savedVolume);
         } else {
             Tootsville.audio.savedVolume = Tootsville.audio.currentVolume;
             Tootsville.audio.setVolume(0);
         }
     },

     updateVolumeUI: function() {
         Tootsville.audio.updateVolumeSlider();
         Tootsville.audio.updateVolumeMuteIcon();
     },

     setVolume: function(newVolume) {
         Tootsville.audio.currentVolume = newVolume;
         // TODO: apply to sound system;
         Tootsville.audio.updateVolumeUI();
     },

     updateVolumeSlider: function() {
         var slider = document.getElementById('volume-slider');
         if (slider) {
             slider.value = Tootsville.audio.currentVolume;
             slider.disabled = false;
         }
     },

     updateVolumeMuteIcon: function() {
         var muteIcon = document.getElementById('mute-icon');
         if (Tootsville.audio.currentVolume < 9) {
             muteIcon.style.color = 'red';
         } else {
             muteIcon.style.color = 'black';
         }
     }

 };
