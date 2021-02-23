/* -*- js2 -*- */

/**@license
 *
 * ./play/UI/Music.js is part of Tootsville
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

if (!('UI' in Tootsville)) { Tootsville.UI = {}; }

Tootsville.UI.endBackgroundMusic = function ()
{ let backgroundMusic = document.querySelector("#background-music");
  console.log ("Fading out previous song");
  setTimeout ( function () { backgroundMusic.volume = Tootsville.UI.Audio.currentVolume / 100 * .5; }, 333 );
  setTimeout ( function () { backgroundMusic.volume = Tootsville.UI.Audio.currentVolume / 100 * .25; }, 666 );
  setTimeout ( function () { backgroundMusic.volume = 0; }, 1000 );
  setTimeout ( function () { backgroundMusic.parentNode.removeChild (backgroundMusic); }, 1050 ); };

/**
 *
 */
Tootsville.UI.setMusicForArea = function (m)
{ const bgMusic = document.querySelector ('#background-music');
  if (bgMusic) Tootsville.UI.endBackgroundMusic ();
  const musicFooter = document.querySelector ('#music-footer');
  if (musicFooter) musicFooter.parentNode.removeChild (musicFooter);
  setTimeout (
      () => {
          console.log (`Starting music “${m.title}” by ${m.artist}`);
          let musicFooter = document.createElement('P');
          musicFooter.id = 'music-footer';
          musicFooter.style.opacity = 1;
          musicFooter.innerHTML = `
Music: <A TARGET="background-music" HREF="${m.link}">
<Q>${m.title}</Q> by ${m.artist}</A>`;
          let audio = document.createElement('AUDIO');
          audio.id = 'background-music';
          audio.loop = true;
          audio.autoplay = true;
          audio.volume = Tootsville.UI.Audio.currentVolume / 100;
          audio.innerHTML = `
<SOURCE SRC="https://jumbo.tootsville.org/Assets/Music/5/${m.file}.mp3">
<SOURCE SRC="https://jumbo.tootsville.org/Assets/Music/5/${m.file}.ogg">
<SOURCE SRC="https://jumbo.tootsville.org/Assets/Music/5/${m.file}.webm">`;
          document.getElementById ('hud').appendChild (musicFooter);
          document.getElementById ('hud').appendChild (audio);
          setTimeout ( function ()
                       { document.getElementById ("music-footer").style.opacity = 0;
                         setTimeout ( () => { musicFooter.style.display = 'none'; },
                                      150);},
                       10000 );
      },
      150 ); };

/**
 * 
*/
Tootsville.UI.setBackgroundMusic = function (song)
{ let backgroundMusic = document.querySelector("#background-music");
  if (backgroundMusic)
  { Tootsville.UI.endBackgroundMusic (); }
  let musicFooter = document.querySelector("#music-footer");
  if (musicFooter) { musicFooter.parentNode.removeChild (musicFooter); }
  console.log ("Scheduling to play " + song);
  setTimeout ( function ()
               { console.log ("Starting " + song);
                 var audio = document.createElement ("AUDIO");
                 audio.setAttribute ("ID", "background-music");
                 audio.autoplay = true;
                 audio.loop = true;
                 audio.volume = Tootsville.UI.Audio.currentVolume / 100;
                 var source = document.createElement ("SOURCE");
                 source.setAttribute ("SRC", "https://jumbo.tootsville.org/Assets/Music/5/" + song + ".mp3");
                 audio.appendChild (source);
                 var source2 = document.createElement ("SOURCE");
                 source2.setAttribute ("SRC", "https://jumbo.tootsville.org/Assets/Music/5/" + song + ".webm");
                 audio.appendChild (source2);
                 document.getElementById ("hud").appendChild (audio);
                 console.log (audio);
                 var musicFooter = document.createElement ("P");
                 musicFooter.setAttribute ("ID", "music-footer");
                 musicFooter.innerHTML = "Music: <A TARGET=\"background-music\" ID=\"background-music-link\"><Q ID=\"background-music-title\"></Q> by <SPAN ID=\"background-music-artist\"></SPAN></A>";
                 musicFooter.style.opacity = 0;
                 document.getElementById ("hud").appendChild (musicFooter);
                 fetch("https://jumbo.tootsville.org/Assets/Music/5/" + song + ".json").
                 then (function (response)
                       { return response.json (); }).
                 then (function (json) {
                     console.log ("Credits for song " + song, json);
                     if (json.artist)
                     { document.getElementById ("background-music-artist").innerText = json.artist; }
                     else
                     { document.getElementById ("background-music-artist").innerText = "(Unknown Artist)"; }
                     if (json.title)
                     { document.getElementById ("background-music-title").innerText = json.title; }
                     else
                     { document.getElementById ("background-music-title").innerText = "(" + song + ")"; }
                     if (json.link)
                     { document.getElementById ("background-music-link").href = json.link; }
                     musicFooter.style.opacity = 1;
                     setTimeout ( function ()
                                  { document.getElementById ("music-footer").style.opacity = 0;
                                    setTimeout ( () => { musicFooter.style.display = 'none'; },
                                               150);},
                                  10000 ); }); },
               1500 ); };
