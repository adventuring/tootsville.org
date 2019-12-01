/* -*- js2 -*- */

if (!('Tootsville' in window)) { Tootsville = {}; }
if (!('ui' in Tootsville)) { Tootsville.UI = {}; }

Tootsville.UI.setBackgroundMusic = function (song)
{ var backgroundMusic = document.querySelector("#background-music");
  if (backgroundMusic)
  { console.log ("Fading out previous song");
    setTimeout ( function () { backgroundMusic.volume = .5; }, 333 );
    setTimeout ( function () { backgroundMusic.volume = .25; }, 666 );
    setTimeout ( function () { backgroundMusic.volume = 0; }, 1000 );
    setTimeout ( function () { backgroundMusic.parentNode.removeChild (backgroundMusic); }, 1050 ); }
  var musicFooter = document.querySelector("#music-footer");
  if (musicFooter) { musicFooter.parentNode.removeChild (musicFooter); }
  console.log ("Scheduling to play " + song);
  setTimeout ( function ()
               { console.log ("Starting " + song);
                 var audio = document.createElement ("AUDIO");
                 audio.setAttribute ("ID", "background-music");
                 audio.autoplay = true;
                 audio.loop = true;
                 audio.volume = 1;
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
                                  { document.getElementById ("music-footer").style.opacity = 0; },
                                  10000 ); }); },
               1500 ); };
