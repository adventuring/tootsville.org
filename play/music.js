/* -*- js2 -*- */

if (!('Tootsville' in window)) { Tootsville = {}; }
if (!('ui' in Tootsville)) { Tootsville.ui = {}; }

Tootsville.ui.setBackgroundMusic = function (song)
{ var backgroundMusic = document.querySelector("#background-music");
  if (backgroundMusic) // should always be found
  {  setTimeout ( function () { backgroundMusic.volume = .5; }, 333 );
     setTimeout ( function () { backgroundMusic.volume = .25; }, 666 );
     setTimeout ( function () { backgroundMusic.volume = 0; }, 1000 );
     setTimeout ( function () { backgroundMusic.parentNode.removeChild (backgroundMusic); }, 1050 );
     setTimeout ( function ()
                  { var audio = document.createElement ("AUDIO");
                    audio.setAttribute ("ID", "background-music");
                    audio.autoplay = true;
                    audio.loop = true;
                    audio.volume = 1;
                    var source = document.createElement ("SOURCE");
                    source.setAttribute ("SRC", "https://jumbo.tootsville.org/Assets/Music/5/" + song + ".mp3");
                    audio.appendChild (source);
                    document.getElementById ("hud").appendChild (audio);
                    console.log (audio);
                    var musicFooter = document.createElement ("P");
                    musicFooter.setAttribute ("ID", "music-footer");
                    musicFooter.innerHTML = "Music: <A TARGET=\"background-music\" ID=\"background-music-link\"><I ID=\"background-music-title\"></I> by <SPAN ID=\"background-music-artist\"></SPAN></A>";
                    musicFooter.opacity = 0;
                    document.getElementById ("hud").appendChild (musicFooter);
                    fetch("https://jumbo.tootsville.org/Assets/Music/5/" + song + ".json")
                    .then(function(response) {
                        return response.json();
                    }).then(function(json) {
                        if (json.artist)
                        { document.getElementByID ("background-music-artist").innerText = json.artist; }
                        else
                        { document.getElementByID ("background-music-artist").innerText = "(Unknown Artist)"; }
                        if (json.title)
                        { document.getElementByID ("background-music-title").innerText = json.title; }
                        else
                        { document.getElementByID ("background-music-title").innerText = "(" + song + ")"; }
                        if (json.link)
                        { document.getElementByID ("background-music-artist").href = json.link; }

                        setTimeout ( function ()
                                     { document.getElementById ("music-footer").opacity = 0; },
                                     10000 ); }
                    });
                  },
                  1500 );
 };
