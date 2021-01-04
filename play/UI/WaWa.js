/* -*- js2 -*- */

/**@license
 *
 * ./play/ui/hud.js is part of Tootsville
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
if (!('UI' in Tootsville)) { Tootsville.UI = { WaWa: {} }; }
if (!('WaWa' in Tootsville.UI)) { Tootsville.UI.WaWa = {}; }


/**
* Play a sound sample pitch-shifted by the speed difference given.
*/
Tootsville.UI.WaWa.playShifted = function (file, speed=1, after) {
    /*
          https://stackoverflow.com/a/49749868/475150
          https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/playbackRate
          https://github.com/mdn/webaudio-examples/tree/master/decode-audio-data
          https://www.w3schools.com/jsref/prop_audio_loop.asp
        */
    let audioCtx = new (window.AudioContext || window.webkitAudioContext)();
    let source = audioCtx.createBufferSource();
    let request = new XMLHttpRequest();
    request.open('GET', file, true);
    request.responseType = 'arraybuffer';
    request.onload = function() {
        var audioData = request.response;

        audioCtx.decodeAudioData(audioData, function(buffer) {
            let myBuffer = buffer;
            source.buffer = myBuffer;
            source.playbackRate.value = speed;
            source.connect(audioCtx.destination);
            source.loop = false;
        }, function(e) {
            console.warn ("Error with decoding audio data" + e.error); }); };

    request.send ();
    if (after) { source.addEventListener ('ended', after); }
    source.start ();
    return source; };

/**
 *
 */
Tootsville.UI.WaWa.playChained = function (chain, finish)
{ if ( (! chain) || (0 === chain.length) ) { return; }
  const [ sound, speed ] = chain [0];
  const rest = chain.slice (1);
  Tootsville.UI.WaWa.playShifted ('https://jumbo.tootsville.org/Assets/Voices/5/' + sound, speed, rest ? ( () => {
      Tootsville.UI.WaWa.playChained (rest, finish); }) : finish); };

/**
 *
 */
Tootsville.UI.WaWa.build = function (phrase, finish)
{ const clauses = phrase.split (/([?;,!.])/);
  const clausesLength = clauses.length;
  let chain = [];
  for (let i = 0; i < clausesLength; i += 2)
  { const words = clauses [i].split (' ');
    const ending = clauses [i + 1] || '.';
    const exclamationP = -1 < ending.indexOf ('!');
    const questionP = -1 < ending.indexOf ('?');
    const wordsLength = words.length;
    for (let j = 0; j < wordsLength; ++j)
    { const word = words [j];
      const wordLength = word.length;
      let duration = 0;
      for (let l = 0; l < wordLength; ++l)
      { if (-1 < "0123456789&".indexOf (word[l]))
        { duration += 5; }
        else if (-1 < "aeiouyw".indexOf (word[l]))
        { duration += 2; }
        else
        { duration += 1; } }
      if (duration > 0)
      { const speed = 9 / Math.min(duration, 9);
        if (j === wordsLength)
        { if (exclamationP)
          { chain [chain.length] = [ 'cc0-elephant-bellow-trimmed.webm', .67 * speed ]; }
          else if (questionP)
          { chain [chain.length] = [ 'cc0-elephant-bellow-trimmed.webm', 1.33333 * speed ]; }
          else
          { chain [chain.length] = [ 'cc0-elephant-trumpet-trimmed.webm', speed]; } }
        else
        {  chain [chain.length] = [ 'cc0-elephant-trumpet-trimmed.webm', speed]; } } } }
  Tootsville.UI.WaWa.playChained (chain, finish); };

/**
 *
 */
Tootsville.UI.WaWa.stop = function (source)
{ source.stop(0); };
