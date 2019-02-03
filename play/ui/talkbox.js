/* -*- js2 -*- */
if (!('Tootsville' in window)) { Tootsville = {}; }
if (!('ui' in Tootsville)) { Tootsville.ui = {}; }

Tootsville.ui.beginSelectLoudness = function ()
{}

Tootsville.ui.endSelectLoudness = function ()
{}

Tootsville.ui.beginSelectExpression = function ()
{}

Tootsville.ui.endSelectExpression = function ()
{}

Tootsville.ui.beginSelectEmoji = function ()
{}

Tootsville.ui.endSelectEmoji = function ()
{}

Tootsville.ui.talkSpeakKeyUp = function (ev)
{}

Tootsville.ui.talkSpeakKeyDown = function (ev)
{}

Tootsville.ui.talkSpeakSend = function ()
{}

window.getElementById("talk-loud-selector").addEventListener('click', Tootsville.ui.beginSelectLoudness);
