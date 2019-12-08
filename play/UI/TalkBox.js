/* -*- js2 -*- */
if (!('UI' in Tootsville)) { Tootsville.UI = {}; }

/**
 *
 */
Tootsville.UI.beginSelectLoudness = function ()
{}

/**
 *
 */
Tootsville.UI.endSelectLoudness = function ()
{}

/**
 *
 */
Tootsville.UI.beginSelectExpression = function ()
{}

/**
 *
 */
Tootsville.UI.endSelectExpression = function ()
{}

/**
 *
 */
Tootsville.UI.beginSelectEmoji = function ()
{}

/**
 *
 */
Tootsville.UI.endSelectEmoji = function ()
{}

/**
 *
 */
Tootsville.UI.talkSpeakKeyUp = function (ev)
{}

/**
 *
 */
Tootsville.UI.talkSpeakKeyDown = function (ev)
{}

/**
 *
 */
Tootsville.UI.talkSpeakSend = function ()
{}

window.getElementById("talk-loud-selector").addEventListener('click', Tootsville.UI.beginSelectLoudness);
