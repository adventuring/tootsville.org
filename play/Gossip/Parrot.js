/* -*- js2 -*-*/

/**@license
 *
 * play/Gossip/Parrot.js is part of Tootsville
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

if (!('Gossip' in Tootsville)) { Tootsville.Gossip = { Parrot: {} }; }
if (!("Parrot" in Tootsville.Gossip)) { Tootsville.Gossip.Parrot = {}; }

/**
 * Show or hide the parrot (based on reallyp flag).
 */
Tootsville.Gossip.Parrot.show = function (reallyp)
{ var parrot = document.getElementById('parrot');
  parrot.style.opacity = (reallyp ? 100 : 0);
  document.getElementById('parrot-image').style.left = (reallyp ? 0 : '-500px');
  if (reallyp)
  { parrot.style.display = 'block';
    parrot.style.zIndex = 2000;
    if (Tootsville.Gossip.Parrot.squawk)
    {Tootsville.Gossip.Parrot.squawk.play (); } }
  else
  { setTimeout(() => { parrot.style.display = 'none'; }, 1000 ); }};

/**
 *
 */
Tootsville.Gossip.Parrot.done = function ()
{ return new Promise
  ( (finish) => { Tootsville.Gossip.Parrot.show (false); finish (); } ); };

/**
 * Give a message from the Gossip Parrot with an OK button.
 */
Tootsville.Gossip.Parrot.say = function (title, message)
{ return new Promise
  ( (finish) =>
    { var reply = document.getElementById('parrot-reply');
      reply.style.display = 'none';
      reply.innerHTML = '';
      var buttonBox = document.createElement('DIV');
      buttonBox.setAttribute ('class', 'button-box');
      var okButton = document.createElement('BUTTON');
      okButton.innerText = ' O.K. ';
      okButton.onclick = () => { Tootsville.Gossip.Parrot.done().then(() => {finish ()}); };
      buttonBox.append (okButton);
      reply.append (buttonBox);
      reply.style.display = 'block';
      var speech = document.getElementById('parrot-speech');
      speech.style.display = 'none';
      speech.innerHTML = '<h2>' + title + '</h2>' + message;
      speech.style.display = 'block';
      Tootsville.Gossip.Parrot.show(true); });};

/**
 * Ask  a question from the Gossip Parrot with multiple replies possible.
 *
 * TODO:  Document  the  format  in   which  replies  are  submitted  to
 * this function.
 */
Tootsville.Gossip.Parrot.ask = function (title, message, replies)
{ return new Promise
  ( (finish) =>
    { var reply = document.getElementById ('parrot-reply');
      reply.style.display = 'none';
      reply.innerHTML = '';
      var buttonBox = document.createElement('DIV');
      buttonBox.setAttribute ('class', 'button-box');
      replies.forEach( (reply) =>
                       { var button = document.createElement ('BUTTON');
                         button.innerText = reply.text || reply.tag;
                         button.onclick = () => { Tootsville.Gossip.Parrot.done().then(function () {finish (reply.tag)}); };
                         buttonBox.append (button); });
      reply.append (buttonBox);
      reply.style.display = 'block';
      var speech = document.getElementById('parrot-speech');
      speech.style.display = 'none';
      speech.innerHTML = '<h2>' + title + '</h2>' + message;
      speech.style.display = 'block';
      Tootsville.Gossip.Parrot.show(true); }); };

/**
 * Ask a yes-or-no question from the Gossip Parrot.
 */
Tootsville.Gossip.Parrot.ynP = function (title, message)
{ return Tootsville.Gossip.Parrot.ask (title, message,
                                [  { tag: false, text: "No" },
                                   { tag: true, text: "Yes" } ]); };


// 
/**
 * Get the text which the Gossip Parrot should use to present an HTTP error.
 *
 * TODO: document the input format.
 */
Tootsville.Gossip.Parrot.parrotErrorText = function (body)
{ let code = body.error;
  let text = body.status || body.errorMessage;
  if (text)
  { text = text.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;"); }
  else
  { if (code)
    { text = "HTTP Error code " + code; }
    else
    { text = "The server did not respond with a recognizable error reply."; } }
  if (! code)
  { code = 500; }
  return "<P>The server reported an error.</P>" +
  "<P>" + text + "</P>" +
  "<P> <A TARGET=\"new\"" +
  " HREF=\"https://wiki.tootsville.org/wiki/Error_" + code + "\">Learn more ...</A></P>"; };
