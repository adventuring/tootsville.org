/* -*- js2 -*- */

/*@license
 *
 * ./play/ui/ui.js is part of Tootsville
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
 */if (!('Tootsville' in window)) { window.Tootsville = {ui:{}}; }
if (!('ui' in Tootsville)) {Tootsville.ui={};}

Tootsville.ui.makeDivOrParagraph = function (text)
{ var element;
  if (text.indexOf('<') >= 0)
  { element = document.createElement('DIV'); }
  else
  { element = document.createElement('P'); }
  element.innerHTML = text;
  return element; };

Tootsville.ui.makeIDFromTitle = function (title)
{ return 'elt-' + title.replace(/[^a-zA-Z0-9]+/, '-'); };

Tootsville.ui.makePrettyDialog = function (title,text,accept,cancel,resolve)
{ var dialog = document.createElement('DIALOG');
  dialog.id = Tootsville.ui.makeIDFromTitle(title);
  var caption = document.createElement('H3');
  caption.innerHTML = title;
  dialog.appendChild(caption);
  dialog.appendChild(Tootsville.ui.makeDivOrParagraph(text));
  var buttons = document.createElement('DIV');
  buttons.className = 'button-box';
  var cancelButton = document.createElement('BUTTON');
  cancelButton.className = 'cancel-button';
  cancelButton.innerHTML = cancel;
  cancelButton.addEventListener('click', ()=> {
      hud.removeChild(dialog);
      resolve(false);
  });
  buttons.appendChild(cancelButton);
  var acceptButton = document.createElement('BUTTON');
  acceptButton.className = 'accept-button';
  acceptButton.innerHTML = accept;
  acceptButton.addEventListener('click', () => {
      hud.removeChild(dialog);
      resolve(true);
  });
  buttons.appendChild(acceptButton);
  dialog.appendChild(buttons);
  return dialog; };

Tootsville.ui.confirmPretty = function (title,text,accept)
{ var hud = document.getElementById('hud');
  return new Promise(resolve => {
      var cancel = 'Cancel';
      if ('Yes' == accept) { cancel = 'No'; }
      hud.appendChild(Tootsville.ui.makePrettyDialog(title,text,accept,cancel,resolve)); }); };

Tootsville.ui.forceQuit = function ()
{ Tootsville.login.quit(); };

Tootsville.ui.quit = function () { Tootsville.ui.signOut (); }

Tootsville.ui.signOut = function ()
{ Tootsville.ui.confirmPretty(
    "Quit?",
    "Are you sure you want to quit playing now?",
    "Quit").then
  ( reallyP =>
    { if (reallyP)
      { Tootsville.login.quit(); }}); };

Tootsville.ui.setFullscreenFromNavigator = function ()
{ var fullscreenCheck = document.getElementById('fullscreen-toggle');
  if (fullscreenCheck)
  { fullscreenCheck.checked = !! (document.fullscreenElement); }};

Tootsville.ui.setFullscreen = function (really)
{ if (really)
  { document.documentElement.requestFullscreen(); }
  else
  { document.exitFullscreen(); }};

Tootsville.ui.toggleFullscreen = function ()
{ Tootsville.ui.setFullscreen( ! (document.fullscreenElement) ); };

Tootsville.ui.onFirstClick = function ()
{ window.removeEventListener('click', Tootsville.ui.onFirstClick);
  document.addEventListener('beforeunload', (ev) => {
      var confirmationMessage = "If you leave or reload this page, you'll quit the game.";
      ev.returnValue = confirmationMessage;  /* Gecko, Trident, Chrome 34+ */
      return confirmationMessage; /* Gecko, WebKit, Chrome <34 */ }); };

Tootsville.ui.slowLoadingWatchdog = function ()
{ Tootsville.ui.confirmPretty
  ( "Slow Loading?",
    ( "Loading seems to be taking a long time. You might have trouble " +
      "playing the game if your connection is very bad. Do you want to " +
      "wait and see if it improves?" ),
    "Keep Waiting", "Read Troubleshooting Tips").then
  ( waitP =>
    { if (! waitP)
      { document.location = 'https://wiki.tootsville.org/wikiki/PlayNotLoading'; } }); };
