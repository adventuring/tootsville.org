/* -*- js2 -*- */

/**@license
 *
 * ./play/ui/ui.js is part of Tootsville
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

if (!('UI' in Tootsville)) {Tootsville.UI={};}

/**
 * Turns a  string without  HTML into a  paragraph, one  containing HTML
 * markup into a DIV.
 */
Tootsville.UI.makeDivOrParagraph = function (text)
{ let element;
  if (text.indexOf('<') >= 0)
  { element = document.createElement('DIV'); }
  else
  { element = document.createElement('P'); }
  element.innerHTML = text;
  return element; };

/** */
Tootsville.UI.makeIDFromTitle = function (title)
{ return 'elt-' + title.replace(/[^a-zA-Z0-9]+/, '-'); };

/**
 * Make  a basic  dialog  box  with a  title,  text,  accept and  cancel
 * buttons, and call resolve function with user input later.
 */
Tootsville.UI.makePrettyDialog = function (title,text,accept,cancel,resolve)
{ let dialog = document.createElement ('DIALOG');
  dialog.id = Tootsville.UI.makeIDFromTitle (title);
  let caption = document.createElement ('H3');
  caption.innerHTML = title;
  dialog.appendChild (caption);
  dialog.appendChild (Tootsville.UI.makeDivOrParagraph (text));
  let buttons = document.createElement ('DIV');
  buttons.className = 'button-box';
  let cancelButton = document.createElement ('BUTTON');
  cancelButton.className = 'cancel-button';
  cancelButton.innerHTML = cancel;
  cancelButton.addEventListener('click', ()=> {
      const hud = document.getElementById ('hud');
      hud.removeChild(dialog);
      resolve(false); });
  buttons.appendChild(cancelButton);
  let acceptButton = document.createElement ('BUTTON');
  acceptButton.className = 'accept-button';
  acceptButton.innerHTML = accept;
  acceptButton.addEventListener('click', () => {
      document.getElementById("hud").removeChild(dialog);
      resolve(true); });
  buttons.appendChild (acceptButton);
  dialog.appendChild (buttons);
  return dialog; };

/**
* WRITEME
*/
Tootsville.UI.makePrompt = function (prompt, resolve)
{ let dialog = document.createElement ("DIALOG");
  dialog.id = "prompt-" + prompt.id;
  let caption  = document.createElement ("H3");
  let close = document.createElement ('SPAN');
  close.innerText = "□";
  close.style.float = 'left';
  close.addEventListener ('click', () => {
        document.getElementById("hud").removeChild (dialog);
        resolve ('close'); } );
  caption.appendChild (close);
  caption.innerHTML += prompt.title;
  dialog.appendChild (caption);
  dialog.appendChild (Tootsville.UI.makeDivOrParagraph (prompt.msg));
  let buttons = document.createElement ('DIV');
  buttons.className = 'button-box';
  for (let reply in prompt.replies)
  { let desc = prompt.replies [ reply ];
    let button = document.createElement ("BUTTON");
    button.className = ( ('aff' === desc.type) ?
                         "accept-button" :
                         ('neg' === desc.type) ?
                         "cancel-button" : "" );
    button.innerHTML = desc.label;
    button.addEventListener ('click', () => {
        document.getElementById("hud").removeChild (dialog);
        resolve (reply); } );
    buttons.appendChild (button);
  }
  dialog.appendChild (buttons);
  document.getElementById("hud").appendChild (dialog); };

/**
 * Present a nice UI box to confirm whether to do something or not.
 *
 * The title and text are displayed.  The ``accept'' text is displayed on
 * one  button;  the negative  button  will  read ``Cancel''  unless  the
 * ``accept'' text reads ``Yes,'' in which case it will read ``No.''
 */
Tootsville.UI.confirmPretty = function (title,text,accept,cancel='Cancel')
{ let hud = document.getElementById('hud');
  return new Promise(resolve => {
      if ('Yes' === accept && 'Cancel' === cancel) cancel = 'No';
      hud.appendChild(Tootsville.UI.makePrettyDialog(title,text,accept,cancel,resolve)); }); };

/**
 * Quit without prompting
 */
Tootsville.UI.forceQuit = function ()
{ Tootsville.Login.quit(); };

/**
 * Request the user's confirmation to quit (or not)
 */
Tootsville.UI.quit = function () { Tootsville.UI.signOut (); };

/**
 * Request the user's confirmation to quit (or not)
 */
Tootsville.UI.signOut = function ()
{ Tootsville.UI.confirmPretty(
    "Quit?",
    "Are you sure you want to quit playing now?",
    "Quit").then
  ( reallyP =>
    { if (reallyP)
      { Tootsville.Login.quit(); }}); };

/**
 * Set the Fullscreen control panel toggle based on the current state of
 * the navigator.
 */
Tootsville.UI.setFullscreenFromNavigator = function ()
{ let fullscreenCheck = document.getElementById('fullscreen-toggle');
  if (fullscreenCheck)
  { fullscreenCheck.checked = !! (document.fullscreenElement); }};

/**
 * Sot fullscreen or windowed mode.
 */
Tootsville.UI.setFullscreen = function (really)
{ if (really)
  { document.documentElement.requestFullscreen(); }
  else
  { document.exitFullscreen(); }};

/**
 * Toggle between fullscreen and windowed mode
 */
Tootsville.UI.toggleFullscreen = function ()
{ Tootsville.UI.setFullscreen( ! (document.fullscreenElement) );
  if (Tootsville.Tank.engine)
  { setTimeout ( () => { Tootsville.Tank.engine.resize (); }, 1000); } };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.UI.onFirstClick = function ()
{ window.removeEventListener('click', Tootsville.UI.onFirstClick);
  console.log ('noticed a first click');
  document.addEventListener('beforeunload', (ev) => {
      let confirmationMessage = "If you leave or reload this page, you'll quit the game.";
      ev.returnValue = confirmationMessage;  /* Gecko, Trident, Chrome 34+ */
      return confirmationMessage; /* Gecko, WebKit, Chrome <34 */ }); };

/**
 * Put up a warning about slow loading.
 */
Tootsville.UI.slowLoadingWatchdog = function ()
{ Tootsville.UI.confirmPretty
  ( "Slow Loading?",
    ( "Loading seems to be taking a long time. You might have trouble " +
      "playing the game if your connection is very bad. Do you want to " +
      "wait and see if it improves?" ),
    "Keep Waiting", "Read Troubleshooting Tips").then
  ( waitP =>
    { if (! waitP)
      { document.location = 'https://wiki.tootsville.org/wikiki/PlayNotLoading'; } }); };

/**
 * Walk one step in any direction.
 *
 * For keyboard or gamepad inputs.
 */
Tootsville.UI.takeOneStep = function (δx, δz)
{ const avatar = Tootsville.Tank.avatars [Tootsville.character];
  const model = avatar.model;
  Tootsville.Game.Nav.walkTheLine (avatar,
                                   new BABYLON.Vector3 (model.position.x + δx,
                                                        model.position.y,
                                                        model.position.z + δz));
};

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.UI.interact = function (entity)
{ if (!entity) { entity = Tootsville.UI.findAdjacentEntity (); }
  /* TODO */ };

/**
 * Discover the nearest entity within ``arms' reach'' of the player's facing direction.
 *
 * This is for e.g. game pad or keyboard inputs.
 */
Tootsville.UI.findAdjacentEntity = function ()
{ /* TODO */ };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.UI.useActiveItem = function (entity)
{ if (!entity) { entity = Tootsville.UI.findAdjacentEntity (); }
  /* TODO */ };

/**
 * WRITEME — this function is not yet documented.
 */
Tootsville.UI.clickedOnItem = function (meshName, picked)
{ /* TODO: Identify owning entity and call Tootsville.UI.interact (entity) */
    console.log ("Click on " + meshName, picked);
};

Tootsville.UI.getSpeechVolume = function ()
{ return Tootsville.Game.Speech.loudness || 'talk'; };

/**
 * WRITEME
 */
Tootsville.UI.say = function (speech)
{ if ("~" === speech.charAt (0))
  { Tootsville.Game.Speech.dispatchCommand (speech); }
  else
  { Tootsville.Gossip.send ("speak", { speech: speech,
                                       vol: Tootsville.UI.getSpeechVolume () }); } };

/**
*
*/
Tootsville.UI.insertEmoji = function (event) {
    document.getElementById ('talk-speak').value += event.target.innerText;
} ;
