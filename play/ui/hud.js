/* -*- js2 -*-*/

/*@license
 *
 * ./play/ui/hud.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
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
if (! ('hud' in Tootsville.ui)) { Tootsville.ui.hud = {}; }

/**
 * If true, the Talk Box is open (visible).
 */
Tootsville.ui.hud.talkBoxOpenP = true;

/**
 * Get the name of the currently-visible HUD panel.
 */
Tootsville.ui.hud.getOpenPanel = function ()
{ var panels = document.querySelectorAll ('.hud-panel');
  if (! panels) { return null; }
  for (var i = 0; i < panels.length; i++)
  { var panel = panels[i];
    if (panel.style.opacity > .5)
    { return panel; } }
  return null;};

/**
 * Close (hide) the active HUD panel.
 */
Tootsville.ui.hud.closePanel = function ()
{ var foundAny = false;
  for (var panelPopup = Tootsville.ui.hud.getOpenPanel ();
       panelPopup;
       panelPopup = Tootsville.ui.hud.getOpenPanel ())
  { var panelID = panelPopup.id;
    if (panelID == 'paperdoll')
    { Tootsville.ui.hud.returnPaperdollMini (); }
    else
    { panelPopup.style.opacity = .1;
      panelPopup.style.maxHeight = '1px';
      setTimeout ( () => { (function (id)
                            { document.getElementById (id).style.display = 'none'; }
                           ) (panelID); },
                   1000 );
      panelPopup = null; }
    foundAny = true; }
  if (! Tootsville.character)
  { Tootsville.login.start (); }
  return foundAny; };

/**
 * Load a SCRIPT into a given DIV container.
 */
Tootsville.ui.hud.loadScriptIntoDiv = function (src, div)
{ console.debug ("Load JavaScript: " + src);
  var script = document.createElement ('SCRIPT');
  script.src = src;
  div.appendChild (script);
  return script; };

/**
 * Load an HTML layer into a container.
 */
Tootsville.ui.hud.loadHTML = function (src)
{ console.debug ("Load HTML: " + src);
  return new Promise (
      after =>
          { var xhr = new XMLHttpRequest; // XXX: Fetch
            xhr.onload = () => {
                console.debug ("Got response from " + src, xhr.response);
                after (xhr.response);
            };
            xhr.open ("GET", src);
            xhr.responseType = 'document';
            xhr.send (); }); };

/**
 * Create a placeholder ``loading'' pop-up for a HUD panel.
 */
Tootsville.ui.hud.createHUDLoaderPanel = function (panel)
{ var div = document.getElementById (panel + "-loading");
  var hud = document.getElementById ('hud');
  if (! hud) { return; }
  if (! div)
  { div = document.createElement ('DIV');
    div.id = panel + '-loading';
    div.className = 'hud-panel hud-panel-loader';
    div.style.opacity = .1;
    div.style.maxHeight = '1px';
    div.style.transition = 'opacity .2s, max-height .2s';
    if ('help' == panel)
    { div.innerHTML = '<i class="fa fa-life-ring fa-spin fa-5x"></i>'; }
    else
    { div.innerHTML = '<i class="fa fa-spinner fa-spin fa-5x"></i>'; }
    hud.appendChild (div); }
  setTimeout (function ()
              { div.style.display = 'block';
                div.style.opacity = 1;
                div.style.maxHeight = '100vh'; },
              60); };

/**
 * Show the HUD panel named in the given DIV container.
 */
Tootsville.ui.hud.showHUDPanel = function (panel, div)
{ console.debug ("Show HUD Panel " + panel, div);
  return new Promise (
      finish =>
          { if (! div)
            { console.debug ("No DIV for " + panel + " passed in — looking for one");
              div = document.getElementById (panel); }
            if (div)
            { if (Tootsville.ui.hud.getOpenPanel () && Tootsville.ui.hud.getOpenPanel () != div)
              { Tootsville.ui.hud.closePanel (); }
              div.style.opacity = 1;
              div.style.maxHeight = '100vh';
              div.style.display = 'block';
              div.className = 'hud-panel';
              div.position = 'absolute';
              finish ();
              return; }
            else
            { console.debug ("DIV not loaded; calling loadHUDPanel (" + panel + ")");
              Tootsville.ui.hud.closePanel ();
              Tootsville.ui.hud.loadHUDPanel (panel, finish); } }); };

/**
 * Load a  HUD Panel from /play/ui/panels/.  Each panel has an  HTML and
 * a Javascript component associated with it.
 */
Tootsville.ui.hud.loadHUDPanel = function (panelName, finish)
{ console.debug ("Loading HUD panel: " + panelName);
  var panelDiv = document.getElementById (panelName);
  if (panelDiv)
  { Tootsville.ui.hud.showHUDPanel (panelName, panelDiv);
    console.debug ("Re-using panel: " + panelName);
    if (finish) { finish (panelDiv); } }
  else
  { var spinnerDiv = Tootsville.ui.hud.createHUDLoaderPanel (panelName);
    Tootsville.ui.hud.loadHTML ("/play/ui/panels/" + panelName + ".html").
    then (
        htmlf =>
            { if (!htmlf)
              { Tootsville.error ("Failed to load: " + panelName); }
              var panelDiv = htmlf.getElementById (panelName);
              if (!panelDiv)
              { Tootsville.parrot.say ("Pop-up problem",
                                       "I loaded the pop-up for " + panelName +
                                       " and got back something I couldn't use. You won't be able " +
                                       "to access that panel for now. ☹");
                Tootsville.error ("No DIV <DIV ID=\"" + panelName +
                                  "\"> in HTML fragment " + panelName); }
              var hud = document.getElementById ('hud');
              var spinner = document.getElementById(panelName + '-loading');
              if (spinner)
              { console.debug ("Loaded " + panelName + ", removing spinner ", spinner);
                hud.removeChild (spinner); }
              console.debug ("Attaching " + panelName + " from ", htmlf, " as ", panelDiv);
              panelDiv.setAttribute("class", "hud-panel");
              hud.appendChild (panelDiv);
              console.debug ("Showing " + panelName);
              Tootsville.ui.hud.showHUDPanel (panelName, panelDiv);
              console.debug ("Loading " + panelName + " JavaScript");
              Tootsville.ui.hud.loadScriptIntoDiv ('/play/ui/panels/' + panelName + '.js',
                                                   panelDiv);
              if (finish) { console.debug ("Calling panel finish function", finish, panelDiv);
                            finish (panelDiv); }
              else { console.debug ("No finish function for " + panelName); }}); }};

/**
 * Toggle the visibility of the named HUD panel.
 */
Tootsville.ui.hud.toggleHUDPanel = function (panel)
{ var div = document.getElementById (panel);
  if (div && Tootsville.ui.hud.getOpenPanel () == div)
  { Tootsville.ui.hud.closePanel ();
    return false; }
  else
  { Tootsville.ui.hud.showHUDPanel (panel, div);
    return true; }};

/**
 * Drop all HUD panels to force  reloading them. May not always work due
 * to caching.
 */
Tootsville.ui.hud.dropHUDPanels = function ()
{ /* Only  for debugging,  but useful  to call  to force  reloading when
   * there are system changes on-the-fly. */
    var panels = document.querySelector ('.hud-panel');
    for (var i = 0; i < panels.length; i++)
    { var panel = panels[i];
      panel.parentNode.removeChild (panel); } };

/**
 * Watch the  paperdoll (large)  window for Close  events (ie,  layer is
 * made invisible or hidden in the DOM).
 */
Tootsville.ui.hud.beginWatchingPaperdollWindowForClose = function ()
{ var box = document.getElementById ('paperdoll');

  /* watch for hiding … */
  var boxWatcher = new MutationObserver (
      records =>
          { if ( (box.style.opacity < 1) || (box.style.display == 'none'))
            { Tootsville.ui.hud.returnPaperdollMini (); }});
  boxWatcher.observe (box, { attributes: true });

  /* … or unloading */
  var hudWatcher = new MutationObserver (
      records =>
          { for (var mutation of records)
            { for (var i = 0; i < mutation.removedNodes.length; ++i)
              { if (mutation.removedNodes[i] == box)
                { Tootsville.ui.hud.returnPaperdollMini ();}}}});
  var hud = document.getElementById ('hud');
  boxWatcher.observe (hud, { childList: true });};

/**
 * Position the paperdoll  appropriately for the stage box  in the large
 * paperdoll   display,    or   the    mini   box   (widget)    in   the
 * lower-right corner.
 */
Tootsville.ui.hud.positionPaperdollMini = function ()
{ var mini = document.getElementById ('paperdoll-mini');
  var stage = document.getElementById ('doll-stage');
  var miniBox = mini.getBoundingClientRect ();
  var stageBox = stage.getBoundingClientRect ();
  var centerX = stageBox.left + (stageBox.width / 2) + (miniBox.width / 2);
  var centerY = stageBox.top + (stageBox.height / 2) + (miniBox.height / 2);
  mini.style.left = miniBox.left + "px";
  mini.style.top = miniBox.top + "px";
  mini.style.right = null;
  mini.style.bottom = null;
  mini.style.cursor = 'default';
  setTimeout ( () =>
               { mini.style.left = centerX + "px";
                 mini.style.top = centerY + "px"; },
               5);
  setTimeout ( () =>
               { mini.style.top = stageBox.top + 'px';
                 mini.style.left = stageBox.left + 'px';

                 /* These become the start positions for the return trip */
                 mini.style.right = (window.innerWidth - stageBox.right) + 'px';
                 mini.style.bottom = (window.innerHeight
                                      - stageBox.bottom
                                      + (stageBox.height/2)
                                      - (miniBox.height/2)) + 'px';

                 mini.style.height = stageBox.height + 'px';
                 mini.style.width = stageBox.width + 'px';
                 mini.style.borderRadius = 0; },
               500);};

/**
 * Return the paperdoll from the large window to the icon widget.
 */
Tootsville.ui.hud.returnPaperdollMini = function ()
{ var mini = document.getElementById ('paperdoll-mini');
  mini.style.top = 'unset';
  mini.style.left = 'unset';
  mini.style.height = 'unset';
  mini.style.width = 'unset';
  setTimeout ( () => { mini.style = {}; }, 60);
  mini.addEventListener ('click', openPaperdoll);
};

/**
 * Open the Paperdoll display from the paperdoll-mini widget.
 */
Tootsville.ui.hud.openPaperdoll = function ()
{ document.getElementById ('paperdoll-mini').removeEventListener
  ('click', Tootsville.ui.hud.openPaperdoll);
  Tootsville.ui.hud.showHUDPanel ('paperdoll').
  then ( () =>
         { Tootsville.ui.hud.beginWatchingPaperdollWindowForClose ();
           Tootsville.ui.hud.positionPaperdollMini (); }); };

/**
 * Destroy the HUD layer.
 */
Tootsville.ui.hud.destroyHUD = function ()
{ var hud = document.getElementById ('hud');
  if (hud) { hud.parentNode.removeChild (hud); }};

/**
 * Open (reveal) the Talk Box.
 */
Tootsville.ui.hud.openTalkBox = function ()
{ Tootsville.ui.hud.talkBoxOpenP = true;
  document.getElementById ('talk-box').removeEventListener
  ('click', Tootsville.ui.hud.openTalkBox);
  document.getElementById ('talk-box').style.bottom = 0;};

/**
 * Close (hide) the Talk Box
 */
Tootsville.ui.hud.closeTalkBox = function ()
{ Tootsville.ui.hud.talkBoxOpenP = false;
  document.getElementById ('talk-box').addEventListener
  ('click', Tootsville.ui.hud.openTalkBox);
  document.getElementById ('talk-box').style.bottom = '-2.25rem';};

/**
 * Toggle visibility of the Talk Box
 */
Tootsville.ui.hud.toggleTalkBox = function ()
{ if (Tootsville.ui.hud.talkBoxOpenP)
  { Tootsville.ui.hud.closeTalkBox (); }
  else
  { Tootsville.ui.hud.openTalkBox (); }};

/**
 * Ensure that the paperdoll is up-to-date
 */
Tootsville.ui.hud.refreshPaperdoll = function ()
{ const paperdoll = document.getElementById ('paperdoll-mini');
    if (Tootsville.character != paperdoll.avatar)
  { Tootsville.Avatars.createViewerInCanvas (Tootsville.character, paperdoll); }
  paperdoll.avatar = Tootsville.character;
};

/**
 * Refresh the display of the active equipment item.
 */
Tootsville.ui.hud.refreshEquipment = function ()
{ if (null == Tootsville.player || null == Tootsville.player.activeItem)
  { document.getElementById ('active-item-box').style.opacity = 0; }
  else
  { console.debug ("TODO: active item box"); }
  if (null == Tootsville.player || null == Tootsville.player.inactiveItem)
  { document.getElementById ('inactive-item-box').style.opacity = 0; }
  else
  { console.debug ("TODO: inactive item box"); }};

/**
 * Switch the active item with the secondary item.
 */
Tootsville.ui.hud.switchActiveItem = function ()
{ if (null == Tootsville.player || null == Tootsville.player.inactiveItem)
  { return; }
  var prior = Tootsville.player.activeItem;
  Tootsville.wardrobe.doff (Tootsville.player.activeItem);
  Tootsville.wardrobe.don (Tootsville.player.inactiveItem);
  Tootsville.wardrobe.don2 (prior);
  console.debug ("TODO: cool rotate-and-swap animation between the two item boxes");
  Tootsville.ui.hud.refreshEquipment ();
};

/**
 * Refresh the status of the Talk Box (disconnected, sensitive, or regular).
 */
Tootsville.ui.hud.refreshTalkStatus = function ()
{ if (Tootsville.gossip.connectedP ())
  { document.getElementById ('talk-box').className = 'talk-connected';
    document.getElementById ('talk-speak').placeholder = "Let's make some noise!"; }
  else
  { document.getElementById ('talk-box').className = 'talk-disconnected';
    document.getElementById ('talk-speak').placeholder = 'disconnected'; }};

/**
 * Refresh  the wallet  display, both  in the  HUD and  (if loaded)  the
 * Wallet app in Tootnix.
 */
Tootsville.ui.hud.refreshWallet = function ()
{ if (! Tootsville.character)
  { Tootsville.character = { peanuts: -1, fairyDust: -1 }; }
  const walletAppPeanuts = document.getElementById ('wallet-show-peanuts');
  const walletAppPeanutCount = document.getElementById ('wallet-show-peanuts-count');
  if (0 < Tootsville.character.peanuts)
  { document.getElementById ('wallet-peanuts-display').innerHTML = Tootsville.character.peanuts;
    document.getElementById ('wallet-peanuts-icon').style.opacity = 1;
    if (walletAppPeanuts) {
        walletAppPeanuts.style.opacity = 1;
        walletAppPeanutCount.innerHTML = Tootsville.character.peanuts; } }
  else
  { document.getElementById ('wallet-peanuts-display').innerHTML = '';
    document.getElementById ('wallet-peanuts-icon').style.opacity = 0;
    if (walletAppPeanuts) {
        walletAppPeanuts.style.opacity = 0;
        walletAppPeanutCount.innerHTML = ''; } }
  const walletAppFairyDust = document.getElementById ('wallet-show-fairy-dust');
  const walletAppFairyDustCount = document.getElementById ('wallet-show-fairy-dust-count');
  if (0 < Tootsville.character.fairyDust)
  { document.getElementById ('wallet-fairy-dust-display').innerHTML = Tootsville.character.fairyDust;
    document.getElementById ('wallet-fairy-dust-icon').style.opacity = 1;
    if (walletAppFairyDust) {
        walletAppFairyDust.style.opacity = 1;
        walletAppFairyDustCount.innerHTML = Tootsville.character.peanuts; } }
  else
  { document.getElementById ('wallet-fairy-dust-display').innerHTML = '';
    document.getElementById ('wallet-fairy-dust-icon').style.opacity = 0;
    if (walletAppFairyDust) {
        walletAppFairyDust.style.opacity = 0;
        walletAppFairyDustCount.innerHTML = ''; } }
  return true; };

/**
 * Refresh HUD  elements that are set  by server events (other  than the
 * clock), such as equipment, talk status, and wallet readouts.
 */
Tootsville.ui.hud.refreshHUD = function ()
{ Tootsville.ui.hud.refreshEquipment ();
  Tootsville.ui.hud.refreshTalkStatus ();
  Tootsville.ui.hud.refreshWallet ();
  Tootsville.ui.hud.refreshPaperdoll (); };

/**
 * Toggle visibility of the Loudness selector for the Talk Box.
 */
Tootsville.ui.hud.toggleTalkLoud = function ()
{ };

/**
 * Toggle visibility of the Expressions selector for the Talk Box.
*
* TODO
 */
Tootsville.ui.hud.toggleTalkExpression = function ()
{ };

/**
 * Toggle visibility of the Emoji selector for the Talk Box.
* 
* TODO
 */
Tootsville.ui.hud.toggleTalkEmoji = function ()
{ };

/**
 * Connect events for the Talk box widgets at the bottom of the display.
 */
Tootsville.ui.hud.connectTalkBox = function ()
{ document.getElementById ("talk-loud-selector").addEventListener
  ("click", Tootsville.ui.hud.toggleTalkLoud);};
{ document.getElementById ("talk-expression").addEventListener
  ("click", Tootsville.ui.hud.toggleTalkExpression);};
{ document.getElementById ("talk-emoji").addEventListener
  ("click", Tootsville.ui.hud.toggleTalkEmoji);};

/**
 * Set up the HUD layer and start housekeeping
 */
Tootsville.ui.hud.initHUD = function ()
{ setInterval (Tootsville.ui.hud.refreshHUD (), 250);
  Tootsville.ui.hud.connectTalkBox (); };
