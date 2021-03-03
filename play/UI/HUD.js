/* -*- js2 -*-*/

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
if (!('UI' in Tootsville)) { Tootsville.UI = { HUD: {} }; }
if (!('HUD' in Tootsville.UI)) { Tootsville.UI.HUD = {}; }

/**
 * If true, the Talk Box is open (visible).
 */
Tootsville.UI.HUD.talkBoxOpenP = true;

/**
 * Get the name of the currently-visible HUD panel.
 */
Tootsville.UI.HUD.getOpenPanel = function ()
{ const panels = document.querySelectorAll ('.hud-panel');
  if (! panels) { return null; }
  for (let i = 0; i < panels.length; i++)
  { const panel = panels[i];
    if (panel.style.opacity > .5)
    { return panel; } }
  return null; };

/**
 * Close (hide) the active HUD panel.
 */
Tootsville.UI.HUD.closePanel = function ()
{ let foundAny = false;
  let panelPopup = Tootsville.UI.HUD.getOpenPanel ();
  if (panelPopup)
  { let panelID = panelPopup.id;
    if ('paperdoll' === panelID)
    { Tootsville.UI.HUD.returnPaperdollMini (); }

    panelPopup.style.opacity = .1;
    panelPopup.style.maxHeight = '1px';
    setTimeout ( () => { (function (id)
                          { document.getElementById (id).style.display = 'none'; }
                         ) (panelID); },
                 1000 );
    panelPopup = null;
    foundAny = true; }
  if (! Tootsville.character)
  { Tootsville.Login.start (); }
  if ('$new toot' == Tootsville.character)
  { Tootsville.UI.HUD.loadHUDPanel ('new-toot'); }
  return foundAny; };

/**
 * Load a SCRIPT into a given DIV container.
 */
Tootsville.UI.HUD.loadScriptIntoDiv = function (src, div)
{ console.debug ("Load JavaScript: " + src);
  const script = document.createElement ('SCRIPT');
  script.src = src;
  div.appendChild (script);
  return script; };

/**
 * Load an HTML layer into a container.
 */
Tootsville.UI.HUD.loadHTML = function (src)
{ console.debug ("Load HTML: " + src);
  return new Promise (
      after =>
          { const xhr = new XMLHttpRequest; // XXX: Fetch
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
Tootsville.UI.HUD.createHUDLoaderPanel = function (panel)
{ let div = document.getElementById (panel + "-loading");
  const hud = document.getElementById ('hud');
  if (! hud) { return; }
  if (! div)
  { div = document.createElement ('DIV');
    div.id = panel + '-loading';
    div.className = 'hud-panel hud-panel-loader';
    div.style.opacity = .1;
    div.style.maxHeight = '1px';
    div.style.transition = 'opacity .2s, max-height .2s';
    if ('help' === panel)
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
Tootsville.UI.HUD.showHUDPanel = function (panel, div=null)
{ console.debug ("Show HUD Panel " + panel, div);
  return new Promise (
      finish =>
          { if (! div)
            { console.debug ("No DIV for " + panel + " passed in — looking for one");
              div = document.getElementById (panel); }
            if (div)
            { if (Tootsville.UI.HUD.getOpenPanel () &&
                  Tootsville.UI.HUD.getOpenPanel () != div)
              { Tootsville.UI.HUD.closePanel (); }
              div.style.opacity = 1;
              div.style.maxHeight = '100vh';
              div.style.display = 'block';
              div.className = 'hud-panel';
              div.position = 'absolute';
              finish ();
              return; }
            else
            { console.debug ("DIV not loaded; calling loadHUDPanel (" + panel + ")");
              Tootsville.UI.HUD.closePanel ();
              Tootsville.UI.HUD.loadHUDPanel (panel, finish); } }); };

/**
 * Load a  HUD Panel from /play/UI/panels/.  Each panel has an  HTML and
 * a Javascript component associated with it.
 */
Tootsville.UI.HUD.loadHUDPanel = function (panelName, finish)
{ console.debug ("Loading HUD panel: " + panelName);
  var panelDiv = document.getElementById (panelName);
  if (panelDiv)
  { Tootsville.UI.HUD.showHUDPanel (panelName, panelDiv);
    console.debug ("Re-using panel: " + panelName);
    if (finish) { finish (panelDiv); } }
  else
  { var spinnerDiv = Tootsville.UI.HUD.createHUDLoaderPanel (panelName);
    Tootsville.UI.HUD.loadHTML ("/play/UI/panels/" + panelName + ".html").
    then (
        htmlf =>
            { if (!htmlf)
              { Tootsville.error ("Failed to load: " + panelName); }
              var panelDiv = htmlf.getElementById (panelName);
              if (!panelDiv)
              { Tootsville.Gossip.Parrot.say ("Pop-up problem",
                                              "I loaded the pop-up for " + panelName +
                                              " and got back something I couldn't use." +
                                              "You won't be able to access that panel " +
                                              "for now. ☹");
                Tootsville.error ("No DIV <DIV ID=\"" + panelName +
                                  "\"> in HTML fragment " + panelName); }
              const hud = document.getElementById ('hud');
              var spinner = document.getElementById(panelName + '-loading');
              if (spinner)
              { console.debug ("Loaded " + panelName + ", removing spinner ", spinner);
                hud.removeChild (spinner); }
              console.debug ("Attaching " + panelName + " from ", htmlf, " as ",
                             panelDiv);
              panelDiv.setAttribute("class", "hud-panel");
              hud.appendChild (panelDiv);
              console.debug ("Showing " + panelName);
              Tootsville.UI.HUD.showHUDPanel (panelName, panelDiv);
              console.debug ("Loading " + panelName + " JavaScript");
              Tootsville.UI.HUD.loadScriptIntoDiv ('/play/UI/panels/' + panelName + '.js',
                                                   panelDiv);
              if (finish) { console.debug ("Calling panel finish function", finish,
                                           panelDiv);
                            finish (panelDiv); }
              else { console.debug ("No finish function for " + panelName); }}); }};

/**
 * Toggle the visibility of the named HUD panel.
 */
Tootsville.UI.HUD.toggleHUDPanel = function (panel)
{ var div = document.getElementById (panel);
  if (div && Tootsville.UI.HUD.getOpenPanel () === div)
  { Tootsville.UI.HUD.closePanel ();
    return false; }
  else
  { Tootsville.UI.HUD.showHUDPanel (panel, div);
    return true; }};

/**
 * Drop all HUD panels to force  reloading them. May not always work due
 * to caching.
 */
Tootsville.UI.HUD.dropHUDPanels = function ()
{ /* Only  for debugging,  but useful  to call  to force  reloading when
   * there are system changes on-the-fly. */
    var panels = document.querySelector ('.hud-panel');
    for (var i = 0; i < panels.length; i++)
    { let panel = panels[i];
      panel.parentNode.removeChild (panel); } };

/**
 * Watch the  paperdoll (large)  window for Close  events (ie,  layer is
 * made invisible or hidden in the DOM).
 */
Tootsville.UI.HUD.beginWatchingPaperdollWindowForClose = function ()
{ let box = document.getElementById ('paperdoll');

  /* watch for hiding … */
  let boxWatcher = new MutationObserver (
      records =>
          { if ( (box.style.opacity < 1) || ('none' === box.style.display))
            { Tootsville.UI.HUD.returnPaperdollMini (); }});
  boxWatcher.observe (box, { attributes: true });

  /* … or unloading */
  let hudWatcher = new MutationObserver (
      records =>
          { for (let mutation of records)
            { for (let i = 0; i < mutation.removedNodes.length; ++i)
              { if (mutation.removedNodes[i] === box)
                { Tootsville.UI.HUD.returnPaperdollMini ();}}}});
  let hud = document.getElementById ('hud');
  boxWatcher.observe (hud, { childList: true });};

/**
 * Position the paperdoll  appropriately for the stage box  in the large
 * paperdoll   display,    or   the    mini   box   (widget)    in   the
 * lower-right corner.
 */
Tootsville.UI.HUD.positionPaperdollMini = function ()
{ let mini = document.getElementById ('paperdoll-mini');
  let fig = mini.querySelector ("IMG");
  let stage = document.getElementById ('doll-stage');
  let miniBox = mini.getBoundingClientRect ();
  let stageBox = stage.getBoundingClientRect ();
  let halfWidth = (stageBox.width / 2);
  let halfHeight = (stageBox.height / 2);
  let centerX = stageBox.left + halfWidth + (miniBox.width / 2);
  let centerY = stageBox.top + halfHeight + (miniBox.height / 2);
  mini.style.left = miniBox.left + "px";
  mini.style.top = miniBox.top + "px";
  mini.style.removeProperty('right');
  mini.style.removeProperty('bottom');
  mini.style.cursor = 'default';
  setTimeout ( () =>
               { let halfWidth = (stageBox.width / 2);
                 let halfHeight = (stageBox.height / 2);
                 mini.style.left = centerX + "px";
                 mini.style.top = centerY + "px";
                 mini.style.width = halfWidth + "px";
                 mini.style.height = halfHeight + "px";
                 fig.style.width = halfWidth + "px";
                 fig.style.height = halfHeight + "px";
                 fig.width = halfWidth;
                 fig.height = halfHeight;
                 mini.style.zIndex = 650; },
               5);
  setTimeout ( () =>
               { let halfWidth = (stage.offsetWidth / 2);
                 let halfHeight = (stage.offsetHeight / 2);
                 mini.style.top = stageBox.top + 'px';
                 mini.style.left = stageBox.left + 'px';

                 /* These become the start positions for the return trip */
                 mini.style.right = (window.innerWidth - stageBox.right) + 'px';
                 mini.style.bottom = (window.innerHeight
                                      - stageBox.bottom
                                      + halfHeight
                                      - (miniBox.height/2)) + 'px';

                 mini.style.height = stage.offsetHeight + 'px';
                 mini.style.width = stage.offsetWidth + 'px';
                 fig.style.height = stage.offsetHeight + 'px';
                 fig.style.width = stage.offsetWidth + 'px';
                 fig.height = stage.offsetHeight;
                 fig.width = stage.offsetWidth;
                 mini.style.borderRadius = 0; },
               500);};

/**
 * Return the paperdoll from the large window to the icon widget.
 */
Tootsville.UI.HUD.returnPaperdollMini = function ()
{ let mini = document.getElementById ('paperdoll-mini');
  mini.style.top = 'unset';
  mini.style.left = 'unset';
  mini.style.height = 'unset';
  mini.style.width = 'unset';
  mini.style.zIndex = 'unset';
  let fig = mini.querySelector ('img');
  fig.removeAttribute ('height');
  fig.removeAttribute ('width');
  fig.removeAttribute ('style');
  setTimeout ( () => { mini.removeAttribute ('style'); }, 60);
  mini.addEventListener ('click',
                         event => { Tootsville.UI.HUD.openPaperdoll (event); });
};

/**
 * Open the Paperdoll display from the paperdoll-mini widget.
 */
Tootsville.UI.HUD.openPaperdoll = function (event=null)
{ if (event) { event.stopPropagation (); }
  document.getElementById ('paperdoll-mini').removeEventListener
  ('click', Tootsville.UI.HUD.openPaperdoll);
  Tootsville.UI.HUD.showHUDPanel ('paperdoll').
  then ( () =>
      { if (!(Τootsville.activity.inv))
          Tootsville.Util.infinity('getInventory');
        Tootsville.UI.HUD.beginWatchingPaperdollWindowForClose ();
        Tootsville.UI.HUD.positionPaperdollMini (); });
  return true; };

/**
 * Destroy the HUD layer.
 */
Tootsville.UI.HUD.destroyHUD = function ()
{ const hud = document.getElementById ('hud');
  if (hud) { hud.parentNode.removeChild (hud); }};

/**
 * Open (reveal) the Talk Box.
 */
Tootsville.UI.HUD.openTalkBox = function (event=null)
{ Tootsville.UI.HUD.talkBoxOpenP = true;
  document.getElementById ('talk-box').removeEventListener  ('click', Tootsville.UI.HUD.openTalkBox);
  document.getElementById ('talk-box').style.bottom = '0px';
  if (event) { event.stopPropagation (); } };

/**
 * Close (hide) the Talk Box
 */
Tootsville.UI.HUD.closeTalkBox = function (event=null)
{ Tootsville.UI.HUD.talkBoxOpenP = false;
  document.getElementById ('talk-box').addEventListener ('click', Tootsville.UI.HUD.openTalkBox);
  document.getElementById ('talk-box').style.bottom = '-2.25rem';
  if (event) { event.stopPropagation (); } };

/**
 * Toggle visibility of the Talk Box
 */
Tootsville.UI.HUD.toggleTalkBox = function ()
{ if (Tootsville.UI.HUD.talkBoxOpenP)
  { Tootsville.UI.HUD.closeTalkBox (); }
  else
  { Tootsville.UI.HUD.openTalkBox (); }};

/**
 * Is the paperdoll mini up-to-date?
 */
Tootsville.UI.HUD.paperdollCurrentP = function ()
{ let paperdoll = document.getElementById ('paperdoll-mini');
  if (! paperdoll) { return true; }
  if (! Tootsville.Tank.avatars ) { return true; }
  let currentAvatar = Tootsville.Tank.avatars [ Tootsville.character ];
  if (! currentAvatar) { return true; }
  if (! currentAvatar.avatar ) { return true; }
  if (! paperdoll.avatar) { paperdoll.avatar = {}; }
  if (//Tootsville.Util.equalP (currentAvatar.equip != paperdoll.avatar.equip) &&
      (paperdoll.lastHeight === paperdoll.offsetHeight) &&
      (paperdoll.lastWidth === paperdoll.offsetWidth) )
  { return true; }
  return false; };

/**
 * Create the canvas for paperdoll display
 */
Tootsville.UI.HUD.createPaperdollCanvas = function (paperdoll)
{ let canvas = document.createElement ("CANVAS");
  canvas.height = paperdoll.offsetHeight;
  canvas.width = paperdoll.offsetWidth;
  return canvas; };

/**
 * Set the  paperdoll avatar  values to the  current avatar  values, and
 * also remember its current height.
 *
 * If  any of  these changes,  the paperdoll  will need  to be  redrawn.
 * See `Tootsville.UI.HUD.paperdollCurrentP'.
 */
Tootsville.UI.HUD.setPaperdollForPlayerAvatar = function (paperdoll)
{ let currentAvatar = Tootsville.Tank.avatars [ Tootsville.character ];
  Object.assign (paperdoll.avatar, currentAvatar);
  paperdoll.lastHeight = paperdoll.offsetHeight;
  paperdoll.lastWidth = paperdoll.offsetWidth; };
  
/**
 * Ensure that the paperdoll is up-to-date, updating it if needed.
 */
Tootsville.UI.HUD.refreshPaperdoll = function ()
{ if (Tootsville.UI.HUD.paperdollCurrentP ()) { return; }
  let paperdoll = document.getElementById ('paperdoll-mini');
  Tootsville.UI.HUD.setPaperdollForPlayerAvatar (paperdoll);
  let canvas = Tootsville.UI.HUD.createPaperdollCanvas (paperdoll);
  Tootsville.AvatarViewer.createViewerInCanvas (paperdoll.avatar, canvas, paperdoll); };

/**
 * Refresh the display of the active equipment item.
 */
Tootsville.UI.HUD.refreshEquipment = function ()
{ if (!Tootsville.player || !Tootsville.player.activeItem)
  { document.getElementById ('active-item-box').style.opacity = 0; }
  else
  { document.getElementById ('active-item-box').style.opacity = 1; 
    console.debug ("TODO: active item box"); }
  if (!Tootsville.player || !Tootsville.player.inactiveItem)
  { document.getElementById ('inactive-item-box').style.opacity = 0; }
  else
  { document.getElementById ('inactive-item-box').style.opacity = 1;
    console.debug ("TODO: inactive item box"); }};

/**
 * Switch the active item with the secondary item.
 */
Tootsville.UI.HUD.switchActiveItem = function ()
{ if (null === Tootsville.player || null === Tootsville.player.inactiveItem)
  { return; }
  let prior = Tootsville.player.activeItem;
  if (prior) { Tootsville.Game.Wardrobe.doff (prior); }
  Tootsville.Game.Wardrobe.don (Tootsville.player.inactiveItem);
  if (prior) { Tootsville.Game.Wardrobe.don2 (prior); }
  console.debug ("TODO: cool rotate-and-swap animation between the two item boxes");
  Tootsville.UI.HUD.refreshEquipment (); };

/**
 * Refresh the status of the Talk Box (disconnected, sensitive, or regular).
 */
Tootsville.UI.HUD.refreshTalkStatus = function ()
{ if (Tootsville.Gossip.connectedP () && Tootsville.character)
  { if (Tootsville.Game.anyAvatarSensitiveP ())
    { document.getElementById ('talk-box').className = 'talk-warning';
      document.getElementById ('talk-speak').placeholder = "◆ (Mind your manners)";
      document.getElementById ('talk-speak').title = 'There may be children or sensitive players nearby. Please use appropriate language.'; }
    else
    { document.getElementById ('talk-box').className = 'talk-connected';
      document.getElementById ('talk-speak').placeholder = "Press HOME or CONTROL+E to talk";
      document.getElementById ('talk-speak').title = 'Click here to talk';  } }
  else
  { document.getElementById ('talk-box').className = 'talk-disconnected';
    document.getElementById ('talk-speak').placeholder = 'disconnected';
    document.getElementById ('talk-speak').title = 'You appear to be off-line.'; }};

/**
 * Refresh  the wallet  display, both  in the  HUD and  (if loaded)  the
 * Wallet app in Tootnix.
 */
Tootsville.UI.HUD.refreshWallet = function ()
{ if (! Tootsville.character) { return false; }
  if (! Tootsville.Tank.avatars [ Tootsville.character ] )
  { console.warn ("No avatar info for " + Tootsville.character);
    Tootsville.Tank.avatars [ Tootsville.character ] = { peanuts: -1, fairyDust: -1 }; }
  const walletAppPeanuts = document.getElementById ('wallet-show-peanuts');
  const walletAppPeanutCount = document.getElementById ('wallet-show-peanuts-count');
  if (0 < Tootsville.Tank.avatars [ Tootsville.character ].peanuts)
  { document.getElementById ('wallet-peanuts-display').innerHTML = Tootsville.Tank.avatars [ Tootsville.character ] .peanuts;
    document.getElementById ('wallet-peanuts-icon').style.opacity = 1;
    if (walletAppPeanuts) {
        walletAppPeanuts.style.opacity = 1;
        walletAppPeanutCount.innerHTML =Tootsville.Tank.avatars [ Tootsville.character ] .peanuts; } }
  else
  { document.getElementById ('wallet-peanuts-display').innerHTML = '';
    document.getElementById ('wallet-peanuts-icon').style.opacity = 0;
    if (walletAppPeanuts) {
        walletAppPeanuts.style.opacity = 0;
        walletAppPeanutCount.innerHTML = ''; } }
  const walletAppFairyDust = document.getElementById ('wallet-show-fairy-dust');
  const walletAppFairyDustCount = document.getElementById ('wallet-show-fairy-dust-count');
  if (0 < Tootsville.Tank.avatars [ Tootsville.character ] .fairyDust)
  { document.getElementById ('wallet-fairy-dust-display').innerHTML = Tootsville.Tank.avatars [ Tootsville.character ] .fairyDust;
    document.getElementById ('wallet-fairy-dust-icon').style.opacity = 1;
    if (walletAppFairyDust) {
        walletAppFairyDust.style.opacity = 1;
        walletAppFairyDustCount.innerHTML =Tootsville.Tank.avatars [ Tootsville.character ] .peanuts; } }
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
 *
 * These elements are refreshed about every 333 ms, but occur in a 4msec
 * (the minimum allowed setTimeout value  for HTML5) timeout handlers so
 * as to be more effectively asynchronous (and because browsers bitch if
 * you run too long in one setInterval handler).
 */
Tootsville.UI.HUD.refreshHUD = function ()
{ setTimeout ( () => { Tootsville.UI.HUD.refreshEquipment (); }, 4);
  setTimeout ( () => { Tootsville.UI.HUD.refreshTalkStatus (); }, 4);
  setTimeout ( () => { Tootsville.UI.HUD.refreshWallet (); }, 4);
  setTimeout ( () => { Tootsville.UI.HUD.refreshPaperdoll (); }, 4);
  setTimeout ( () => { Tootsville.UI.HUD.refreshTimeLeft (); }, 4);
  setTimeout ( () => { Tootsville.UI.HUD.refreshMapPointer (); }, 4); };

/**
 * When the map app is open, position the crosshairs over the player's position
 */
Tootsville.UI.HUD.refreshMapPointer = function ()
{ const crosshairs = document.getElementById ('tootanga-pointer');
  const map = document.getElementById ('tootanga-map');
  if (!(crosshairs)) return;
  const left = map.offsetLeft + ((Tootsville.activity.long + 400)/800) * map.offsetWidth - crosshairs.offsetWidth/2;
  const top = map.offsetTop + ((Tootsville.activity.lat + 300)/600) * map.offsetHeight - crosshairs.offsetHeight/2;
  crosshairs.style.top = top + 'px';
  crosshairs.style.left = left + 'px';
};

/**
 * WRITEME
 */
Tootsville.UI.HUD.setMapBadge = function (badge, position)
{ if ((position.world != 'CHOR') || (position.alt != 0)) return;
  const map = document.getElementById ('tootanga-map');
  if (!(map)) return;
  let badgeIcon = document.getElementById ('map-badge-' + badge);
  if (!badgeIcon) {
      badgeIcon = document.createElement ('IMG');
      badgeIcon.src = 'https://jumbo.tootsville.org/Assets/Maps/5/Badges/' + badge + '.png';
      badgeIcon.title = badge.replace ('-', ' ');
      badgeIcon.style.position = 'absolute'; }
  const left = map.offsetLeft + ((position.long + 400)/800) * map.offsetWidth - badgeIcon.offsetWidth/2;
  const top = map.offsetTop + ((position.lat + 300)/600) * map.offsetHeight - badgeIcon.offsetHeight/2;
  badgeIcon.style.top = top + 'px';
  badgeIcon.style.left = left + 'px'; };

/**
 * WRITEME
 */
Tootsville.UI.HUD.refreshMapBadges = function ()
{ for (let badge in gram.badges)
    Tootsville.UI.HUD.setMapBadge (badge, gram.badges [badge]); };

/**
 * Refresh the time remaining indicator for a child player
 */
Tootsville.UI.HUD.refreshTimeLeft = function ()
{ if (Tootsville.playUntil)
  { let remainMsec = ( (Tootsville.playUntil * 1000) -
                       (new Date ().getTime ()) );
    let remainMin = Math.floor (remainMsec / (60 * 1000));
    let remainHour = Math.floor (remainMin / 60);
    remainMin = remainMin % 60;
    if (remainMin < 10) { remainMin = '0' + remainMin; }
    document.getElementById ('time-left').innerText = '⌚' + remainHour + ':' + remainMin;
    if (remainHour > 0)
    { document.getElementById ('time-left').style.color =
      Tootsville.UI.interpretTootColor ('spring-green'); }
    else if (remainMin > 15)
    { document.getElementById ('time-left').style.color = 
      Tootsville.UI.interpretTootColor ('yellow'); }
    else
    { document.getElementById ('time-left').style.color = 
      Tootsville.UI.interpretTootColor ('red'); }
  }
  else
  { document.getElementById('time-left').innerText = ''; } };

/**
 * Toggle whether ELEMENT is displayed or not (with a transition
 * fade).
 */
Tootsville.UI.HUD.toggleElement = function (element)
{ if ('block' === element.style.display)
  { element.style.opacity = 0;
    setTimeout ( () => { element.style.display = 'none'; }, 150 ); }
  else
  { element.style.opacity = 1;
    element.style.display = 'block'; } };

/**
 * Toggle visibility of the Loudness selector for the Talk Box.
 */
Tootsville.UI.HUD.toggleTalkLoud = function (event)
{ Tootsville.UI.HUD.toggleElement (document.getElementById ('talk-loud-menu'));
  if ('shout' === Tootsville.Game.Speech.loudness)
  { document.getElementById ('talk-loud-shout').checked = true; }
  else if ('whisper' === Tootsville.Game.Speech.loudness)
  { document.getElementById ('talk-loud-whisper').checked = true; }
  else
  { document.getElementById ('talk-loud-talk').checked = true; }      
  event.stopPropagation (); };

/**
 * Toggle visibility of the Expressions selector for the Talk Box.
 */
Tootsville.UI.HUD.toggleTalkExpression = function (event)
{ Tootsville.UI.HUD.toggleElement (document.getElementById ('talk-expression-menu'));
  event.stopPropagation (); };

/**
 * Toggle visibility of the Emoji selector for the Talk Box.
 */
Tootsville.UI.HUD.toggleTalkEmoji = function (event)
{ Tootsville.UI.HUD.toggleElement (document.getElementById ('talk-emoji-menu'));
  event.stopPropagation (); };

/**
 * Connect events for the Talk box widgets at the bottom of the display.
 */
Tootsville.UI.HUD.connectTalkBox = function ()
{ document.getElementById ("talk-loud-selector").addEventListener
  ("click", Tootsville.UI.HUD.toggleTalkLoud);
  document.getElementById ("talk-expression").addEventListener
  ("click", Tootsville.UI.HUD.toggleTalkExpression);
  document.getElementById ("talk-emoji").addEventListener
  ("click", Tootsville.UI.HUD.toggleTalkEmoji);
  document.getElementById ("talk-speak-button").addEventListener
  ("click", Tootsville.UI.Keys.speakLine); };

/**
 * Set up the HUD layer and start housekeeping.
 */
Tootsville.UI.HUD.initHUD = function ()
{ setInterval (Tootsville.UI.HUD.refreshHUD, 333);
  Tootsville.UI.HUD.connectTalkBox (); };

/**
 * Refresh one 2D name tag  attachment object.
 *
 * These  attachments need  to  be refreshed  to keep  in  sync with  the
 * underlying 3D scene from time to time.
 */
Tootsville.UI.HUD.refreshNameTagAttachment = function (model, nameTag)
{ const renderWidth = Tootsville.Tank.engine.getRenderWidth ();
  const renderHeight = Tootsville.Tank.engine.getRenderHeight ();
  if (! (Tootsville.Tank.camera && Tootsville.Tank.camera.viewport) ) { return; }
  const abs = BABYLON.Vector3.Project (
      model.getAbsolutePosition (),
      BABYLON.Matrix.IdentityReadOnly,
      Tootsville.Tank.scene.getTransformMatrix (),
      Tootsville.Tank.camera.viewport.toGlobal (
          renderWidth, renderHeight)).divide (
              new BABYLON.Vector3 (renderWidth, renderHeight, 1)).multiply (
                  new BABYLON.Vector3 (document.getElementById ('tootsville3d').offsetWidth,
                                       document.getElementById ('tootsville3d').offsetHeight,
                                       1));
  nameTag.style.top = Math.max (30, Math.min (abs.y, window.innerHeight - 64)) + 'px';
  nameTag.style.left = Math.max (50, Math.min (abs.x, window.innerWidth - 50)) + 'px'; };

/**
 * Refresh one 2D speech attachment object.
 *
 * These  attachments need  to  be refreshed  to keep  in  sync with  the
 * underlying 3D scene from time to time.
 */
Tootsville.UI.HUD.refreshSpeechAttachment = function (model, speechBubble)
{ const renderWidth = Tootsville.Tank.engine.getRenderWidth ();
  const renderHeight = Tootsville.Tank.engine.getRenderHeight ();
  const abs = BABYLON.Vector3.Project (
      model.getAbsolutePosition ().add (new BABYLON.Vector3 (0, 10, 0)),
      BABYLON.Matrix.IdentityReadOnly,
      Tootsville.Tank.scene.getTransformMatrix (),
      Tootsville.Tank.camera.viewport.toGlobal (
          renderWidth, renderHeight)).divide (
              new BABYLON.Vector3 (renderWidth, renderHeight, 1)).multiply (
                  new BABYLON.Vector3 (document.getElementById ('tootsville3d').offsetWidth,
                                       document.getElementById ('tootsville3d').offsetHeight,
                                       1));
  if (!(speechBubble.ctime)) speechBubble.ctime = new Date();
  speechBubble.style.top = Math.max (30, Math.min (abs.y,
                                                   window.innerHeight - 64)) + 'px';
  speechBubble.style.left = Math.max (50, Math.min (abs.x,
                                                    window.innerWidth - 50)) + 'px'; };

/**
 * Refresh the 2D attachments for one avatar.
 */
Tootsville.UI.HUD.refreshAttachmentsForAvatar = function (avatar)
{ if (avatar.nameTag)
  { Tootsville.UI.HUD.refreshNameTagAttachment (avatar.model, avatar.nameTag); }
  if (avatar.speech)
  { Tootsville.UI.HUD.refreshSpeechAttachment (avatar.model, avatar.speech); } };

/**
 * Sort speech by the creation time of speech balloons
 */
Tootsville.UI.HUD.sortSpeechByCTime = function (avatars) {
    let unsorted = [];
    for (let i in avatars)
        if (avatars[i].speech) unsorted = unsorted.concat (avatars[i].speech);
    return unsorted.sort( (a,b) => {
        if (a.ctime > b.ctime) return 1; else return -1;
    }); };

/**
 * A generalized boolean as to whether a overlaps b
 *
 * a and b are HTML elements
 */
Tootsville.UI.HUD.overlappingP = function (a, b) {
    const rect1 = a.getBoundingClientRect ();
    const rect2 = b.getBoundingClientRect ();
    return (!(rect1.right < rect2.left || 
              rect1.left > rect2.right || 
              rect1.bottom < rect2.top || 
              rect1.top > rect2.bottom)); };

/**
 * If array[index] overlaps array[0..index-1], return the offending overlapper.
 * 
 * index must be >0.
 */
Tootsville.UI.HUD.speechOverlaps = function (array, index) {
    const firstArrival = array [index];
    for (let i = 0; i < index; ++i)
        if (Tootsville.UI.HUD.overlappingP (array [i], firstArrival))
            return array[i];
    return null;
};

/**
 * Try to move bumped out of the way of keeper
 *
 * It's a given that the two overlap currently
 */
Tootsville.UI.HUD.bumpSpeech = function (bumped, keeper) {
    bumped.style.fontSize = '0.75rem';
    const keepRect = keeper.getBoundingClientRect ();
    const bumpRect = bumped.getBoundingClientRect ();
    if (keepRect.top < bumpRect.top)
        bumped.style.top = Math.min(window.innerHeight - 64,
                               Math.max(keepRect.bottom, 30)) + "px";
    else
        bumped.style.top = Math.min(window.innerHeight - 64,
                               Math.max(keepRect.top - bumpRect.height, 30)) + "px"; };

/**
 * Try to move speech balloons so they don't overlap
 */
Tootsville.UI.HUD.arrangeSpeechBalloons = function (avatars) {
    const sorted = Tootsville.UI.HUD.sortSpeechByCTime (avatars);
    if (sorted.length < 2) return;
    for (let i = 1; i < sorted.length; ++i) {
        let overlapper = Tootsville.UI.HUD.speechOverlaps (sorted, i);
        if (overlapper)
            Tootsville.UI.HUD.bumpSpeech (overlapper, sorted[i]);
    }};

/**
 * Refresh all 2D attachment overlays to follow the 3D scene.
 */
Tootsville.UI.HUD.refreshAttachmentOverlays = function ()
{ if ( (! Tootsville.Tank.scene) ||
       (! Tootsville.Tank.avatars) )
  { return; }
  const avatars = Tootsville.Tank.avatars;
  for (let i in avatars)
      Tootsville.UI.HUD.refreshAttachmentsForAvatar (avatars [i]);
  Tootsville.UI.HUD.arrangeSpeechBalloons (avatars); };

/**
 * Convert an event on the HUD or CANVAS object into a 3D event as appropriate.
 */
Tootsville.UI.HUD.convertCanvasEventTo3D = function (event)
{ const picked = Tootsville.Tank.scene.pick (event.clientX, event.clientY);
  console.log ("Click at ", event.clientX + ", " + event.clientY, " hit ", picked);
  if (! picked) { return; }
  if (! picked.pickedMesh) { return; }
  if ('ground' === picked.pickedMesh.name /* === Tootsville.Tank.ground */)
  { if (event.detail > 1) /* double or triple click */
    { Tootsville.Game.Nav.runTo (Tootsville.Tank.avatars [ Tootsville.character ], picked.pickedPoint);
      return;}
    else
    { Tootsville.Game.Nav.walkTheLine (Tootsville.Tank.avatars [ Tootsville.character ], picked.pickedPoint); }
    return; }
  Tootsville.UI.HUD.clickedOnMesh (picked.pickedMesh, picked); };

/**
 * Show the Player Card pop-up for another player
 */
Tootsville.UI.HUD.showPlayerCard = function (name)
{ if (name === Tootsville.character) { return; }
  Tootsville.UI.HUD.showHUDPanel ('player-card').
  then ( () =>
      { document.getElementById ('other-player-name').innerText = Tootsville.Tank.avatars [ name ].userName;
        document.getElementById ('other-player-name').setAttribute ('data-player-name', name);
        const container = document.getElementById ('other-player-avatar');
        const canvas = Tootsville.UI.HUD.createPaperdollCanvas (container.querySelector ("IMG"));
        Tootsville.AvatarViewer.createViewerInCanvas (Tootsville.Tank.avatars [ name ],
                                                      canvas,
                                                      container);
        const contactStatus = document.getElementById ('other-player-contact-status');
        let contactKind = null;
        for (let i = 0; i < Tootsville.buddyList.length; ++i)
            if (Tootsville.buddyList [i].n === name)
        { if (Tootsville.buddyList [i].starredP)
            contactKind = 'starred';
          else
              contactKind = 'buddy'; };
        let ignore = `<button id="add-contact-as-ignore">Ignore</button>`;
        for (let i = 0; i < Tootsville.ignoreList.length; ++i)
            if (Tootsville.ignoreList[i].n === name)
                ignore = `<button id="remove-contact-as-ignore">Stop Ignoring</button>`;
        
        if (null === contactKind)
            contactStatus.innerHTML = `
<button id="add-contact-as-buddy">Add Contact</button>
${ignore}`;
        else if ('buddy' === contactKind)
            contactStatus.innerHTML = `
<button id="remove-contact-as-buddy">Remove Contact</button>
<button id="star-buddy">★ Star Contact</button>
${ignore}`;
        else if ('starred' === contactKind)
            contactStatus.innerHTML = `
<button id="remove-contact-as-buddy">Remove Contact</button>
<button id="unstar-buddy">☆ Unstar Contact</button>
${ignore}`;
        else
            contactStatus.innerHTML = `
<button id="add-contact-as-buddy">Add Contact</button>
${ignore}`;

        const addContactAsBuddy = document.getElementById('add-contact-as-buddy');
        if (addContactAsBuddy)
            addContactAsBuddy.addEventListener ('click',
                                                () => { Tootsville.Game.UserLists.addContactAsBuddy (name); });
        const addContactAsIgnore = document.getElementById('add-contact-as-ignore');
        if (addContactAsIgnore)
            addContactAsIgnore.addEventListener ('click',
                                                 () => { Tootsville.Game.UserLists.addContactAsIgnore (name); });
        const removeContactAsBuddy = document.getElementById('remove-contact-as-buddy');
        if (removeContactAsBuddy)
            removeContactAsBuddy.addEventListener ('click',
                                                   () => { Tootsville.Game.UserLists.removeContactAsBuddy (name); });
        const removeContactAsIgnore = document.getElementById('remove-contact-as-ignore');
        if (removeContactAsIgnore)
            removeContactAsIgnore.addEventListener ('click',
                                                    () => { Tootsville.Game.UserLists.removeContactAsIgnore (name); });
        const starBuddy = document.getElementById('star-buddy');
        if (starBuddy)
            starBuddy.addEventListener ('click',
                                        () => { Tootsville.Game.UserLists.starBuddy (name); });
        const unstarBuddy = document.getElementById('unstar-buddy');
        if (unstarBuddy)
            unstarBuddy.addEventListener ('click',
                                          () => { Tootsville.Game.UserLists.unstarBuddy (name); });
        
      });};

/**
 * Respond to a user click (tap) on a mesh in the tank
 */
Tootsville.UI.HUD.clickedOnMesh = function (mesh, picked)
{ const pickedName = mesh.name || '';
  if ('ground' === pickedName)
  { console.error ("Click on ground went to clickedOnMesh"); }
  if (0 === pickedName.indexOf ('avatar/'))
  { Tootsville.UI.HUD.showPlayerCard (mesh.name.substr (7)); }
  else if (0 === pickedName.indexOf ('item/'))
  { Tootsville.UI.clickedOnItem (mesh.name, picked); }
  else
  { // console.debug ('User clicked mesh ', mesh.name, picked);
    if (mesh.parent)
    { Tootsville.UI.HUD.clickedOnMesh (mesh.parent, picked); }
    else
    { console.debug ("No parent node of ", mesh.name, picked); } } };

/**
 * Respond to a user click (tap) on a name tag
 */
Tootsville.UI.HUD.nameTagClicked = function (event)
{ event.stopPropagation ();
  Tootsville.UI.HUD.showPlayerCard (event.target.tootName); };

/**
 * Show the player's mobile device
 */
Tootsville.UI.HUD.showMobile = function (event)
{ event.stopPropagation ();
  Tootsville.UI.HUD.showHUDPanel ('mobile'); };


/**
 * Show the camera widget
 */
Tootsville.UI.HUD.showCamera = function (event)
{ event.stopPropagation ();
  Tootsville.UI.HUD.showHUDPanel ('camera'); };

/**
 * Show the control panel menu
 */
Tootsville.UI.HUD.showControlPanel = function (event)
{ event.stopPropagation ();
  Tootsville.UI.HUD.showHUDPanel ('control-panel'); };

/**
 * WRITEME
 */
Tootsville.UI.HUD.refreshInventory = function () {
    const toolbar = document.getElementById ('paperdoll-toolbar');
    if (!toolbar) return;
    toolbar.innerHTML = 'Inventory';
    const page = document.getElementById ('inventory-page');
    page.innerHTML = '';
    for (let i = 0; i < Tootsville.activity.inv.length; ++i) {
        const item = document.createElement ('DIV');
        const info = Tootsville.activity.inv[i].info;
        const image = document.createElement ('IMG');
        image.height = 256;
        image.width = 256;
        item.appendChild (image);
        const canvas = document.createElement ('CANVAS');
        setTimeout ( () => {
            Tootsville.ItemViewer.createViewerInCanvas (info,
                                                        canvas, image); },
                     4);
        const name = document.createElement ('SPAN');
        name.innerText = info.template.name;
        item.appendChild (name);
        item.addEventListener (
            'click',
            event => { Tootsville.UI.HUD.inventoryClicked
                       (event, item, Tootsville.activity.inv[i]); });
        page.appendChild (item); }};

/**
 * WRITEME
 */
Tootsville.UI.HUD.inventoryClicked = function (event, itemDiv, item) {
    console.log ('got a click on inventory item ' + item.info.template.name);
    let replies = { drop: { type: 'neg', label: 'Drop' } };
    if (item.isActive)
        replies.don = { type: 'aff',
                          label: ([] === item.info.template.wearSlot ? 'Hold' : 'Wear') };
    else
        replies.doff = { type: 'neg', label: 'Put Away' };
    replies.close = { type: 'neu', label: '✗ Close' };
    Tootsville.UI.makePrompt ({
        title: item.info.template.name,
        msg: `<P>${item.info.template.description}</P>`,
        replies: replies },
                              selection => { Tootsville.UI.HUD.inventoryAction (selection); } );};

/**
 * WRITEME
 */
Tootsville.UI.HUD.inventoryAction = function (selection) {
    if ('close' === selection) return;
    if ('don' === selection) alert ('TODO: equip this item');
    if ('doff' === selection) alert ('TODO: doff this item');
    if ('drop' === selection) alert ('TODO: drop this item');
};
