if (!('hud' in Tootsville.ui)) { Tootsville.ui.hud = {}; }

Tootsville.ui.hud.talkBoxOpenP = true;
Tootsville.ui.hud.getOpenPanel = function() {
    var panels = document.querySelectorAll('.hud-panel');
    if (! panels) { return null; }
    for (var i = 0; i < panels.length; i++) {
        var panel = panels[i];
        if (panel.style.opacity > .5) {
            return panel;
        }
    }
    return null;
};

Tootsville.ui.hud.closePanel = function(){ var foundAny = false;
  for (var panelPopup = Tootsville.ui.hud.getOpenPanel();
       panelPopup;
       panelPopup = Tootsville.ui.hud.getOpenPanel())
  { var panelID = panelPopup.id;
    if (panelID == 'paperdoll')
    { Tootsville.ui.hud.returnPaperdollMini(); }
    else
    { panelPopup.style.opacity = .1;
      panelPopup.style.maxHeight = '1px';
      setTimeout( (function(id)
                   { document.getElementById(id).style.display = 'none'; }
                  )(panelID), 1000 );
      panelPopup = null; }
    foundAny = true; }
  return foundAny; };

Tootsville.ui.hud.loadScriptIntoDiv = function(src, div) {
    var script = document.createElement('SCRIPT');
    script.src = src;
    div.appendChild(script);
    return script;
};

Tootsville.ui.hud.loadHTML = function(src) {
    return new Promise( (after) => {
        var xhr = new XMLHttpRequest;
        xhr.onload = () => {
            after(xhr.response)
        };
        xhr.open("GET", src);
        xhr.responseType = 'document';
        xhr.send();
    } );
};

Tootsville.ui.hud.createHUDLoaderPanel = function (panel)
{ var div = document.getElementById (panel + "-loading");
    var hud = document.getElementById('hud');
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

Tootsville.ui.hud.loadHUDPanel = function (panelName, finish)
{ var spinnerDiv = Tootsville.ui.hud.createHUDLoaderPanel (panelName);
  Tootsville.ui.hud.loadHTML ("/play/ui/panels/" + panelName + ".html").
  then (
      htmlf =>
          { var panelDiv = htmlf.getElementById (panelName);
        if (!panelDiv)
            { console.error ("No DIV <DIV ID=\"" + panelName +
                             "\"> in HTML fragment " + panelName); }
            var hud = document.getElementById('hud');
            console.log("Loaded " + panel + ", remove spinner", spinnerDiv);
            hud.removeChild (spinnerDiv);
            if (panelDiv)
            { hud.appendChild (panelDiv);
              loadScriptIntoDiv ('/play/ui/panels/' + panelName + '.js',
                                 panelDiv);
        Tootsville.ui.hud.showHUDPanel(panelName, panelDiv);
              if (finish) { finish (panelDiv); }}}); };

Tootsville.ui.hud.showHUDPanel = function(panel, div) {
    return new Promise (
        finish =>
            { if (! div)
              { div = document.getElementById (panel); }
              if (div)
              { if (Tootsville.ui.hud.getOpenPanel () != div)
                { Tootsville.ui.hud.closePanel (); }
            div.style.opacity = 1;
            div.style.maxHeight = '100vh';
            div.style.display = 'block';
            div.className = 'hud-panel';
            div.position = 'absolute';
            finish();
                return; }
        Tootsville.ui.hud.closePanel();
              Tootsville.ui.hud.loadHUDPanel (panel, finish); }); };

Tootsville.ui.hud.toggleHUDPanel = function(panel) {
    var div = document.getElementById(panel);
    if (div && Tootsville.ui.hud.getOpenPanel() == div) {
        Tootsville.ui.hud.closePanel();
        return false;
    } else {
        Tootsville.ui.hud.showHUDPanel(panel, div);
        return true;
    }
};

Tootsville.ui.hud.dropHUDPanels = function() {
    /* Only for  debugging, but useful  to call to force  reloading when
     * there are system changes on-the-fly. */
    var panels = document.querySelector('.hud-panel');
    for (var i = 0; i < panels.length; i++) {
        var panel = panels[i];
        panel.parentNode.removeChild(panel);
    }
};

Tootsville.ui.hud.beginWatchingPaperdollWindowForClose = function() {
    var box = document.getElementById('paperdoll');

    /* watch for hiding … */
    var boxWatcher = new MutationObserver( (records) => {
        if ((box.style.opacity < 1) || (box.style.display == 'none')) {
            Tootsville.ui.hud.returnPaperdollMini();
        }
    });
    boxWatcher.observe(box, { attributes: true });

    /* … or unloading */
    var hudWatcher = new MutationObserver( (records) => {
        for (var mutation of records) {
            for (var i = 0; i < mutation.removedNodes.length; ++i) {
                if (mutation.removedNodes[i] == box) {
                    Tootsville.ui.hud.returnPaperdollMini();
                }
            }
        }
    });
    var hud = document.getElementById('hud');
    boxWatcher.observe(hud, { childList: true });
};

Tootsville.ui.hud.positionPaperdollMini = function() {
    var mini = document.getElementById('paperdoll-mini');
    var stage = document.getElementById('doll-stage');
    var miniBox = mini.getBoundingClientRect();
    var stageBox = stage.getBoundingClientRect();
    var centerX = stageBox.left + (stageBox.width / 2) + (miniBox.width / 2);
    var centerY = stageBox.top + (stageBox.height / 2) + (miniBox.height / 2);
    mini.style.left = miniBox.left + "px";
    mini.style.top = miniBox.top + "px";
    mini.style.right = null;
    mini.style.bottom = null;
    mini.style.cursor = 'default';
    setTimeout( () => {
        mini.style.left = centerX + "px";
        mini.style.top = centerY + "px";
    }, 5);
    setTimeout( () => {
        mini.style.top = stageBox.top + 'px';
        mini.style.left = stageBox.left + 'px';

        /* These become the start positions for the return trip */
        mini.style.right = (window.innerWidth - stageBox.right) + 'px';
        mini.style.bottom = (window.innerHeight
                             - stageBox.bottom
                             + (stageBox.height/2)
                             - (miniBox.height/2)) + 'px';

        mini.style.height = stageBox.height + 'px';
        mini.style.width = stageBox.width + 'px';
        mini.style.borderRadius = 0;
    }, 500);
};

Tootsville.ui.hud.returnPaperdollMini = function() {
    var mini = document.getElementById('paperdoll-mini');
    mini.style.top = 'unset';
    mini.style.left = 'unset';
    mini.style.height = 'unset';
    mini.style.width = 'unset';
    setTimeout( () => {
        mini.style = {};
    }, 60);
    mini.addEventListener('click', openPaperdoll);
};

Tootsville.ui.hud.openPaperdoll = function() {
    document.getElementById('paperdoll-mini').removeEventListener(
        'click', Tootsville.ui.hud.openPaperdoll);
    Tootsville.ui.hud.showHUDPanel('paperdoll').then(() => {
        Tootsville.ui.hud.beginWatchingPaperdollWindowForClose();
        Tootsville.ui.hud.positionPaperdollMini();
    });
};

Tootsville.ui.hud.destroyHUD = function() {
    var hud = document.getElementById('hud');
    if (hud) { hud.parentNode.removeChild(hud); }
};

Tootsville.ui.hud.openTalkBox = function() {
    Tootsville.ui.hud.talkBoxOpenP = true;
    document.getElementById('talk-box').style.bottom = 0;
};

Tootsville.ui.hud.closeTalkBox = function() {
    Tootsville.ui.hud.talkBoxOpenP = false;
    document.getElementById('talk-box').style.bottom = '-2.5rem';
};

Tootsville.ui.hud.toggleTalkBox = function() {
    if (Tootsville.ui.hud.talkBoxOpenP) {
        Tootsville.ui.hud.closeTalkBox();
    } else {
        Tootsville.ui.hud.openTalkBox();
    }
};
Tootsville.ui.hud.refreshEquipment = function() {
    if (null == Tootsville.player || null == Tootsville.player.activeItem) {
        document.getElementById('active-item-box').style.opacity = 0;
    } else {
        // TODO
    }
    if (null == Tootsville.player || null == Tootsville.player.inactiveItem) {
        document.getElementById('inactive-item-box').style.opacity = 0;
    } else {
        // TODO
    }
};

Tootsville.ui.hud.switchActiveItem = function() {
    if (null == Tootsville.player || null == Tootsville.player.inactiveItem) { return; }
    var prior = Tootsville.player.activeItem;
    Tootsville.wardrobe.doff(Tootsville.player.activeItem);
    Tootsville.wardrobe.don(Tootsville.player.inactiveItem);
    Tootsville.wardrobe.don2(prior);
    /* TODO: cool rotate-and-swap animation between the two entities */
    Tootsville.ui.hud.refreshEquipment();
};
Tootsville.ui.hud.refreshTalkStatus = function() {
    if (Tootsville.gossip.connectedP()) {
        // TODO
    } else {
        document.getElementById('talk-box').className = 'talk-disconnected';
        document.getElementById('talk-speak').placeholder = 'disconnected';
    }
};

Tootsville.ui.hud.refreshWallet = function () {
    // TODO
};

window.addEventListener('load', () => {
    Tootsville.ui.hud.refreshEquipment();
    Tootsville.ui.hud.refreshTalkStatus();
    Tootsville.ui.hud.refreshWallet();
});
