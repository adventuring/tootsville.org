Tootsville.hud = {
    talkBoxOpenP: true,
    getOpenPanel: function() {
        var panels = document.querySelectorAll('.hud-panel');
        if (! panels) { return null; }
        for (var i = 0; i < panels.length; i++) {
            var panel = panels[i];
            if (panel.style.opacity > .5) {
                return panel;
            }
        }
        return null;
    },

    closeHUDPanel: function() {
        var panelPopup = Tootsville.hud.getOpenPanel();
        if (panelPopup) {
            var panelID = panelPopup.id;
            if (panelID == 'paperdoll') {
                Tootsville.hud.returnPaperdollMini();
            }
            panelPopup.style.opacity = .1;
            panelPopup.style.maxHeight = '1px';
            setTimeout( (function(id) {
                document.getElementById(id).style.display = 'none';
            })(panelID), 1000 );
            panelPopup = null;
            return true;
        }
        return false;
    },

    loadScriptIntoDiv: function(src, div) {
        var script = document.createElement('SCRIPT');
        script.src = src;
        div.appendChild(script);
        return script;
    },

    loadHTML: function(src) {
        return new Promise( (after) => {
            var xhr = new XMLHttpRequest;
            xhr.onload = () => {
                after(xhr.response)
            };
            xhr.open("GET", src);
            xhr.responseType = 'document';
            xhr.send();
        } );
    },

    createHUDLoaderPanel: function(panel) {
        var div = document.createElement('DIV');
        var hud = document.getElementById('hud');
        if (! hud) { return; }

        div.id = panel + '-loading';
        div.className = 'hud-panel hud-panel-loader';
        div.style.opacity = .1;
        div.style.maxHeight = '1px';
        div.style.transition = 'opacity .2s, max-height .2s';
        if ('help' == panel) {
            div.innerHTML = '<i class="fa fa-life-ring fa-spin fa-5x"></i>';
        } else {
            div.innerHTML = '<i class="fa fa-spinner fa-spin fa-5x"></i>';
        }
        hud.appendChild(div);
        setTimeout(function () {
            div.style.display = 'block';
            div.style.opacity = 1;
            div.style.maxHeight = '100vh';
        }, 60);
    },

    loadHUDPanel: function(panelName, finish) {
        var spinnerDiv = Tootsville.hud.createHUDLoaderPanel(panelName);
        loadHTML("/play/ui/panels/" + panelName + ".html").then((htmlf) => {
            var panelDiv = htmlf.getElementById(panelName);
            hud.appendChild(panelDiv);
            loadScriptIntoDiv('/play/ui/panels/' + panelName + '.js', panelDiv);
            Tootsville.hud.showHUDPanel(panelName, panelDiv);
            var spinner = document.getElementById(panelName + '-loading');
            hud.removeChild(spinner);
            if (finish) { finish(panelDiv); }
        });
    },

    showHUDPanel: function(panel, div) {
        return new Promise( finish => {
            if (! div) {
                div = document.getElementById(panel);
            }
            if (div) {
                if (Tootsville.hud.getOpenPanel() != div) {
                    Tootsville.hud.closeHUDPanel();
                }
                div.style.opacity = 1;
                div.style.maxHeight = '100vh';
                div.style.display = 'block';
                div.className = 'hud-panel';
                finish();
                return;
            }
            Tootsville.hud.closeHUDPanel();
            Tootsville.hud.loadHUDPanel(panel, finish);
        });
    },

    toggleHUDPanel: function(panel) {
        var div = document.getElementById(panel);
        if (div && Tootsville.hud.getOpenPanel() == div) {
            Tootsville.hud.closeHUDPanel();
            return false;
        } else {
            Tootsville.hud.showHUDPanel(panel, div);
            return true;
        }
    },

    dropHUDPanels: function() {
        /* Only for  debugging, but useful  to call to force  reloading when
         * there are system changes on-the-fly. */
        var panels = document.querySelector('.hud-panel');
        for (var i = 0; i < panels.length; i++) {
            var panel = panels[i];
            panel.parentNode.removeChild(panel);
        }
    },

    beginWatchingPaperdollWindowForClose: function() {
        var box = document.getElementById('paperdoll');

        /* watch for hiding … */
        var boxWatcher = new MutationObserver( (records) => {
            if ((box.style.opacity < 1) || (box.style.display == 'none')) {
                Tootsville.hud.returnPaperdollMini();
            }
        });
        boxWatcher.observe(box, { attributes: true });

        /* … or unloading */
        var hudWatcher = new MutationObserver( (records) => {
            for (var mutation of records) {
                for (var i = 0; i < mutation.removedNodes.length; ++i) {
                    if (mutation.removedNodes[i] == box) {
                        Tootsville.hud.returnPaperdollMini();
                    }
                }
            }
        });
        var hud = document.getElementById('hud');
        boxWatcher.observe(hud, { childList: true });
    },

    positionPaperdollMini: function() {
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
    },

    returnPaperdollMini: function() {
        var mini = document.getElementById('paperdoll-mini');
        mini.style.top = 'unset';
        mini.style.left = 'unset';
        mini.style.height = 'unset';
        mini.style.width = 'unset';
        setTimeout( () => {
            mini.style = {};
        }, 60);
        mini.addEventListener('click', openPaperdoll);
    },

    openPaperdoll: function() {
        document.getElementById('paperdoll-mini').removeEventListener(
            'click', Tootsville.hud.openPaperdoll);
        Tootsville.hud.showHUDPanel('paperdoll').then(() => {
            Tootsville.hud.beginWatchingPaperdollWindowForClose();
            Tootsville.hud.positionPaperdollMini();
        });
    },

    destroyHUD: function() {
        var hud = document.getElementById('hud');
        if (hud) { hud.parentNode.removeChild(hud); }
    },

    openTalkBox: function() {
        Tootsville.hud.talkBoxOpenP = true;
        document.getElementById('talk-box').style.bottom = 0;
    },

    closeTalkBox: function() {
        Tootsville.hud.talkBoxOpenP = false;
        document.getElementById('talk-box').style.bottom = '-2.5rem';
    },

    toggleTalkBox: function() {
        if (Tootsville.hud.talkBoxOpenP) {
            Tootsville.hud.closeTalkBox();
        } else {
            Tootsville.hud.openTalkBox();
        }
    },
    refreshEquipment: function() {
        if (null == Tootsville.activeItem) {
            document.getElementById('active-item-box').style.opacity = 0;
        } else {
            // TODO
        }
        if (null == Tootsville.inactiveItem) {
            document.getElementById('inactive-item-box').style.opacity = 0;
        } else {
            // TODO
        }
    },

    switchActiveItem: function() {
        if (Tootsville.inactiveItem == null) { return; }
        var prior = Tootsville.activeItem;
        Tootsville.wardrobe.doff(Tootsville.activeItem);
        Tootsville.wardrobe.don(Tootsville.inactiveItem);
        Tootsville.wardrobe.don2(prior);
        /* TODO: cool rotate-and-swap animation between the two entities */
        Tootsville.hud.refreshEquipment();
    },
    refreshTalkStatus: function() {
        if (Tootsville.gossip.connectedP()) {
            // TODO
        } else {
            document.getElementById('talk-box').className = 'talk-disconnected';
            document.getElementById('talk-speak').placeholder = 'disconnected';
        }
    },

    refreshWallet: function () {
        // TODO
    }
};
