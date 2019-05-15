(function () {
    var clickHandlers = {
        'control-panel-icon': () => { Tootsville.ui.hud.toggleHUDPanel('control-panel'); },
        'toolbox-mobile-phone': () => { Tootsville.ui.hud.showHUDPanel('mobile'); },
        'toolbox-contacts': () => { Tootsville.ui.hud.showHUDPanel('contacts'); },
        'paperdoll-mini': () => Tootsville.ui.hud.openPaperdoll,
        'troubleshooting-icon': () => { Tootsville.ui.hud.toggleHUDPanel('troubleshooting'); }
    };
    for (var id in clickHandlers) {
        document.getElementById(id).addEventListener('click', clickHandlers[id]);
    }
})();
