(function () {
    var clickHandlers = {
        'control-panel-icon': () => { Tootsville.hud.toggleHUDPanel('control-panel'); },
        'toolbox-mobile-phone': () => { Tootsville.hud.showHUDPanel('mobile'); },
        'toolbox-contacts': () => { Tootsville.hud.showHUDPanel('contacts'); },
        'paperdoll-mini': () => Tootsville.hud.openPaperdoll,
        'troubleshooting-icon': () => { Tootsville.hud.toggleHUDPanel('troubleshooting'); }        
    };
    for (var id in clickHandlers) {
        document.getElementById(id).addEventListener('click', clickHandlers[id]);
    }
})();
