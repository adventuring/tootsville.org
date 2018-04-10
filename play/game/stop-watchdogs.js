Tootsville.game.hideWhenGameReady = function () {
    var toHide = document.querySelectorAll('.hide-when-game-ready');
    for (var i = 0; i < toHide.length; i++) {
        toHide[i].style.display = 'none';
    }
}


Tootsville.game.stopSlowLoadingWatchdogs = function () {
    clearTimeout(window.loadWatchdog1);
    clearTimeout(window.loadWatchdog2);
    window.loadWatchdog1 = null;
    window.loadWatchdog2 = null;
    
    var dialog = document.getElementById('elt-Slow-Loading-');
    if (dialog) {
        dialog.parentNode.removeChild(dlg);
    }
};
