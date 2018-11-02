window.onGoogleYoloLoad = function() {
    Tootsville.login.startSignIn();
    Tootsville.login.googleYoloSignIn();
}
window.addEventListener('load', () => {
    navigator.serviceWorker.register('./worker.js');
    Tootsville.hud.refreshEquipment();
    Tootsville.hud.refreshTalkStatus();
    Tootsville.hud.refreshWallet();

    setTimeout(function() {
        document.getElementById('control-panel-icon').style.width = '7vw';
    }, 1000);

    window.addEventListener('click', Tootsville.ui.onFirstClick);

    document.addEventListener('fullscreenchange', Tootsville.ui.setFullscreenFromNavigator);

    Tootsville.game.hideWhenGameReady();

    Tootsville.game.stopSlowLoadingWatchdogs();

    Tootsville.tank.start3D();

    setInterval(function(){Tootsville.updateClock();},250);
});
