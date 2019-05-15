window.onGoogleYoloLoad = function() {
    Tootsville.login.startSignIn();
    Tootsville.login.googleYoloSignIn();
}
window.addEventListener('load', () => {
    setTimeout(function() {
        document.getElementById('control-panel-icon').style.width = '7vw';
    }, 1000);

    window.addEventListener('click', Tootsville.ui.onFirstClick);
    document.addEventListener('fullscreenchange',
                              Tootsville.ui.setFullscreenFromNavigator);

    Tootsville.game.hideWhenGameReady();
    Tootsville.game.stopSlowLoadingWatchdogs();

    Tootsville.tank.prepareFor3D();

    Tootsville.login.start();

    setInterval(Tootsville.updateClock, 250);
});
