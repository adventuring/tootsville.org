if (! document.documentElement.requestFullscreen) {
    document.getElementById('control-panel-fullscreen').className = "disabled";
    document.getElementById('fullscreen-toggle').disabled = true;
}

Tootsville.ui.setFullscreenFromNavigator();

if (navigator.userAgent.indexOf("Firefox/") > 0) {
    var ffox = document.getElementById('control-panel-get-firefox');
    ffox.outerHTML = '';
}

Tootsville.audio.updateVolumeUI();
