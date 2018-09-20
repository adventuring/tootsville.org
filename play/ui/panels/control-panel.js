if (! document.documentElement.requestFullscreen) {
    document.getElementById('control-panel-fullscreen').className = "disabled";
    document.getElementById('fullscreen-toggle').disabled = true;
}
Tootsville.ui.setFullscreenFromNavigator();

if (navigator.userAgent.indexOf("Firefox/") > 0) {
    var ffox = document.getElementById('control-panel-get-firefox');
    ffox.innerHTML =
        '<i class="fab fa-firefox fa-fw"></i> Got Firefox ✓ ' +
        '<i class="fas fa-window-restore footnote-marker"></i>';
    ffox.style.opacity = .7;
}

Tootsville.audio.updateVolumeUI();
