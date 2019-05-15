/* Fullscreen polyfill */
if (! document.documentElement.requestFullscreen) {
    if (document.documentElement.mozRequestFullScreen) {
        document.documentElement.requestFullscreen =
            document.documentElement.mozRequestFullScreen;
    }
}
if (! document.exitFullscreen) {
    if (document.mozCancelFullScreen) {
        document.exitFullscreen = document.mozCancelFullScreen;
    }
}
