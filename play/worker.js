Tootsville.registerServiceWorkner = function () {
    if ('serviceWorker' in navigator) {
        window.addEventListener('load', function() {
            navigator.serviceWorker.register('./worker.js').then(registration => {
                Tootsville.inform('ServiceWorker registration successful with scope: ', registration.scope);
            }, err => {
                Tootsville.error('ServiceWorker registration failed: ', err);
            });
        });
    } else {
        alert('Can not register ServiceWorker.\nThis is a required feature to play the game.');
        document.location = 'https://wiki.tootsville.org/wiki/Can_not_register_ServiceWorker';
    }
}
