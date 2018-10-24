if (window.chrome && chrome.app) {
    chrome.app.runtime.onLaunched.addListener(function() {
        chrome.app.window.create('/play/index.html', {
            'outerBounds': {
                'width': 1920,
                'height': 1000
            }
        });
    });
}
