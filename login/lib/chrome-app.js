if (window.chrome && chrome.app) {
    chrome.app.runtime.onLaunched.addListener(function() {
        chrome.app.window.create('/login/index.html', {
            'outerBounds': {
                'width': 800,
                'height': 550
            }
        });
    });
}
