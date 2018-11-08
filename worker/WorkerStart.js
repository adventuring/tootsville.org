self.addEventListener('install', function (event) {
    return event.waitUntil(new Promise(() => { return true; }));
});
