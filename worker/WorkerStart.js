this.addEventListener('install', event => {
    event.waitUntil(new Promise( () => {
        /* do something interesting */
        return true; }));});

