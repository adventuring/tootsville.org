Tootsville.db.migrateTo[1] = function(db) {
    return new Promise( done => {
        var credentials = db.createObjectStore("Credentials", {keyPath: 'service'});
        credentials.createIndex('service', 'service', { unique: true});
        credentials.transaction.oncomplete = () => {
            var prefs = db.createObjectStore("Preferences", {keyPath: 'pref'});
            prefs.createIndex('pref', 'pref', { unique: true});
            prefs.transaction.oncomplete = () => {
                done(db);
            };
        };
    });              
};

