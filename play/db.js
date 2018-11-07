if (!('Tootsville' in window)) { Tootsville = { db: {}};}
if (!('db' in Tootsville)) {Tootsville.db = {};}

Tootsville.db.db = null;
Tootsville.db.version = 1;
Tootsville.db.migrateTo = {};
Tootsville.db.handleError = function (event)
{ switch (event.target.errorCode)
  { case VER_ERR:
    Tootsville.error("Database error", "Database version is greater than " + Tootsville.db.version);
    break;
    default:
    Tootsville.warn("Database error", event);
    alert("Database error: " + event.target.toSource()); }; };
Tootsville.db.upgradeNeeded = function (event)
{ var db = event.target.result;
  for (var i = 0; i < Tootsville.db.version; ++i)
  { Tootsville.db.migrateTo[i].call(db, db); } };
Tootsville.db.open = function ()
{ if (Tootsville.db.db)
  { return new Promise ( pass => { pass(Tootsville.db.db); }); }
  return new Promise
  ( pass =>
    { var request = window.indexedDB.open("Tootsville.Player", Tootsville.db.version);
      request.onsuccess = function (event) { Tootsville.db.db = event.result.target;
                                             pass(Tootsville.db.db); };
      request.onerror = Tootsville.db.handleError;
      request.onupgradeneeded = Tootsville.db.upgradeNeeded; }); };
Tootsville.db.find = function (store, field, key)
{ return new Promise
  ( after => { Tootsville.db.open().
               then( db => { var transaction = db.transaction([store]);
                             // FIXME, TODO, incomplete/broken
                             transaction.oncomplete = function (event)
                             { after(); };
                             transaction.onerror = function (event)
                             { Tootsville.error(event); }; }); }); };
Tootsville.db.set = function (storeName, object)
{ return new Promise
  ( after => { Tootsville.db.open().
               then( db => { var transaction = db.transaction([storeName], "readwrite");
                             var store = transaction.objectStore(storeName);
                             var request = store.add(object); // FIXME, TODO, incomplete/broken
                             request.oncomplete = () => {};
                             request.onerror = function (event) { Tootsville.error(event); };
                             request.onsuccess = function (event) { after(object);};});});};
