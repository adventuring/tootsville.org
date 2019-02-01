/* -*- js2 -*-*/

/*@license
 *
 * ./play/db.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
 * Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */
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
