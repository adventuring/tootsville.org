/* -*- js2 -*-*/

/*@license
 *
 * ./play/db-migration-1.js is part of Tootsville
 *
 * Copyright   ©  2016,2017   Bruce-Robert  Pocock;   ©  2018,2019   The
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
