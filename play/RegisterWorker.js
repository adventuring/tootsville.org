/* -*- js2 -*-*/

/**@license
 *
 * ./play/register-worker.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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
 */if ('serviceWorker' in navigator) {
     window.addEventListener('load', function() {
         navigator.serviceWorker.register('/worker.js').then(registration => {
             Tootsville.inform('ServiceWorker registration successful with scope: ', registration.scope);
         }, err => {
             Tootsville.error('ServiceWorker registration failed: ', err);
         });
     });
 } else {
     alert('Can not register ServiceWorker.\nThis is a required feature to play the game.');
     document.location = 'https://wiki.tootsville.org/wiki/Can_not_register_ServiceWorker';
 }
