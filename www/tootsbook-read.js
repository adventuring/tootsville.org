/* -*- js2 -*-*/

/*@license
 *
 * ./www/tootsbook-read.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2021   The
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

(function () {
    fetch('https://www.tootsbook.com/tootsbook/feed/?format=rss',
          { method: 'GET', Accept: 'application/rss+xml' }).then 
    ( response => { if (response.ok) return response.text (); }).then
    ( text => new window.DOMParser().parseFromString(text, 'text/xml')).then
    ( feed => {
        const latest = feed.querySelector('item');
        if (latest)
            document.querySelector('#tootsbook-feed').innerHTML = `
<small>Latest News on Tootsbook: &nbsp; </small> 
<a href="${ latest.querySelector('link').innerHTML }"
>${ latest.querySelector('title').innerHTML }</a>`;   
    });})();
