/* -*- js2 -*-*/

/*@license
 *
 * ./play/analytics.js is part of Tootsville
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

Tootsville.trace = function (event, details) {
    console.log.apply(console, arguments);
};
Tootsville.inform = function (event, details) {
    console.info.apply(console, arguments);
    dataLayer.push({event: event, details: arguments});
    // ga('send', arguments[0], Array.prototype.join(arguments.splice(1), "\n"));
    if (('Rollbar' in window) && Rollbar.info)
    { Rollbar.info(Array.prototype.splice.call(arguments).
                   map((x) => {x.toSource();}).join('\t')); }
};
Tootsville.warn = function (message) {
    console.warn.apply(console, arguments);
    dataLayer.push({event: 'warning', details: arguments});
    ga('send', 'warning', Array.prototype.join(arguments, "\n"));
    if (('Rollbar' in window) && Rollbar.warn)
    { Rollbar.warn.apply (Rollbar,
                          Array.prototype.splice.call(arguments).
                          map((x) => {x.toSource();}).join('\t')); }
};
Tootsville.error = function (message) {
    console.error.apply(console, arguments);
    dataLayer.push({event: 'error', details: arguments});
    ga('send', 'error', Array.prototype.join(arguments, "\n"));
    throw new Error(message);
};
