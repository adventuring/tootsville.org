/* -*- js2 -*-*/

/*@license
 *
 * ./play/colors.js is part of Tootsville
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

(function () {
    var tootColorMapping = {
        silver: "#ddd",
        charcoal: "#333",
        white: "#fff",
        black: "#000",

        "deep-purple": "#b117ff",
        "deep purple": "#b117ff", 
        yellow: "#fff216",
        pink: "#e73e97",
        turquoise: "#00a290",
        cyan: "#ccffff",
        periwinkle: "#96b4de",

        violet: "#9669ad",
        gold: "#f7d023",
        burgundy: "#9c0059",
        green: "#7ac142",
        blue: "#0082c8",

        lavender: "#ba9dca",
        tan: "#ffd2a0",
        red: "#e51b24",
        "spring-green": "#c4d82d",
        "spring green": "#c4d82d",
        indigo: "#0028ff",

        orange: "#ff7b26"
    };

    /**
     * Translate the color named @code{name} into HTML-style hex code.
     */
    window.interpretTootColor = function (name) {
        if (null == name || undefined == name)
        { return '#000'; }
        return tootColorMapping[name.toLowerCase()] || name;
    };
})();
