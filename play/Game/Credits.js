/* -*- js2 -*- */
/**@license
 * play/Game/Credits.js is part of Tootsville
 *
 * Copyright © 2008-2017, Bruce-Robert  Pocock; Copyright © 2018-2021,
 * the Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software: you can redistribute it and/or modify
 * it  under the  terms of  the GNU  Affero General  Public License  as
 * published by the  Free Software Foundation; either version  3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the  hope that it will be useful, but
 * WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
 * Affero General Public License for more details.
 *
 * You should  have received a  copy of  the GNU Affero  General Public
 * License    along     with    this     program.    If     not,    see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 */

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }


/**
 * Display the client credits and offer to fetch the server credits
 */
Tootsville.Game.credits = function () {
        Tootsville.UI.confirmPretty("Credits",
                                `
<p> Tootsville V version ${ Tootsville.version } </p>

<p>The Tootsville V web application is by Bruce-Robert Pocock.</p>

 <p> Copyright © 2008-2017,  Bruce-Robert Pocock; Copyright © 2018-2021,
The Corporation for Inter-World Tourism and Adventuring. </p>

<p>    This     program    is     Free    Software;    you     can    <a target="_new"
href="https://www.gnu.org/licenses/agpl-3.0.en.html">copy and  alter the
program</a>, but  you may not  use the Tootsville  characters, graphics,
sound, music, etc without permission. </p>
`,
                                "Server Credits", "Done").then
    (
        confirm => { if (confirm)
            Tootsville.Util.infinity ('speak', { speech: ",credits" }); }); };

