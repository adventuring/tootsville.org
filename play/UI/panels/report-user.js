/* -*- js2 -*-*/

/**@license
 *
 * ./play/UI/panels/report-user.js is part of Tootsville
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

if (!('Tootsville' in window)) Tootsville = {UI: {ReportUser: {}}};
if (!('UI' in Tootsville)) Tootsville.UI = {ReportUser: {}};
if (!('ReportUser' in Tootsville.UI)) Tootsville.UI.ReportUser = {};

Tootsville.UI.ReportUser.send = function (event) {
    if (event)
        event.preventDefault(); /* do not submit form */
    let rule = '';
    const form = document.getElementById('report-user-form');
    const radios = form.querySelectorAll('input[type=radio]');
    for (let i = 0; i < radios.length; ++i)
        if (radios[i].checked)
            rule = radios[i].value;
    if (!(rule)) {
        Tootsville.UI.confirmPretty('Which Rule?',
                                    `<p> In order to report a player for breaking the rules, you must tell us
which rule they have broken. Our  volunteers will review the game's logs
and determine what actions to take next. </p>`,
                                    'O.K.', null);
        return;
    }
    Tootsville.Util.infinity ('reportUser',
                              { userName: document.getElementById('report-user-name').innerText,
                                note: document.getElementById('report-user-note').value,
                                rule: rule });
    Tootsville.UI.HUD.closePanel ();
};

document.getElementById('report-user-send').addEventListener ('click', Tootsville.UI.ReportUser.send);
