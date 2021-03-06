/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/fit-steps.js is part of Tootsville
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
if (!('UI' in Tootsville)) { Tootsville.UI = {}; }

Tootsville.UI.googleFit = {
    getStepCount: function () {
        return new Promise( (gotSteps, deniedSteps) => {
            Tootsville.Util.ensureGoogleOAuth().then(
                (pass) => { return Tootsville.UI.googleFit.sendStepCountRequest(); },
                (fail) => { return deniedSteps(fail); });
        });
    },
    sendStepCountRequest: function () {
        Tootsville.Util.postJSONforJSON(
            'https://www.googleapis.com/fitness/v1/users/me/dataset:aggregate',
            {
                aggregateBy: [{
                    dataTypeName: "com.google.step_count.delta",
                    dataSourceID:  "derived:com.google.step_count.delta:com.google.android.gms:estimated_steps"
                }],
                bucketByTime: { "durationMillis": 86400000 },
                startTimeMillis: precision.now() - 86400000,
                endTimeMillis: precision.now()
            },
            { Authorization: "Bearer " + Tootsville.googleOAuthToken }
        ).then(
            (success) => {
                // XXX no op for now
            }, (fail) => {
                deniedSteps(fail);
            });
    },
    wantStepCountAtLeast: function(wantCountAtLeast) {
        return new Promise( (pass, fail) => {
            Tootsville.UI.googleFit.getStepCount().then( (actualCount) => {
                if (actualCount >= wantCountAtLeast) {
                    pass(actualCount);
                } else {
                    fail(actualCount);
                }
            }, (deniedMessage) => {
                fail(deniedMessage);
            });
        });
    }
}
