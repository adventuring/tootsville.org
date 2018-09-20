if (!Tootsville.ui) { Tootsville.ui = {}; }

Tootsville.ui.googleFit = {
    getStepCount: function () {
        return new Promise( (gotSteps, deniedSteps) => {
            Tootsville.util.ensureGoogleOAuth().then(
                (pass) => { return Tootsville.ui.googleFit.sendStepCountRequest(); },
                (fail) => { return deniedSteps(fail); });
        });
    },
    sendStepCountRequest: function () {
        Tootsville.util.postJSONforJSON(
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
            Tootsville.ui.googleFit.getStepCount().then( (actualCount) => {
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
