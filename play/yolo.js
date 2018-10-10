if (!Tootsville.login) { Tootsville.login = {}; }

Tootsville.login.yoloConfig = {
    supportedAuthMethods: [
        "https://accounts.google.com"
    ],
    supportedIdTokenProviders: [
        {
            uri: "https://accounts.google.com",
            clientId: "1052481910502-97n2sod83avoegskb6849076d6dhtdkr.apps.googleusercontent.com"
        }
    ]
};

Tootsville.login.yoloError = function (error) {
    Tootsville.inform('YOLO Error 2', error.type, error.message, error);
    switch (error.type) {
    case "userCanceled":
        // The user closed the hint selector. Depending on the desired UX,
        // request manual sign up or do nothing.
        break;
    case "noCredentialsAvailable":
        // No hint available for the session. Depending on the desired UX,
        // request manual sign up or do nothing.
        break;
    case "requestFailed":
        // The request failed, most likely because of a timeout.
        // You can retry another time if necessary.
        rollbar.warn("requestFailed  from Google Yolo");
        setTimeout(Tootsville.login.googleYoloSignIn, 300);
        // TODO: put up a user information spinner or something
        break;
    case "operationCanceled":
        // The operation was programmatically canceled, do nothing.
        break;
    case "illegalConcurrentRequest":
        // Another operation is pending, this one was aborted.
        rollbar.warn("illegalConcurrentRequest from Google Yolo");
        break;
    case "initializationError":
        // Failed to initialize. Refer to error.message for debugging.
        rollbar.error("Initialization error from Google Yolo: " + error.message);
        break;
    case "configurationError":
        // Configuration error. Refer to error.message for debugging.
        rollbar.error("Initialization error from Google Yolo: " + error.message);
        break;
    default:
        rollbar.warn("Unexpected error.type from Google Yolo: " + error.type + ' (' + error.message + ')');
        // Unknown error, do nothing.
    };
}

Tootsville.login.storeCredentialInfo = function (credential) {
    Tootsville.inform("Google YOLO Signin", "Success");
    Tootsville.login.googleIDToken = credential.idToken;
    Tootsville.login.storeGoogleCredentials();
    if (! Tootsville.login.player) { Tootsville.login.player = {} ; }
    Tootsville.login.player.name = (credential.displayName || credential.id);
    Tootsville.login.player.email = credential.id;
    Tootsville.login.player.face = credential.profilePicture; // FIXME (Why? What's wrong with it?)
}

Tootsville.login.googleYoloSignIn = function (yolo) {
    googleyolo.retrieve(Tootsville.login.yoloConfig).then( (credential) => {
        if (credential.idToken) {
            Tootsville.login.storeCredentialInfo(credential);
            Tootsville.login.switchTootsView();
        } else {
            alert('Google sign-in failed?\nNo ID taken was returned.');
        }
    }, (error) => {
        Tootsville.inform('YOLO Error 1', error.type, error.message, error);
        googleyolo.hint(Tootsville.login.yoloConfig).then( (credential) => {
            if (credential.idToken) {
                Tootsville.login.storeCredentialInfo(credential);
                Tootsville.login.switchTootsView()
            } else {
                alert('Google sign-in failed?\nNo ID taken was returned.');
            }
        }, Tootsville.login.yoloError);
    });
};
