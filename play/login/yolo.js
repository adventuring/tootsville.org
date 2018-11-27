/* -*- js2 -*- */
if (!("Tootsville" in window)) { Tootsville = {}; }
if (!("login" in Tootsville)) { Tootsville.login = {}; }

Tootsville.login.yoloConfig =
    { supportedAuthMethods:
        [ "https://accounts.google.com",
          "https://www.facebook.com",
          // 'https://www.linkedin.com',
          'https://login.live.com',
          // 'https://www.paypal.com',
          'https://twitter.com',
          'https://login.yahoo.com'],
        supportedIdTokenProviders:
        [{ uri: "https://accounts.google.com",
           /* TODO move Google client ID someplace cooler */
           clientId: "1052481910502-kb8u1evb6c82dl2o5km4vu6q5mmudkfo.apps.googleusercontent.com" },
         { uri: 'https://login.live.com' }],
      context: "signIn" /* or "signUp", "continue", "use" */ };

Tootsville.login.beginYolo = function (loginPanel)
{ var ui = new firebaseui.auth.AuthUI(firebase.auth());
  Tootsville.trace ("Starting Firebase login");
  ui.start(
      '#firebaseui-auth-container', 
      { callbacks:
        { signInSuccessWithAuthResult: Tootsville.login.acceptSignedIn,
          uiShown: function()
          { Tootsville.trace ("FirebaseUI NASCAR display ready");
            Tootsville.login.startSignIn(); }},
        // Will use popup for IDP Providers sign-in flow instead of the default, redirect.
        signInFlow: 'popup',
        tosUrl: 'https://wiki.tootsville.org/wiki/Core:ToS',
        privacyPolicyUrl: 'https://wiki.tootsville.org/wiki/Core:Privacy',
        signInOptions: 
        [ { provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
            scopes:
            [ 'https://www.googleapis.com/auth/plus.login' ],
            customParameters:
            // Forces account selection even when one account
            // is available.
            { prompt: 'select_account' } },
          // { provider: firebase.auth.FacebookAuthProvider.PROVIDER_ID,
          //   scopes:
          //   [ 'public_profile', 'email', 'user_likes' ] },
          // firebase.auth.PhoneAuthProvider.PROVIDER_ID,
          firebase.auth.TwitterAuthProvider.PROVIDER_ID /* Twitter does not support scopes. */ ]}); };

Tootsville.login.yoloError = function (error)
{ Tootsville.inform('YOLO Error 2', error.type, error.message, error);
  switch (error.type) {
  case "userCanceled":
      // The user closed the hint selector. Depending on the desired UX,
      // request manual sign up or do nothing.
      Tootsville.inform("OpenYOLO Sign-in", "Canceled");
      Tootsville.parrot.say("Canceled", "You canceled signing in.");
      break;
  case "noCredentialsAvailable":
      // No hint available for the session. Depending on the desired UX,
      // request manual sign up or do nothing.
      Tootsville.warn("No credentials available from OpenYOLO");
      Tootsville.parrot.say("Sign-In Provider Failed",
                            "Your sign-in provider did not pass us your credentials. We cannot sign you in.");
      break;
  case "requestFailed":
      // The request failed, most likely because of a timeout.
      // You can retry another time if necessary.
      Tootsville.warn("requestFailed  from OpenYOLO");
      setTimeout(Tootsville.login.openYoloSignIn, 300);
      /* TODO: put up a user information spinner until we retry */
      break;
  case "operationCanceled":
      // The operation was programmatically canceled, do nothing.
      break;
  case "illegalConcurrentRequest":
      // Another operation is pending, this one was aborted.
      Tootsville.warn("illegalConcurrentRequest from OpenYOLO");
      break;
  case "initializationError":
      // Failed to initialize. Refer to error.message for debugging.
      Tootsville.error("Initialization error from OpenYOLO: " + error.message);
      Tootsville.parrot.say("Sign-in Error",
                            "An error occurred trying to sign you in. This has been reported to our team: <br>" +
                            error.message);
      break;
  case "configurationError":
      // Configuration error. Refer to error.message for debugging.
      // TODO: should this be rollbar.error?
      Tootsville.error("Configuration error from OpenYOLO: " + error.message);
      Tootsville.parrot.say("Sign-In Configuration Error",
                            "Error from OpenYOLO sign-in. This has been reported to our team.<BR>" +
                            error.message);
      break;
  default:
      Tootsville.warn("Unexpected error.type from OpenYOLO: " + error.type +
                      ' (' + error.message + ')');
      // Unknown error, do nothing.
      Tootsville.parrot.say("Unknown Error",
                            "Error from OpenYOLO sign-in. This has been reported to our team.<BR>" +
                            error.message);
  }; };

Tootsville.login.acceptSignedIn = function(result)
{ Tootsville.trace ("User signed in", result);
  Tootsville.login.storeCredentialInfo (result);
  Tootsville.login.switchTootsView();
  return false; };

Tootsville.login.storeCredentialInfo = function (result)
{   var cred = result.credential; /* .accessToken,  idToken, providerId, signInMethod */
  var addl = result.additionalUserInfo; /*   →  isNewUser:  true,  providerId, profile:
   * {email,  family_name,  gender:  "male",  given_name,  hd:
   * "star-hope.org", id, link:  google+ URI, locale: "en-GB",
   * name, picture: URL, verified_email: true} */
  var user = result.user;
  /* o: "[DEFAULT]", refreshToken, displayName,    email,     emailVerified,    isAnonymous,
   * phoneNumber,  photoURL,   providerData  =  [{displayName,
   * email, phoneNumber, photoURL, providerId, uid}…], u = 'tootsville-v.firebaseapp.com', uid (≠ providerData[0].uid)*/
  Tootsville.login.accessToken = cred.accessToken;
  Tootsville.login.idToken = cred.idToken;
  Tootsville.login.idProvider = cred.providerId;
  if (! ("player" in Tootsville.login)) { Tootsville.login.player = {}; }
  Tootsville.login.player.name = user.displayName;
  Tootsville.login.player.email = (user.email ||
                                   user.providerData[0].email ||
                                   addl.email);
  Tootsville.login.player.face = (user.photoURL ||
                                  user.providerData[0].photoURL ||
                                  addl.picture);
  Tootsville.login.player.gender = addl.gender;
  Tootsville.login.player.locale = addl.locale; };

Tootsville.login.gotYolo = function (credential)
{ if (credential.idToken)
  { console.log("Got credentials", credential);
    Tootsville.login.googleIDToken = credential.idToken;
    Tootsville.login.storeGoogleCredentials();
    Tootsville.login.googleUser.getBasicProfile().then(
    (profile) => 
    { if (! Tootsville.login.player) { Tootsville.login.player = {} ; }
      Tootsville.login.player.name = profile.getName();
      Tootsville.login.player.email = profile.getEmail();
      Tootsville.login.player.face = profile.getImageUrl(); }); }
  else
  { console.log("Mysterious credentials lack an idToken", credential); } };

Tootsville.login.yoloRetrieveFailed = function (error)
{ Tootsville.inform('YOLO Error 1', error.type, error.message, error);
  openyolo.hint(Tootsville.login.yoloConfig).then(
      (credential) =>
          { if (credential.idToken)
            { Tootsville.login.storeCredentialInfo(credential);
              Tootsville.login.switchTootsView(); }
      else
            { alert('Google sign-in failed?\nNo ID token was returned.'); }},
      Tootsville.login.yoloError); };

Tootsville.login.openYoloSignIn = function ()
{ console.log("Asking Google YOLO to run");
  openyolo.retrieve(Tootsville.login.yoloConfig).then(
      (credential) => {Tootsville.login.gotYolo(credential);},
      (error) => {Tootsville.login.yoloRetrieveFailed(error);}); };

Tootsville.login.cancelYoloSignIn = function ()
{ openyolo.cancelLastOperation (); };

