/* -*- js2 -*- */
if (!("Tootsville" in window)) { Tootsville = {}; }
if (!("login" in Tootsville)) { Tootsville.login = {}; }

Tootsville.login.firebaseLogin = function (loginPanel)
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
          firebase.auth.TwitterAuthProvider.PROVIDER_ID
          /* Twitter does not support scopes. */ ]}); };

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


