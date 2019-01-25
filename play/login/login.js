/* -*- js2 -*- */

/*@license
 *
 * ./play/login/login.js is part of Tootsville
 *
 * Copyright   ©  2016,2017   Bruce-Robert  Pocock;   ©  2018,2019   The
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
if (!("Tootsville" in window)) { Tootsville = {}; }
if (! ("login" in Tootsville)) { Tootsville.login = {}; }

Tootsville.login.overlay = function ()
{ Tootsville.ui.hud.loadHUDPanel ("login"); };

Tootsville.login.start = function ()
{ if (document.getElementById ("login") && document.getElementById ("login").style.display == "block")
  { Tootsville.trace ("Restarting login…");
    Tootsville.login.startSignIn();
    return; }
  Tootsville.trace ("Start login");
  Tootsville.ui.hud.loadHUDPanel ("login", Tootsville.login.firebaseLogin); };

Tootsville.login.drawAvatarOnCanvas = function (avatar,canvas)
{ /* TODO: Replace super-lame placeholder with paperdolls in Tootsville.login.drawAvatarOnCanvas */
    canvas.height = 128;
    canvas.width = 128;
    canvas.className = 'toot-paperdoll';
    var context = canvas.getContext ('2d');
    context.fillStyle = interpretTootColor (avatar.baseColor);
    context.fillRect (0, 0, 128, 128);
    context.font = '64px Acme';
    context.fillStyle = interpretTootColor (avatar.patternColor);
    context.fillText (avatar.pattern, 0, 64); };

Tootsville.login.clearTootsList = function ()
{ var toots = document.querySelectorAll ('.toot');
  for (var i = 0; i < toots.length; ++i)
  { toots[i].parentNode.removeChild (toots[i]); } };

Tootsville.login.serverQueryCharacters = function ()
{ return new Promise (
    (finish, reject) =>
        { Tootsville.util.rest ('GET', 'users/me/toots').
          then (
              response =>
                  { if (response.toots.length == 0)
                    { reject (); } else
                    { finish (response.toots); } },
              error =>
                  { Tootsville.parrot.ask ("Can't get Toots list",
                                           "I can't get a list of your Toots. Maybe there are network problems?",
                                           [{ tag: "retry",
                                              text: "Try Again" }]).
                    then (Tootsville.login.serverQueryCharacters);
                    Tootsville.error ("Can't retrieve Toots list", error);} ); }); };

Tootsville.login.createTootListItem = function (tootName)
{ var li = document.createElement ('LI');
  li.innerHTML = '<span class="toot-name">' +
  tootName + '</span><i class="fas fa-spin fa-spinner"></i>';
  li.onclick = function () { Tootsville.login.pickCharacter (li); };
  Tootsville.util.rest ('GET', 'users/me/toots/' + tootName).then(
      toot => { li.className = 'toot';
                li['data-toot'] = toot;
                var canvas = document.createElement ('CANVAS');
                Tootsville.login.drawAvatarOnCanvas (toot, canvas);
                li.innerHTML = '';
                li.appendChild (canvas);
                li.innerHTML += '<span class="toot-name">' +
                toot.name + '</span><span class="note">' + toot.note + '</span>';
                Tootsville.login.addChildOrSensitiveFlag (li); });
  return li; };

Tootsville.login.populateTootsList = function (list)
{ Tootsville.login.clearTootsList ();
  for (var i = 0; i < list.length; ++i)
  { var toot = list[i];
    var li = Tootsville.login.createTootListItem (toot);
    document.getElementById ('toots-list').appendChild (li); } };

Tootsville.login.generateNewToot = function ()
{ /* TODO let server generate new Toots with unique names */
    var toot =
        { name: "Needs-Naming",
          baseColor: "indigo",
          patternColor: "yellow",
          pattern: "lightning",
          padColor: "spring-green",
          childP: false,
          sensitiveP: false };
    Tootsville.login.createTootListItem (toot);
    Tootsville.login.editToot (toot); };

Tootsville.login.editToot = function (toot)
{ Tootsville.ui.hud.showHUDPanel('new-toot'); };

Tootsville.login.startCharacterCreation = function ()
{ Tootsville.parrot.say ("Let's get started!",
                         "<p>Let's create your Toot character.</p>" +
                         "<p>This takes about a minute. I'll create a new " +
                         "Toot character for you. You can change the name, colors, or " +
                         "pattern to be what you like. </p>").
  then(Tootsville.login.generateNewToot); };

Tootsville.login.loadTootsList = function ()
{ Tootsville.login.serverQueryCharacters ().then
  (Tootsville.login.populateTootsList,
   Tootsville.login.startCharacterCreation); };

Tootsville.login.dimUnpickedCharacters = function (picked)
{ var allItems = document.querySelectorAll ('#toots-list li');
  for (var i = 0; i < allItems.length; i++)
  { if (allItems[i] != picked)
    { allItems[i].style.opacity = .25;
      allItems[i].style.filter = 'grayscale (100%)'; } } };

Tootsville.login.playWithCharacter = function (name)
{ Tootsville.login.serverLinkTokenToCharacter (name).
  then (Tootsville.tank.startSimulation); };

Tootsville.login.pickCharacter = function (picked)
{ Tootsville.login.dimUnpickedCharacters (picked);
  document.querySelector ('#edit-toot-settings').style.opacity = .25;
  document.querySelector ('#new-toot-hint').style.opacity = .25;
  document.querySelector ('#switch-google-account').style.opacity = .25;
  Tootsville.login.playWithCharacter (picked['data-toot'].name); };

Tootsville.login.fillGoogleUserInfo = function ()
{ document.getElementById ('google-user-name').innerHTML = Tootsville.login.player.name;
  document.getElementById ('google-user-email').innerHTML = Tootsville.login.player.email;
  document.getElementById ('google-user-face').src = Tootsville.login.player.face; };

Tootsville.login.switchTootsView = function ()
{ Tootsville.login.loadTootsList ();
  Tootsville.login.fillGoogleUserInfo ();
  document.getElementById ('pick-toot').style.display = 'block';
  document.getElementById ('sign-in').style.display = 'none';
  document.getElementById ('login-warm-up').style.display = 'none'; };

Tootsville.login.startSignIn = function ()
{ document.getElementById ('sign-in').style.display = 'block';
  document.getElementById ('login-warm-up').style.display = 'none';
  document.getElementById ('pick-toot').style.display = 'none'; };

Tootsville.login.serverLinkTokenToCharacter = function (character)
{ return Tootsville.util.rest ('POST', 'users/me/play-with/' + character).then(
    () => { var loginMusic = querySelector("#login audio");
            if (loginMusic) // should always be found
            {  setTimeout ( () => { loginMusic.volume = .5; }, 333 );
               setTimeout ( () => { loginMusic.volume = .25; }, 666 );
               setTimeout ( () => { loginMusic.volume = 0; }, 1000 ); }
          alert ("Signed in as " + character); },
    error => { alert (character + " can't play right now. " + error); }); };

Tootsville.login.removeChildOrSensitive = function (li)
{ var child = li.querySelector ('.child');
  if (child) { li.removeChild (child); }
  var sensitive = li.querySelector ('.sensitive');
  if (sensitive) { li.removeChild (sensitive); } };

Tootsville.login.appendChildMode = function (li, tag, label, checkedP)
{ var name = '"' + li['data-toot'].name + '/child-mode"';
  li.innerHTML += '<label><input type="radio" onchange="updateChildMode (\'' + li['data-toot'].name +
  '\')" name=' + name +
  (checkedP ? 'checked ' : '') + ' value="' + tag + '"> ' + label + ' </label> <br>'; };

Tootsville.login.appendChildSensitiveRadioSet = function (li)
{ li.innerHTML += '<br clear="left"><hr>';
  Tootsville.login.appendChildMode (li, 'adult', '<i class="fas fa-graduation-cap fa-fw"></i> Adult account',
                                    ! (li['data-toot'].childP || li['data-toot'].sensitiveP));
  appendChildMode (li, 'child', '<i class="fas fa-child fa-fw"></i> Child account',
                   li['data-toot'].childP);
  appendChildMode (li, 'sensitive', '<i class="fas fa-chess-queen fa-fw"></i> Sensitive Player account',
                   li['data-toot'].sensitiveP); };

Tootsville.login.appendChildCodeEntry = function (li)
{ li.innerHTML += '<div class="define-child-code"' +
  (li['data-toot'].childP ? '' : ' style="display: none"' )+
  '>TODO: Child Code entry </div>'; };

Tootsville.login.findLIForToot = function (name)
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  for (var i = 0; i < toots.length; ++i)
  { if (toots[i]['data-toot'].name == name)
    { return toots[i]; } }
  throw new Error ("Altered a non-existent Toot"); };

Tootsville.login.findSelectedChildMode = function (li)
{ var buttons = li.querySelectorAll ('input[type="radio"]');
  for (var i = 0; i < buttons.length; i++)
  { if (buttons[i].checked)
    { return buttons[i].value; } }
  console.warn ("No radio button checked");
  return 'adult'; };

Tootsville.login.enableChildMode = function (li, name)
{ Tootsville.util.rest ('POST', 'toots/' + name + '/child-p', { set: true });
  li['data-toot'].childP = true;
  li['data-toot'].sensitiveP = false;
  li.querySelector ('.define-child-code').style.display = 'block'; };

Tootsville.login.disableChildMode = function (li, name)
{ Tootsville.util.rest ('POST', 'toots/' + name + '/child-p', { set: false });
  li['data-toot'].childP = false;
  li.querySelector ('.define-child-code').style.display = 'none'; };

Tootsville.login.enableSensitiveMode = function (li, name)
{ Tootsville.util.rest ('POST', 'toots/' + name + '/sensitive-p', { set: true });
  li['data-toot'].childP = false;
  li['data-toot'].sensitiveP = true; };

Tootsville.login.disableSensitiveMode = function (li, name)
{ Tootsville.util.rest ('POST', 'toots/' + name + '/sensitive-p', { set: false });
  li['data-toot'].sensitiveP = false; };

Tootsville.login.updateChildMode = function (name)
{ var li = findLIForToot (name);
  var mode = findSelectedChildMode (li);
  if (mode == 'child')
  { enableChildMode (li, name);
    disableSensitiveMode (li, name);
    return; }
  if (mode == 'sensitive')
  { disableChildMode (li, name);
    enableSensitiveMode (li, name);
    return; }
  disableChildMode (li, name);
  disableSensitiveMode (li, name); };

Tootsville.login.ensureChildSettings = function (li)
{ if (li.querySelector ('input[type="radio"]')) { return; }
  li.onclick = undefined;
  li.style.cursor = 'default';
  removeChildOrSensitive (li);
  appendChildSensitiveRadioSet (li);
  appendChildCodeEntry (li); };

Tootsville.login.childSettings = function ()
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  for (var i = 0; i < toots.length; ++i)
  { ensureChildSettings (toots[i]); }
  document.querySelector ('#toots-list').style.backgroundColor = '#c4d82d';
  document.querySelector ('#toots-list>#new-toot').style.display = 'none';
  document.querySelector ('#pick-toot>h2').innerHTML = 'Edit Toot Characters';
  document.querySelector ('#pick-toot>p').innerHTML = 'Set up Child and Sensitive Player options here. Child accounts have a sign-in code. (TODO: link to help)';
  document.querySelector ('#edit-toot-settings').style.display = 'none';
  document.querySelector ('#new-toot-hint').style.display = 'none';
  document.querySelector ('#edit-toot-settings-done').style.display = 'block'; };

Tootsville.login.stripChildSettings = function (li)
{ var buttons = li.querySelectorAll ('label');
  for (var i = 0; i < buttons.length; ++i)
  { li.removeChild (buttons[i]); }
  var breaks = li.querySelectorAll ('br');
  for (var i = 0; i < breaks.length; ++i)
  { li.removeChild (breaks[i]); }
  li.removeChild (li.querySelector ('hr'));
  li.removeChild (li.querySelector ('.define-child-code'));

  li.onclick = function () { pickCharacter (li); };
  addChildOrSensitiveFlag (li); };

Tootsville.login.addChildOrSensitiveFlag = function (li)
{ var toot = li['data-toot'];
  if (toot.childP)
  { li.innerHTML += '<span class="child"><i class="fas fa-child fa-fw"></i> Child Account</span>'; }
  if (toot.sensitiveP)
  { li.innerHTML += '<span class="sensitive"><i class="fas fa-chess-queen fa-fw"></i> Sensitive Player</span>'; } };

Tootsville.login.doneEditingSettings = function ()
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  for (var i = 0; i < toots.length; ++i)
  { stripChildSettings (toots[i]); }
  document.querySelector ('#toots-list').style.backgroundColor = '#ba9dca';
  document.querySelector ('#toots-list>#new-toot').style.display = 'block';
  document.querySelector ('#pick-toot>h2').innerHTML = 'Pick a Toot Character';
  document.querySelector ('#pick-toot>p').innerHTML = 'Click or tap a character to play now.';
  document.querySelector ('#edit-toot-settings').style.display = 'block';
  document.querySelector ('#new-toot-hint').style.display = 'block';
  document.querySelector ('#edit-toot-settings-done').style.display = 'none'; };

// Tootsville.login.copyGoogleUserInfo = function ()
// { Tootsville.login.googleIDToken = Tootsville.login.googleUser.getAuthResponse ().id_token;
//   Tootsville.login.storeGoogleCredentials ();
//   var profile = Tootsville.login.googleUser.getBasicProfile ();
//   if (! Tootsville.login.player) { Tootsville.login.player = {} ; }
//   Tootsville.login.player.name = profile.getName ();
//   Tootsville.login.player.email = profile.getEmail ();
//   Tootsville.login.player.face = profile.getImageUrl (); };

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
  return false; };

Tootsville.login.storeCredentialInfo = function (result)
{ var cred = result.credential;
  /* .accessToken,  idToken, providerId, signInMethod */
  var addl = result.additionalUserInfo;
  /*   →  isNewUser: true,  providerId, profile:  {email, family_name,
   * gender:  "male",  given_name,   hd:  "star-hope.org",  id,  link:
   * google+ URI, locale: "en-GB", name, picture: URL, verified_email:
   * true} */
  var user = result.user;
  /* o: "[DEFAULT]", refreshToken,  displayName, email, emailVerified,
   * isAnonymous, phoneNumber, photoURL, providerData = [{displayName,
   * email,   phoneNumber,   photoURL,   providerId,   uid}…],   u   =
   * 'tootsville-v.firebaseapp.com', uid (≠ providerData[0].uid)*/

  firebase.auth ().currentUser.getIdToken(/* forceRefresh */ true).
  then (function (idToken)
        { Tootsville.login.firebaseAuth = idToken;
          Tootsville.login.switchTootsView(); }).
  catch(function(error) {
      // Handle error TODO
  });

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
  Tootsville.login.player.locale = addl.locale;

};

Tootsville.login.quit = function ()
{ firebase.auth().signOut().then(Tootsville.login.start); };
