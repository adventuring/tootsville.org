/* -*- js2 -*- */

/**@license
 *
 * play/Login/Login.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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

if (!("Login" in Tootsville)) { Tootsville.Login = {}; }

/**
 * Open the Login HUD panel
 */
Tootsville.Login.overlay = function ()
{ Tootsville.UI.HUD.loadHUDPanel ("login"); };

/**
 * Start the login process
 */
Tootsville.Login.start = function ()
{ if (document.getElementById ("login") &&
      document.getElementById ("login").style.display == "block")
  { Tootsville.trace ("Restarting login…");
    Tootsville.Login.startSignIn();
    return; }
  Tootsville.inform ("Start login");
  Tootsville.UI.HUD.loadHUDPanel ("login", Tootsville.Login.firebaseLogin); };

/**
 * Colorize the SVG avatar. DEPRECATED
 */
Tootsville.Login.colorizeAvatar = function (avatar, svg)
{ svg.children[0].children[1].children[0].setAttribute
  ("style",
   "fill:" +
   interpretTootColor (avatar.baseColor) +
   ";fill-opacity:1;fill-rule:evenodd;stroke:#030303;stroke-width:2;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1");
  svg.children[0].children[1].children[8].setAttribute
  ("style",
   "fill:" +
   interpretTootColor (avatar.padColor) +
   ";fill-opacity:1;stroke:#000000;stroke-width:2.15044212;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill");
  svg.children[0].children[1].removeChild (svg.children[0].children[1].children[9]); };

/**
 * Clear the login Toots list
 */
Tootsville.Login.clearTootsList = function ()
{ var spin = document.querySelector ('.toots-list-loading-spinner');
  if (spin)
  { spin.parentNode.removeChild (spin); }
  var toots = document.querySelectorAll ('.toot');
  for (var i = 0; i < toots.length; ++i)
  { toots[i].parentNode.removeChild (toots[i]); } };

/**
 * Is the login panel currently presenting account settings mode?
 */
Tootsville.Login.settingsP = false;

/**
 * Query the server for my characters after user has signed in.
 */
Tootsville.Login.loadTootsList = function ()
{ Tootsville.Util.infinity ("tootList"); };

/**
 * The set of Toot characters available to the player.
 */
if (!('toots' in Tootsville.Login)) { Tootsville.Login.toots = {}; }

/**
 *
 */
Tootsville.Login.createTootListItem = function (toot)
{ var li = document.createElement ('LI');
  if (! Tootsville.Login.settingsP)
  { li.onclick = function () { Tootsville.Login.pickCharacter (li); }; }
  li.innerHTML = '';
  li ['data-toot'] = toot;
  li.className = 'toot';
  if (toot.avatar)
  { li.innerHTML += '<IMG HEIGHT=256 WIDTH=256>';
    const canvas = document.createElement ('CANVAS');
    canvas.width = 256;
    canvas.height = 256;
    /* Create AvatarViewer asynchronously */
    setTimeout ( () => { 
        console.debug ("Drawing avatar " + toot.name + " in a canvas");
        Tootsville.AvatarViewer.createViewerInCanvas (toot, canvas, li); } ,
                 10 ); }
  li.innerHTML += '<SPAN CLASS="toot-name">' +
  toot.name + '</SPAN><SPAN CLASS="note">' + (toot.note || "") + '</SPAN>'; /// FIXME innerText
  Tootsville.Login.addChildFlag (li);return li; };

/**
 *
 */
Tootsville.Login.toots = {};

/**
 *
 */
Tootsville.Login.saveTootsList = function (list)
{ for (let i = 0; i < list.length; ++i)
  { Tootsville.Login.toots [ list [ i ].name ] = list [ i ];}
  Tootsville.Login.populateTootsList (); };

/**
 *
 */
Tootsville.Login.setSensitiveP = function ()
{ document.getElementById('sensitive-player-p').checked =
  (Tootsville.player && Tootsville.player.sensitiveP); };

/**
 *
 */
Tootsville.Login.populateTootsList = function ()
{ Tootsville.Login.clearTootsList ();
  Tootsville.Login.setSensitiveP ();
  for (let name in Tootsville.Login.toots)
  { let toot = Tootsville.Login.toots [name];
    let li = Tootsville.Login.createTootListItem (toot);
    document.getElementById ('toots-list').appendChild (li); };
  if (Tootsville.Login.settingsP)
  { Tootsville.Login.childSettings (); }
  else
  { Tootsville.Login.doneEditingSettings (); } };

/**
 *
 */
Tootsville.Login.generateNewToot = function ()
{ Tootsville.character = "$new toot";
  Tootsville.UI.HUD.showHUDPanel('new-toot'); };

/**
 *
 */
Tootsville.Login.startCharacterCreation = function ()
{ Tootsville.Gossip.Parrot.say ("Let's get started!",
                                "<p>Let's create your Toot character.</p>" +
                                "<p>This takes about a minute. I'll create a new " +
                                "Toot character for you. You can change the name, colors, or " +
                                "pattern to be what you like. </p>").
  then(Tootsville.Login.generateNewToot); };

/**
 *
 */
Tootsville.Login.dimUnpickedCharacters = function (picked)
{ var allItems = document.querySelectorAll ('#toots-list li');
  for (var i = 0; i < allItems.length; i++)
  { if (allItems[i] != picked)
    { allItems[i].style.opacity = .25;
      allItems[i].style.filter = 'grayscale (100%)'; } } };

/**
 *
 */
Tootsville.Login.doRealLogin = function (name)
{ Tootsville.Login.serverLinkTokenToCharacter (name); };

/**
 *
 */
Tootsville.Login.playWithCharacter = function (name)
{ let li = this.findLIForToot (name);
  if (li ['data-toot'].childP)
  { Tootsville.Gossip.Parrot.ask ("That's a Child Toot",
                                  "Are you sure you want to sign in with a Child account?",
                                  [{ tag: "yes", text: "Sign In" },
                                   { tag: "no", text: "Cancel" }]).
    then (answer => { if ('yes' == answer)
                      { this.doRealLogin (name); }
                      else
                      { this.populateTootsList ();
                        document.querySelector ('#edit-toot-settings').style.opacity = 1;
                        document.querySelector ('#edit-toot-settings').disabled = false;
                        document.querySelector ('#new-toot-hint').style.opacity = 1;
                        document.querySelector ('#new-toot-hint').disabled = false;
                        document.querySelector ('#switch-google-account').style.opacity = 1;
                        document.querySelector ('#switch-google-account').disabled = false;
                      } } ); }
  else
  { this.doRealLogin (name); } };

/**
 *
 */
Tootsville.Login.pickCharacter = function (picked)
{ Tootsville.Login.dimUnpickedCharacters (picked);
  document.querySelector ('#edit-toot-settings').style.opacity = .25;
  document.querySelector ('#edit-toot-settings').disabled = true;
  document.querySelector ('#new-toot-hint').style.opacity = .25;
  document.querySelector ('#new-toot-hint').disabled = true;
  document.querySelector ('#switch-google-account').style.opacity = .25;
  document.querySelector ('#switch-google-account').disabled = true;
  Tootsville.Login.playWithCharacter (picked ['data-toot'].name); };

/**
 *
 */
Tootsville.Login.fillGoogleUserInfo = function ()
{ document.getElementById ('google-user-name').innerHTML = Tootsville.Login.player.name;
  document.getElementById ('google-user-email').innerHTML = Tootsville.Login.player.email;
  document.getElementById ('google-user-face').src = Tootsville.Login.player.face;
  if (! Tootsville.player)
  { Tootsville.Util.rest ('GET', '/users/me').then
    ( personInfo =>
      { Tootsville.player = personInfo;
        Tootsville.Login.player.name = personInfo.displayName; } ); } };

/**
 *
 */
Tootsville.Login.switchTootsView = function ()
{ Tootsville.Login.fillGoogleUserInfo ();
  document.getElementById ('pick-toot').style.display = 'block';
  document.getElementById ('sign-in').style.display = 'none';
  document.getElementById ('login-warm-up').style.display = 'none'; };

/**
 *
 */
Tootsville.Login.startSignIn = function ()
{ document.getElementById ('login').style.display = 'block';
  document.getElementById ('sign-in').style.display = 'block';
  document.getElementById ('login-warm-up').style.display = 'none';
  document.getElementById ('pick-toot').style.display = 'none'; };

/**
 *
 */
Tootsville.Login.endLoginMusic = function ()
{ Tootsville.UI.setBackgroundMusic ("bensound-smile"); };

/**
 * Login has completed; clean up and set up for the game.
 */
Tootsville.Login.loginDone = function ()
{ Tootsville.trace ("loginDone");
  Tootsville.Login.endLoginMusic ();
  Tootsville.UI.HUD.closePanel ();
  if ('Rollbar' in window)
  { Rollbar.configure({ payload: { person: { id: Tootsville.characterUUID,
                                             username: Tootsville.character,
                                             email: Tootsville.player && Tootsville.player.eMail }}}); }
  document.title = Tootsville.character + " in Tootsville";
  Tootsville.UI.HUD.refreshHUD (); };

/**
 * Choose a CHARACTER to play as.
 */
Tootsville.Login.serverLinkTokenToCharacter = function (character)
{ Tootsville.Util.infinity ("playWith", { character: character }); };

/**
 * Remove the Child flag from a LI
 */
Tootsville.Login.removeChildFlag = function (li)
{ var child = li.querySelector ('.child');
  if (child) { li.removeChild (child); } };

/**
 * Finds  the list  item representing  a Toot  named NAME  in the  login
 * selection list.
 */
Tootsville.Login.findLIForToot = function (name)
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  for (var i = 0; i < toots.length; ++i)
  { if (toots [i]['data-toot'].name == name)
    { return toots[i]; } }
  throw new Error ("Altered a non-existent Toot"); };

/**
 * Determines whether STRING might be a valid Child Code.
 */
Tootsville.Login.validChildCode = function (string)
{ return (string.match (/^[a-z]{4,6}$/)) ? string : false; };

/**
 * Set NAME to be a Child Toot.
 */
Tootsville.Login.enableChildMode = function (name)
{ let li = this.findLIForToot (name);
  li ['data-toot'].childP = true;
  let code = li.querySelector ('INPUT.child-code');
  if (code) { li ['data-toot'].childCode = code.value; }
  Tootsville.Login.populateTootsList ();
  let childCode = code && this.validChildCode (code.value);
  if (childCode)
  { Tootsville.Util.rest ('PUT', 'toots/' + name,
                          { key: 'childCode', newValue: childCode }); } };

/**
 *
 */
Tootsville.Login.disableChildMode = function (name)
{ let li = this.findLIForToot (name);
  Tootsville.Util.rest ('PUT', 'toots/' + name,
                        { key: 'childCode', newValue: "" });
  li ['data-toot'].childP = false;
  Tootsville.Login.populateTootsList (); };

/**
 *
 */
Tootsville.Login.childSettings = function ()
{ document.querySelector ('#toots-list').style.backgroundColor = '#c4d82d';
  document.querySelector ('#toots-list>#add-toot').style.display = 'none';
  document.querySelector ('#pick-toot>h2').innerHTML = 'Edit Toot Characters';
  document.querySelector ('#pick-toot>p').innerHTML = 'Set up Child options here. Set a 4-to-6-letter code for child logins (like a password). It must be all lower-case letters. <A TARGET="_blank" HREF="https://wiki.tootsville.org/wiki/Child_Sign-in_Code">More info…</A>';
  document.querySelector ('#edit-toot-settings').style.display = 'none';
  document.querySelector ('#new-toot-hint').style.display = 'none';
  document.querySelector ('#edit-toot-settings-done').style.display = 'block'; };

/**
 *
 */
Tootsville.Login.addChildFlag = function (li)
{ var toot = li ['data-toot'];
  if (Tootsville.Login.settingsP)
  { if (toot.childP)
    { li.innerHTML += '<BR><LABEL><INPUT TYPE="checkbox" CHECKED onchange="Tootsville.Login.disableChildMode(' + "'" + toot.name + "'" + ')"><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN></LABEL><BR>Child sign-in code: <INPUT PATTERN="^[a-z]{4,6}$" TYPE="text" SIZE=6 VALUE="' + (toot.childCode || '') + '" CLASS="child-code" onchange="Tootsville.Login.enableChildMode(' + "'" + toot.name + "'" + ')">'; }
    else
    { li.innerHTML += '<BR><LABEL><INPUT TYPE="checkbox" onchange="Tootsville.Login.enableChildMode(' + "'" + toot.name + "'" + ')"><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN></LABEL>'; } }
  else
  { if (toot.childP)
    { li.innerHTML += '<BR><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN>'; } }};

/**
 *
 */
Tootsville.Login.doneEditingSettings = function ()
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  document.querySelector ('#toots-list').style.backgroundColor = '#ba9dca';
  document.querySelector ('#toots-list>#add-toot').style.display =
  ( ((toots.length < 5) || Tootsville.enableMoreTootsP) ? 'block' : 'none');
  document.querySelector ('#pick-toot>h2').innerHTML = 'Pick a Toot Character';
  document.querySelector ('#pick-toot>p').innerHTML = 'Click or tap a character to play now.';
  document.querySelector ('#edit-toot-settings').style.display = 'block';
  document.querySelector ('#new-toot-hint').style.display = 'block';
  document.querySelector ('#edit-toot-settings-done').style.display = 'none'; };

/**
 *
 */
Tootsville.Login.firebaseLogin = function (loginPanel)
{ var ui = new firebaseui.auth.AuthUI(firebase.auth());
  Tootsville.trace ("Starting Firebase login");
  ui.start(
      '#firebaseui-auth-container',
      { callbacks:
        { signInSuccessWithAuthResult: Tootsville.Login.acceptSignedIn,
          uiShown: function()
          { Tootsville.trace ("FirebaseUI NASCAR display ready");
            Tootsville.Login.startSignIn(); }},
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
          firebase.auth.TwitterAuthProvider.PROVIDER_ID,
          /* Twitter does not support scopes. */,
          /* Yahoo! is a generic OAuth source and should work like this … */
          { provider: "yahoo.com",
            providerName: "Yahoo!",
            buttonColor: "#1111dd",
            iconUrl: "https://user-images.githubusercontent.com/49992195/56804289-28d68b00-681d-11e9-8341-e53b89061745.png",
            loginHintKey: "login_hint" }]}); };

/**
 *
 */
Tootsville.Login.acceptSignedIn = function(result)
{ Tootsville.trace ("User signed in");
  Tootsville.Login.storeCredentialInfo (result);
  return false; };

/**
 *
 */
Tootsville.Login.finishSignIn = function (idToken)
{ console.log ("Finishing sign-in");
  Tootsville.Login.firebaseAuth = idToken;
  Tootsville.Gossip.ensureKeyPair ();
  Tootsville.Util.connectWebSocket ();
  Tootsville.Login.switchTootsView(); };

/**
 *
 */
Tootsville.Login.storeCredentialInfo = function (result)
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
  then ((id) => { Tootsville.Login.finishSignIn (id); }).
  catch (function(error) {
      // Handle error TODO
      console.error ("Error from Firebase?", error);
  });

  Tootsville.Login.accessToken = cred.accessToken;
  Tootsville.Login.idToken = cred.idToken;
  Tootsville.Login.idProvider = cred.providerId;
  if (! ("player" in Tootsville.Login)) { Tootsville.Login.player = {}; }
  Tootsville.Login.player.name = user.displayName;
  Tootsville.Login.player.email = (user.email ||
                                   user.providerData[0].email ||
                                   addl.email);
  Tootsville.Login.player.face = (user.photoURL ||
                                  user.providerData[0].photoURL ||
                                  addl.picture);
  Tootsville.Login.player.gender = addl.gender;
  Tootsville.Login.player.locale = addl.locale;

};

/**
 *
 */
Tootsville.Login.quit = function ()
{ Tootsville.Gossip.closeStreams ();
    Tootsville.player = null;
  Tootsville.character = null;
  Tootsville.Login.accessToken = null;
  Tootsville.Login.idToken = null;
  Tootsville.Login.idProvider = null;
  /* XXX there should be a more elegant route than this */
  document.location = Tootsville.host.www;
  firebase.auth().signOut().then(Tootsville.Login.start); };

/**
 * Toggle whether the player is marked as Sensitive or not.
 */
Tootsville.Login.changeSensitivePlayer = function (button)
{ let status = button.checked;
  Tootsville.Util.rest ('PUT', 'users/me', { key: 'sensitiveP', newValue: status ? 'true' : 'false' }).then
  ( personInfo => { Tootsville.player = personInfo;
                    this.populateTootsList (); } ); };
