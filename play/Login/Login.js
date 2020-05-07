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
 * Actually instantiate  a decorated Toot  List Item with  avatar, note,
 * and child flag.
 *
 * This is called after Toot Info has been fetched from the server.
 */
Tootsville.Login.createTootListItem2 = function (li, toot)
{ li.innerHTML = '';
  li ['data-toot'] = toot;
  li.className = 'toot';
  if (toot.avatar && false /* XXX */ )
  { const canvas = document.createElement ('CANVAS');
    canvas.width = 96;
    canvas.height = 96;
    li.appendChild (canvas);
    /* Create AvatarViewer asynchronously */
    setTimeout ( () => { console.debug ("Drawing avatar " + toot.name + " in a canvas");
                         Tootsville.AvatarViewer.createViewerInCanvas (toot, canvas); },
                 10 ); }
  li.innerHTML += '<SPAN CLASS="toot-name">' +
  toot.name + '</SPAN><SPAN CLASS="note">' + (toot.note || "") + '</SPAN>';
  Tootsville.Login.addChildFlag (li); };

/**
 *
 */
Tootsville.Login.createTootListItem = function (toot)
{ var li = document.createElement ('LI');
  if (! Tootsville.Login.settingsP)
  { li.onclick = function () { Tootsville.Login.pickCharacter (li); }; }
  Tootsville.Login.createTootListItem2 (li, toot);
  return li; };

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
{ Tootsville.player = null;
  Tootsville.character = null;
  Tootsville.Login.accessToken = null;
  Tootsville.Login.idToken = null;
  Tootsville.Login.idProvider = null;
  /* XXX there should be a more elegant route than this */
  document.location = 'https://Tootsville.org/';
  firebase.auth().signOut().then(Tootsville.Login.start); };

/**
 *
 */
Tootsville.Login.avatarSVG = '<svg xmlns="http://www.w3.org/2000/svg" width="15mm" height="15mm" viewBox="0 0 250 250" version="1.1" id="UltraToot-2D"><defs id="defs2"/><g id="layer1" transform="translate(0,-47)"><path style="fill:#ccc;fill-opacity:1;fill-rule:evenodd;stroke:#030303;stroke-width:2;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 208.64285,39.22024 c 0,0 9.07143,11.339286 9.44941,25.702381 0.37798,14.363094 -3.40179,39.687499 -17.00893,57.452379 -13.60714,17.76488 -16.25297,21.16667 -32.88393,30.99405 -16.63095,9.82738 -19.27678,10.20535 -19.27678,10.20535 l 10.20535,9.07143 c 0,0 42.33334,15.49703 46.49108,17.38691 4.15773,1.88988 20.41071,7.18155 25.70237,10.58333 5.29167,3.40179 13.60715,11.71726 13.60715,11.71726 0,0 -13.60715,-9.4494 -31.75,4.91369 -18.14286,14.3631 -8.69345,25.32441 -8.69345,25.32441 0,0 -8.30761,-4.94056 -13.40472,-8.89588 -4.29532,-5.29166 -9.76254,-10.19749 -12.03039,-10.19749 -2.26786,0 -8.20477,-2.45127 -8.20477,-2.45127 l -6.04762,79.37499 -62.74405,1.51191 c 0,0 3.02381,-6.80357 2.26786,-9.07143 -0.75596,-2.26786 -5.29167,-4.53571 -5.29167,-4.53571 l -3.02381,9.82738 -53.672617,1.88988 -3.779761,-5.66964 -0.755954,-41.57738 c 0,0 -2.645833,-2.64584 -2.267858,-8.31548 0.377979,-5.66964 -0.755951,-6.4256 -0.755951,-6.4256 0,0 -20.78869,0 -25.3244043,-2.64583 -4.5357142,-2.64583 -18.8988094,-7.55952 -18.8988094,-7.55952 0,0 4.1577381,-24.19048 9.07142852,-30.61607 4.91369038,-6.4256 23.81250018,-21.16667 23.81250018,-21.16667 0,0 4.535715,-16.25298 6.80357,-21.92262 2.267857,-5.66964 6.425596,-12.85119 6.425596,-12.85119 l -3.779763,-5.66964 c 0,0 -19.276784,-14.3631 -25.3244032,-33.26191 C 1.5119047,83.443454 -0.75595238,79.663693 -4.1577381,61.898811 -7.5595238,44.13393 9.8273808,27.502978 9.8273808,27.502978 Z" id="skin"/><path style="fill:#000000;fill-opacity:0.99607843;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 131.53571,47.535716 c 0,0 21.92262,13.985119 27.21429,26.458332 5.29166,12.473215 14.74107,37.419642 10.58333,51.404762 -4.15774,13.98512 -14.36309,30.23809 -14.36309,30.23809 0,0 7.73479,-33.61738 6.42559,-49.51487 C 158.75,73.994048 142.875,57.741073 123.97619,49.803573 105.07738,41.866073 104.6994,30.148812 104.6994,30.148812 Z" id="path880"/><path style="fill:#000000;fill-opacity:0.99607843;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 181.05059,220.27083 c 0,0 3.40179,-8.69345 7.9375,-11.71726 4.53572,-3.02381 13.22917,-4.15774 13.22917,-4.15774 0,0 -9.4494,5.66965 -12.47321,8.69346 -3.02381,3.0238 -8.69346,7.18154 -8.69346,7.18154 z" id="path904"/><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 23.434524,176.04762 c 0,0 -9.827381,33.2619 -9.827381,38.55357 v 12.47321 c 0,0 16.252975,4.53572 27.592262,3.77977 11.339285,-0.75596 29.860118,-5.29167 29.860118,-5.29167 0,0 8.315476,-39.30952 10.583333,-46.49107 2.267858,-7.18155 11.717261,-29.48214 14.363094,-30.61607 2.645834,-1.13393 -1.133927,10.20535 -1.133927,10.20535 0,0 -13.985118,75.97322 -13.985118,79.75298 0,3.77976 -6.047621,25.3244 3.401785,31.37202 9.449403,6.04762 20.78869,13.98512 35.52976,6.04762 14.74107,-7.9375 27.97024,-16.63095 35.52976,-29.48214 7.55953,-12.85119 15.49703,-25.3244 15.49703,-25.3244 l -6.04762,79.37499 -62.74405,1.51191 -3.02381,-13.60714 -3.02381,9.82738 L 50.270833,297 c 0,0 -7.181547,-38.55357 -12.473215,-44.22321 -5.291668,-5.66964 -3.023809,-14.74108 -3.023809,-14.74108 0,0 -11.339285,1e-5 -16.252976,1e-5 -4.91369,0 -27.9702377,-10.20536 -27.9702377,-10.20536 0,0 -4.3286033,-16.52943 6.9981224,-28.19165 C 5.486407,191.4659 23.434524,176.04762 23.434524,176.04762 Z" id="path950"/><ellipse style="fill:#000000;fill-opacity:1;stroke:#000000;stroke-width:2;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill" id="path972" cx="107.53423" cy="107.06697" rx="4.7247024" ry="6.2366071"/><ellipse ry="6.2366071" rx="4.7247024" cy="104.79911" cx="42.522327" id="ellipse974" style="fill:#000000;fill-opacity:1;stroke:#000000;stroke-width:2;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill"/><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 104.60491,83.537947 c 1.6064,-1.039434 6.99256,-0.944941 6.99256,-0.944941 0,0 3.68527,-0.188986 5.10268,3.496281 1.41741,3.685268 0.66146,6.142112 2.74032,6.236608 2.07888,0.09449 3.68527,-0.850448 3.40179,-2.456847 -0.28348,-1.606399 -2.07887,-7.748511 -4.44122,-9.35491 -2.36235,-1.606399 -6.64754,-3.562283 -10.86682,-2.834822 -5.48065,0.944941 -7.46502,2.929315 -8.031989,3.968749 -0.566965,1.039437 -1.41741,2.362354 -0.472471,3.401788 0.94494,1.039434 3.59077,0.755952 4.06325,0.283483 0.47246,-0.472472 1.5119,-1.795389 1.5119,-1.795389 z" id="path976"/><path style="fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:0.26458332px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" d="m 42.144344,86.750745 c 0,0 2.55134,-5.291666 3.874256,-5.858631 1.322917,-0.566965 3.496281,-1.228423 3.779764,-0.377976 0.283479,0.850448 0.661458,2.834823 1.700892,2.55134 1.039434,-0.283482 2.551337,-0.944941 2.83482,-1.889882 0.283482,-0.94494 0.283482,-5.480655 -1.511903,-5.953125 -1.795389,-0.472469 -5.480656,-1.700892 -7.748514,0 -2.267857,1.700893 -6.709076,6.709077 -6.898065,8.409969 -0.188987,1.700896 -0.472469,3.401788 0.850447,3.685271 1.322917,0.283479 3.118303,-0.566966 3.118303,-0.566966 z" id="path978"/><ellipse style="fill:#999;fill-opacity:1;stroke:#000000;stroke-width:2.15044212;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1;paint-order:stroke markers fill" id="hand-pad" cx="309.67404" cy="-98.294289" rx="15.114422" ry="24.175716" transform="matrix(0.44953014,0.89326516,-0.87050699,0.49215606,0,0)"/><path style="fill:#fbf8f8;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-width:1;stroke-linecap:butt;stroke-linejoin:miter;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1" d="m 88.629851,189.92203 c 0,0 17.203479,-0.26727 27.786819,-5.55894 C 127,179.07143 148.92262,163.5744 148.92262,163.5744 l 10.20535,9.07143 35.71876,13.32367 c 0,0 -13.59371,3.27486 -16.61752,14.04718 -3.02381,10.77232 -0.69852,22.75824 -0.69852,22.75824 l -5.3491,-1.21359 c 0,0 -19.47921,29.32557 -24.01493,34.61724 -4.53571,5.29167 -24.00706,30.93246 -45.92967,22.23901 -21.922614,-8.69346 -23.617943,-10.7779 -23.617943,-20.60527 0,-9.82739 10.010804,-67.89028 10.010804,-67.89028 z" id="t-shirt"/></g></svg>';

/**
 * Toggle whether the player is marked as Sensitive or not.
 */
Tootsville.Login.changeSensitivePlayer = function (button)
{ let status = button.checked;
  Tootsville.Util.rest ('PUT', 'users/me', { key: 'sensitiveP', newValue: status ? 'true' : 'false' }).then
  ( personInfo => { Tootsville.player = personInfo;
                    this.populateTootsList (); } ); };
