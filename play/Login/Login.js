/* -*- js2 -*- */

/**@license
 *
 * play/Login/Login.js is part of Tootsville
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
      'block' === document.getElementById ("login").style.display)
  { Tootsville.trace ("Restarting login…");
    Tootsville.Login.startSignIn();
    return; }
  Tootsville.inform ("Start login (build crypto keys)");
  document.querySelector('#bugs>h1').innerHTML = 'Loading…';
  setTimeout (Tootsville.Gossip.ensureKeyPair, 1);
  Tootsville.UI.HUD.loadHUDPanel ("login", Tootsville.Login.firebaseLogin); };


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
 *
 * See `INFINITY-TOOT-LIST'
 */
Tootsville.Login.loadTootsList = function ()
{ Tootsville.Util.infinity ("tootList"); };

/**
 * The set of Toot characters available to the player.
 */
if (!('toots' in Tootsville.Login)) { Tootsville.Login.toots = {}; }

/**
 * Create a Toot List item for the given Toot JSON object.
 */
Tootsville.Login.createTootListItem = function (toot)
{ var li = document.createElement ('LI');
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
  let name = document.createElement ('SPAN');
  name.className = 'toot-name';
  name.addEventListener ('click', function ()
                         { Tootsville.Login.pickCharacter (li); });
  name.innerText = toot.name;
  li.appendChild (name);
  let note = document.createElement ("TEXTAREA");
  note.rows = "2";
  note.innerText = toot.note;
  note.addEventListener ('change',
                         event => { Tootsville.Login.updateNote (toot.name, event); });
  note.addEventListener ('click',
                         event => { event.stopPropagation ();});
  li.appendChild (note);
  Tootsville.Login.addChildFlag (li);
  return li; };

/**
 * Update the note attached to tootName
 *
 * An event handler to be connected to the TEXTAREA control.
 */
Tootsville.Login.updateNote = function (tootName, event)
{ Tootsville.Util.rest ('PUT', 'toots/' + tootName,
                        { key: 'note', newValue: event.target.value }); };

/**
 * The Toots List in memory.
 */
Tootsville.Login.toots = {};

/**
 * Save LIST as the Toots List, then rebuild the display.
 */
Tootsville.Login.saveTootsList = function (list)
{ for (let i = 0; i < list.length; ++i)
  { Tootsville.Login.toots [ list [ i ].name ] = list [ i ];}
  Tootsville.Login.populateTootsList (); };

/**
 * Set the sensitive player flag in the UI based upon the settings
 */
Tootsville.Login.setSensitiveP = function ()
{ document.getElementById('sensitive-player-p').checked =
  (Tootsville.player && Tootsville.player.sensitiveP); };

/**
 * Build the Toots List display from the Toots List in memory.
 */
Tootsville.Login.populateTootsList = function ()
{ if (Tootsville.character ||
      ('block' !== document.getElementById ('login').style.display))
    return;
  Tootsville.Login.clearTootsList ();
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
 * Launch the New Toot panel.
 *
 * To     get    the     Gossip     Parrot     prompt    first,     call
 * `Tootsville.Login.startCharacterCreation' instead.
 */
Tootsville.Login.generateNewToot = function ()
{ Tootsville.character = "$new toot";
  Tootsville.UI.HUD.showHUDPanel('new-toot'); };

/**
 * Start the New Toot creation process.
 */
Tootsville.Login.startCharacterCreation = function ()
{ Tootsville.Gossip.Parrot.say ("Let's get started!",
                                "<p>Let's create your Toot character.</p>" +
                                "<p>This takes about a minute. I'll create a new " +
                                "Toot character for you. You can change the name, colors, or " +
                                "pattern to be what you like. </p>").
  then(Tootsville.Login.generateNewToot); };

/**
 * Dim all the Toot characters other than the one who was PICKED.
 */
Tootsville.Login.dimUnpickedCharacters = function (picked)
{ var allItems = document.querySelectorAll ('#toots-list li');
  for (var i = 0; i < allItems.length; i++)
  { if (allItems[i] != picked)
    { allItems[i].style.opacity = .25;
      allItems[i].style.filter = 'grayscale(1)'; } } };

/**
 * Link the character ``name'' to the player and start playing the game
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
                                  "<p> Are you sure you want to sign in with a Child account? </p>" +
                                  "<p> This does not give your child permission to play — it lets you control their character. </p>"+
"<a href=\"https://wiki.Tootsville.org/wiki/Child_login\">Find out how your child can sign in</a>",
                                  [{ tag: "yes", text: "Sign In" },
                                   { tag: "no", text: "Cancel" }]).
    then (answer => { if ('yes' === answer)
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
 * Pick ``picked'' as your Toot to play with today
 */
Tootsville.Login.pickCharacter = function (picked)
{ Tootsville.Login.dimUnpickedCharacters (picked);
  document.querySelector ('#edit-toot-settings').style.opacity = .25;
  document.querySelector ('#edit-toot-settings').disabled = true;
  document.querySelector ('#new-toot-hint').style.opacity = .25;
  document.querySelector ('#new-toot-hint').disabled = true;
  document.querySelector ('#switch-google-account').style.opacity = .25;
  document.querySelector ('#switch-google-account').disabled = true;
  Tootsville.player.activeItem = Tootsville.player.inactiveItem = null;
  Tootsville.Login.playWithCharacter (picked ['data-toot'].name); };

/**
 * Accept information from Google to fill in the display
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
 * Show the view for switching Toot characters
 */
Tootsville.Login.switchTootsView = function ()
{ Tootsville.Login.fillGoogleUserInfo ();
  document.getElementById ('pick-toot').style.display = 'block';
  document.getElementById ('sign-in').style.display = 'none';
  document.getElementById ('login-warm-up').style.display = 'none'; };

/**
 * Start the login/sign-in process
 */
Tootsville.Login.startSignIn = function ()
{ document.getElementById ('login').style.display = 'block';
  document.getElementById ('sign-in').style.display = 'block';
  document.getElementById ('login-warm-up').style.display = 'none';
  document.getElementById ('pick-toot').style.display = 'none'; };

/**
 * Stop playing the login music and start playing game background music.
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
 * Inform the server that we want to play with ``character''
 *
 * See `INFINITY-PLAY-WITH'
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
  { if (toots [i]['data-toot'].name === name)
    { return toots[i]; } }
  throw new Error ("Altered a non-existent Toot"); };

/**
 * Determines whether ``string'' might be a valid Child Code.
 */
Tootsville.Login.validChildCode = function (string)
{ return (string.match (/^[a-z]{4,6}$/)) ? string : false; };

/**
 * Set ``name'' to be a Child Toot.
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
 * Set ``name'' to no longer be a Child Toot.
 */
Tootsville.Login.disableChildMode = function (name)
{ let li = this.findLIForToot (name);
  Tootsville.Util.rest ('PUT', 'toots/' + name,
                        { key: 'childCode', newValue: "" });
  li ['data-toot'].childP = false;
  Tootsville.Login.populateTootsList (); };

/**
 * Enter the child settings mode.
 */
Tootsville.Login.childSettings = function ()
{ document.querySelector ('#toots-list').style.backgroundColor = '#c4d82d';
  document.querySelector ('#toots-list>#add-toot').style.display = 'none';
  document.querySelector ('#pick-toot>h2').innerHTML = 'Edit Toot Characters';
  document.querySelector ('#pick-toot>p').innerHTML = 'Set up Child options here. Set a 6-to-12-letter code for child logins (like a password). <A TARGET="_blank"  HREF="https://wiki.tootsville.org/wiki/Child_Sign-in_Code">More info…</A>';
  document.querySelector ('#edit-toot-settings').style.display = 'none';
  document.querySelector ('#new-toot-hint').style.display = 'none';
  document.querySelector ('#edit-toot-settings-done').style.display = 'block'; };

/**
 * Pretty-print the time remaining for a child request
 */
Tootsville.Login.childRequestTimeLeft = function (request)
{ let remainMsec = ( (request.allowedUntil * 1000) -
                     (new Date ().getTime ()) );
  let remainMin = Math.floor (remainMsec / (60 * 1000));
  let remainHour = Math.floor (remainMin / 60);
  remainMin = remainMin % 60;
  let remainder = remainMin;
  if (remainHour > 1)
  { remainder = remainHour + " hours, " + remainder; }
  else if (remainHour > 0)
  { remainder = remainHour + " hour, " + remainder; }
  return remainder; };

/**
 * Add information to a Toot List item about a Child Request.
 *
 * When the child Toot has an outstanding request, this shows whether it
 * has been  granted or denied,  and if granted,  for how long,  and how
 * much of that time remains.
 *
 * Includes the ability to answer (or  change the answer of) the request
 * by triggering a server prompt.
 *
 */
Tootsville.Login.addChildRequest = function (li, request)
{ let div = document.createElement ('DIV');
  let answeredP = false;
  if ( request.deniedAt )
  { answeredP = true;}
  else if ( request.allowedAt )
  { let remainder = Tootsville.Login.childRequestTimeLeft (request);
    div.innerHTML += "<p> Allowed for " + request.allowedFor + " hours, with " +
    remainder + " minutes left; " +
    request.response + "</p>";
    answeredP = true;}
  else
  { div.innerHTML += "<p> Requested to play. </p>"; }
  let response = document.createElement ('BUTTON');
  if (answeredP)
  { response.innerText = "Change Response"; }
  else
  { response.innerText = "Respond to Request"; }
  response.addEventListener ('click',
                             event => { Tootsville.Login.considerChildApproval (request.uuid);
                                        event.stopPropagation (); });
  div.appendChild (response);
  li.appendChild (div);
  return li; };

/**
 * Ask the server to re-prompt us for the Child Request with ``UUID''.
 *
 * The server will send a "prompt" packet down immediately.
 */
Tootsville.Login.considerChildApproval = function (uuid)
{ Tootsville.Util.infinity ('considerChildApproval', { uuid: uuid }); };

/**
 * Add to LI the child settings flag.
 * 
 * Takes into account if we're in child settings mode or just displaying it.
 */
Tootsville.Login.addChildFlag = function (li)
{ let toot = li ['data-toot'];
  if (Tootsville.Login.settingsP)
  { if (toot.childP)
    { li.innerHTML += '<BR><LABEL><INPUT TYPE="checkbox" CHECKED onchange="Tootsville.Login.disableChildMode(' + "'" + toot.name + "'" + ')"><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN></LABEL><BR>Child sign-in code: <INPUT PATTERN="^[a-z]{4,6}$" TYPE="text" SIZE=6 VALUE="' + (toot.childCode || '') + '" CLASS="child-code" onchange="Tootsville.Login.enableChildMode(' + "'" + toot.name + "'" + ')">'; }
    else
    { li.innerHTML += '<BR><LABEL><INPUT TYPE="checkbox" onchange="Tootsville.Login.enableChildMode(' + "'" + toot.name + "'" + ')"><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN></LABEL>'; } }
  else
  { if (toot.childP)
    { li.innerHTML += '<BR><SPAN CLASS="child"><I CLASS="fas fa-child fa-fw"></I> Child Account</SPAN>';
     if (toot.childRequest && toot.childRequest.uuid)
      { console.log ("request for child " + toot.userName);
        Tootsville.Login.addChildRequest (li, toot.childRequest); }
      else
      { console.log ("no request for child " + toot.userName); } }}} ;

/**
 * Leave the Child Settings mode; return to login selection
 */
Tootsville.Login.doneEditingSettings = function ()
{ var toots = document.querySelectorAll ('#toots-list>.toot');
  document.querySelector ('#toots-list').style.backgroundColor = '#ba9dca';
  document.querySelector ('#toots-list>#add-toot').style.display =
  ( ((toots.length < 5) || Tootsville.enableMoreTootsP) ? 'block' : 'none');
  document.querySelector ('#pick-toot>h2').innerHTML = 'Pick a Toot Character';
  document.querySelector ('#pick-toot>p').innerHTML = "Click or tap a character's name to play now.";
  document.querySelector ('#edit-toot-settings').style.display = 'block';
  document.querySelector ('#new-toot-hint').style.display =
  ( ((toots.length < 5) || Tootsville.enableMoreTootsP) ? 'block' : 'none');
  document.querySelector ('#edit-toot-settings-done').style.display = 'none'; };

/**
 * Start the Firebase login system
 */
Tootsville.Login.firebaseLogin = function (loginPanel)
{ var ui = new firebaseui.auth.AuthUI(firebase.auth());
  Tootsville.trace ("Starting Firebase login");
  
  let yahoo = new firebase.auth.OAuthProvider('yahoo.com');
  yahoo.setCustomParameters({ prompt: 'login' });
  document.getElementById ('login-13').innerHTML += `
<center>
  <button id="firebase-sign-in-with-yahoo">Sign in with Yahoo!</button>
</center>`;
  document.getElementById ('firebase-sign-in-with-yahoo').addEventListener (
      'click',
      function () {
          firebase.auth().signInWithPopup (yahoo).then
          (result => {
              console.info ("Got Yahoo! login", result);
              Tootsville.Login.storeCredentialInfo (result);
          },
           error => {
              console.error ("Error with Yahoo! login", result);
              alert ("Error with Yahoo! login");
          }); });
 
  ui.start(
      '#firebaseui-auth-container',
      { callbacks:
        { signInSuccessWithAuthResult: Tootsville.Login.acceptSignedIn,
          uiShown: Tootsville.Login.startSignIn },
        // Will use popup for IDP Providers sign-in flow instead of the default, redirect.
        signInFlow: 'popup',
        tosUrl: 'https://wiki.tootsville.org/wiki/Core:ToS',
        privacyPolicyUrl: 'https://wiki.tootsville.org/wiki/Core:Privacy',
        signInOptions:
        [ { provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
            scopes:
            [ 'https://www.googleapis.com/auth/plus.login' ],
            customParameters:
            /* Forces account selection even when one account
             is available. */
            { prompt: 'select_account' } },
          firebase.auth.TwitterAuthProvider.PROVIDER_ID,
          /* Yahoo! is a generic OAuth source and should work like this … */
          { provider: "yahoo.com",
            providerName: "Yahoo!",
            buttonColor: "#1111dd",
            iconUrl: "https://user-images.githubusercontent.com/49992195/56804289-28d68b00-681d-11e9-8341-e53b89061745.png",
            loginHintKey: "login_hint" }]}); };

/**
 * Callback for Firebase completing authentication
 */
Tootsville.Login.acceptSignedIn = function(result)
{ Tootsville.trace ("User signed in");
  Tootsville.Login.storeCredentialInfo (result);
  document.getElementById('login-kid').style.display = 'none';
  document.getElementById('login-13').style.display = 'none';
  document.getElementById('sign-in').style.display = 'none';
  document.getElementById('login-ready-13').style.display = 'block';
  return false; };

/**
 * After signing in, begin choosing a Toot and connect networking.
 */
Tootsville.Login.finishSignIn = function (idToken)
{ console.log ("Finishing sign-in");
  Tootsville.Login.firebaseAuth = idToken;
  Tootsville.Gossip.ensureKeyPair ();
  Tootsville.Util.connectWebSocket ();
  Tootsville.Login.switchTootsView(); };

/**
 * Get credential information back from Firebase
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
 * Quit the game
 */
Tootsville.Login.quit = function ()
{ Tootsville.Gossip.closeStreams ();
  Tootsville.Tank.shutDown ();
  Tootsville.player = null;
  Tootsville.character = null;
  Tootsville.childCode = null;
  Tootsville.Login.accessToken = null;
  Tootsville.Login.idToken = null;
  Tootsville.Login.idProvider = null;
  firebase.auth().signOut().then(Tootsville.Login.start); };

/**
 * Toggle whether the player is marked as Sensitive or not.
 */
Tootsville.Login.changeSensitivePlayer = function (button)
{ let status = button.checked;
  Tootsville.Util.rest ('PUT', 'users/me', { key: 'sensitiveP', newValue: status ? 'true' : 'false' }).then
  ( personInfo => { Tootsville.player = personInfo;
                    this.populateTootsList (); } ); };

/**
 *
 */
Tootsville.Login.loginKidDirty = function (item)
{ if (document.getElementById ('toot-name').value.length > 2 &&
      document.getElementById ('toot-code').value.length > 2 &&
      document.getElementById ('toot-name').value.length < 33 &&
      document.getElementById ('toot-code').value.length < 33)
  { document.getElementById ('kid-login-submit').style.disabled = false; }
  else
  { document.getElementById ('kid-login-submit').style.disabled = true; } };

/**
 *
 */
Tootsville.Login.loginKidDone = function (button)
{ let name = document.getElementById ('toot-name').value;
  let code = document.getElementById ('toot-code').value;
  Tootsville.Util.rest ('POST', '/login/child', { name: name, code: code }).then (
      reply => { console.log ("Child login reply ", reply);
                 if (reply.error)
                 { reply.response.json ().then (
                     body => { Tootsville.Gossip.Parrot.say ("Problem signing in",
                                                             body.error || "Make sure your Toot name and code are right"); } ); }
                 else
                 { Tootsville.character = name;
                   Tootsville.childCode = code.toLowerCase ();
                   Tootsville.Gossip.ensureKeyPair ();
                   Tootsville.Util.connectWebSocket ();} }); };
