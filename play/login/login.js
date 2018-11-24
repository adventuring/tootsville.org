if (!("login" in Tootsville)) {
    Tootsville.login = {};
}

Tootsville.login.overlay = function () {
    Tootsville.ui.hud.loadHUDPanel("login")
}

Tootsville.login.start = function () {
    Tootsville.ui.hud.loadHUDPanel("login");
    // TODO
}

Tootsville.login.googleInit = function () {
    gapi.load('auth2', () => {
        gapi.auth2.init().then(
            () => {
                Tootsville.login.googleSignIn();
            },
            (err) => {
                document.getElementById('sign-in').style.display = 'none';
                document.getElementById('google-init-failed-reason').innerHTML = err;
                document.getElementById('google-init-failed').style.display = 'block';
            });
    });
}

Tootsville.login.storeGoogleCredentials = function () {
    document.cookie = 'google-api-token=' + Tootsville.login.googleIDToken + ';path=/;domain=' +
        Tootsville.cluster + ';max-age=' + (365 * 24 * 60 * 60);
}

Tootsville.login.eraseGoogleCredentials = function () {
    document.cookie = 'google-api-token=;path=/;domain=tootsville.org;max-age=0';
}

Tootsville.login.googleSignOut = function () {
    return new Promise(after => {
        Tootsville.login.eraseGoogleCredentials()
        gapi.auth2.getAuthInstance().signOut().then( () => { after(); });
    });
}

Tootsville.login.switchGoogleAccount = function () {
    console.log("Switch Google Account");
    if (window.gapi) {
        Tootsville.login.googleSignOut().then(Tootsville.login.googleInit);
    } else if (window.googleyolo) {
        Tootsville.login.googleYoloSignIn(window.googleyolo);
    }
}

Tootsville.login.drawAvatarOnCanvas = function (avatar,canvas) {
    canvas.height = 128;
    canvas.width = 128;
    canvas.className = 'toot-paperdoll';
    var context = canvas.getContext('2d');
    context.fillStyle = interpretTootColor(avatar.baseColor);
    context.fillRect(0, 0, 128, 128);
    context.font = '64px Acme';
    context.fillStyle = interpretTootColor(avatar.patternColor);
    context.fillText(avatar.pattern, 0, 64);
}

Tootsville.login.clearTootsList = function () {
    var toots = document.querySelectorAll('.toot');
    for (var i = 0; i < toots.length; ++i) {
        toots[i].parentNode.removeChild(toots[i]);
    }
}

Tootsville.login.askWhitney = function (query,params) {
    Tootsville.inform('deprecated', 'Call into askWhitney');
    return Tootsville.util.rest('users',query,params);
}

Tootsville.login.criticalFailure = function (message) {
    Tootsville.error("Login failure", message);
}

Tootsville.login.serverQueryCharacters = function () {
    return new Promise( (finish, reject) => {
        Tootsville.login.rest('users','/users/me/toots', {
            want: [ 'avatar', 'name', 'note', 'child-p', 'sensitive-p' ],
            order: 'last-seen'
        }).then( response => {
            if (response.toots.length == 0) {
                reject();
            } else {
                finish(response.toots);
            }
        }, Tootsville.login.criticalFailure);
    });
}

Tootsville.login.createTootListItem = function (toot) {
    var li = document.createElement('LI');
    li.className = 'toot';
    li['data-toot'] = toot;
    li.onclick = function() { Tootsville.login.pickCharacter(li) };
    var canvas = document.createElement('CANVAS');
    li.appendChild(canvas);
    li.innerHTML += '<span class="toot-name">' +
        toot.name + '</span><span class="note">' + toot.note + '</span>';
    Tootsville.login.addChildOrSensitiveFlag(li);
    return li;
}

Tootsville.login.populateTootsList = function (list) {
    Tootsville.login.clearTootsList();
    for (var i = 0; i < list.length; ++i) {
        var toot = list[i];
        var li = Tootsville.login.createTootListItem(toot);
        document.getElementById('toots-list').appendChild(li);
        var canvas = li.querySelector('canvas');
        Tootsville.login.drawAvatarOnCanvas(toot, canvas);
    }
}

Tootsville.login.startCharacterCreation = function () {
    alert("Character creation disabled");
}

Tootsville.login.loadTootsList = function () {
    Tootsville.login.serverQueryCharacters().then(Tootsville.login.populateTootsList,
                                                  Tootsville.login.startCharacterCreation);
}

Tootsville.login.dimUnpickedCharacters = function (picked) {
    var allItems = document.querySelectorAll('#toots-list li');
    for (var i = 0; i < allItems.length; i++) {
        if (allItems[i] != picked) {
            allItems[i].style.opacity = .25;
            allItems[i].style.filter = 'grayscale(100%)';
        }
    }
}

Tootsville.login.playWithCharacter = function (name) {
    Tootsville.login.serverLinkTokenToCharacter(name).then(() => {
        document.location = Tootsville.host.play + '/play/'; // FIXME start playing
    });
}

Tootsville.login.pickCharacter = function (picked) {
    Tootsville.login.dimUnpickedCharacters(picked);
    document.querySelector('#edit-toot-settings').style.opacity = .25;
    document.querySelector('#new-toot-hint').style.opacity = .25;
    document.querySelector('#switch-google-account').style.opacity = .25;
    Tootsville.login.playWithCharacter(picked['data-toot'].name);
}

Tootsville.login.fillGoogleUserInfo = function () {
    document.getElementById('google-user-name').innerHTML = Tootsville.login.player.name;
    document.getElementById('google-user-email').innerHTML = Tootsville.login.player.email;
    document.getElementById('google-user-face').src = Tootsville.login.player.face;
}

Tootsville.login.switchTootsView = function () {
    Tootsville.login.loadTootsList();
    Tootsville.login.fillGoogleUserInfo();
    document.getElementById('pick-toot').style.display = 'block';
    document.getElementById('sign-in').style.display = 'none';
    document.getElementById('warm-up').style.display = 'none';
}

Tootsville.login.startSignIn = function () {
    document.getElementById('sign-in').style.display = 'block';
    document.getElementById('warm-up').style.display = 'none';
    document.getElementById('pick-toot').style.display = 'none';
}

Tootsville.login.serverLinkTokenToCharacter = function (character) {
    return Tootsville.login.askWhitney('play-with', { character: character });
}

Tootsville.login.googleSignIn = function () {
    if (Tootsville.login.googleUser) {
        Tootsville.login.storeGoogleCredentials();
        Tootsville.login.switchTootsView();
    } else {
        Tootsville.login.startSignIn();
    }
}

Tootsville.login.removeChildOrSensitive = function (li) {
    var child = li.querySelector('.child');
    if (child) { li.removeChild(child); }
    var sensitive = li.querySelector('.sensitive');
    if (sensitive) { li.removeChild(sensitive); }
}

Tootsville.login.appendChildMode = function (li, tag, label, checkedP) {
    var name = '"' + li['data-toot'].name + '/child-mode"';
    li.innerHTML += '<label><input type="radio" onchange="updateChildMode(\'' + li['data-toot'].name +
        '\')" name=' + name +
        (checkedP ? 'checked ' : '') + ' value="' + tag + '"> ' + label + ' </label> <br>';
}

Tootsville.login.appendChildSensitiveRadioSet = function (li) {
    li.innerHTML += '<br clear="left"><hr>';
    Tootsville.login.appendChildMode(li, 'adult', '<i class="fas fa-graduation-cap fa-fw"></i> Adult account',
                                     !(li['data-toot'].childP || li['data-toot'].sensitiveP));
    appendChildMode(li, 'child', '<i class="fas fa-child fa-fw"></i> Child account',
                    li['data-toot'].childP);
    appendChildMode(li, 'sensitive', '<i class="fas fa-chess-queen fa-fw"></i> Sensitive Player account',
                    li['data-toot'].sensitiveP);
}

Tootsville.login.appendChildCodeEntry = function (li) {
    li.innerHTML += '<div class="define-child-code"' +
        (li['data-toot'].childP ? '' : ' style="display: none"' )+
        '>TODO: Child Code entry </div>';
}

Tootsville.login.findLIForToot = function (name) {
    var toots = document.querySelectorAll('#toots-list>.toot');
    for (var i = 0; i < toots.length; ++i) {
        if (toots[i]['data-toot'].name == name) {
            return toots[i];
        }
    }
    throw new Error("Altered a non-existent Toot");
}

Tootsville.login.findSelectedChildMode = function (li) {
    var buttons = li.querySelectorAll('input[type="radio"]');
    for (var i = 0; i < buttons.length; i++) {
        if (buttons[i].checked) {
            return buttons[i].value;
        }
    }
    console.warn ("No radio button checked");
    return 'adult';
}

Tootsville.login.enableChildMode = function (li, name) {
    askWhitney('toot/' + name + '/child-p', { set: true });
    li['data-toot'].childP = true;
    li['data-toot'].sensitiveP = false;
    li.querySelector('.define-child-code').style.display = 'block';
}
Tootsville.login.disableChildMode = function (li, name) {
    askWhitney('toot/' + name + '/child-p', { set: false });
    li['data-toot'].childP = false;
    li.querySelector('.define-child-code').style.display = 'none';
}
Tootsville.login.enableSensitiveMode = function (li, name) {
    askWhitney('toot/' + name + '/sensitive-p', { set: true });
    li['data-toot'].childP = false;
    li['data-toot'].sensitiveP = true;
}
Tootsville.login.disableSensitiveMode = function (li, name) {
    askWhitney('toot/' + name + '/sensitive-p', { set: false });
    li['data-toot'].sensitiveP = false;
}

Tootsville.login.updateChildMode = function (name) {
    var li = findLIForToot(name);
    var mode = findSelectedChildMode(li);
    if (mode == 'child') {
        enableChildMode(li, name);
        disableSensitiveMode(li, name);
        return;
    }
    if (mode == 'sensitive') {
        disableChildMode(li, name);
        enableSensitiveMode(li, name);
        return;
    }
    disableChildMode(li, name);
    disableSensitiveMode(li, name);
}

Tootsville.login.ensureChildSettings = function (li) {
    if (li.querySelector('input[type="radio"]')) { return; }
    li.onclick = undefined;
    li.style.cursor = 'default';
    removeChildOrSensitive(li);
    appendChildSensitiveRadioSet(li);
    appendChildCodeEntry(li);
}

Tootsville.login.childSettings = function () {
    var toots = document.querySelectorAll('#toots-list>.toot');
    for (var i = 0; i < toots.length; ++i) {
        ensureChildSettings(toots[i]);
    }
    document.querySelector('#toots-list').style.backgroundColor = '#c4d82d';
    document.querySelector('#toots-list>#new-toot').style.display = 'none';
    document.querySelector('#pick-toot>h2').innerHTML = 'Edit Toot Characters';
    document.querySelector('#pick-toot>p').innerHTML = 'Set up Child and Sensitive Player options here. Child accounts have a sign-in code. (TODO: link to help)';
    document.querySelector('#edit-toot-settings').style.display = 'none';
    document.querySelector('#new-toot-hint').style.display = 'none';
    document.querySelector('#edit-toot-settings-done').style.display = 'block';
}

Tootsville.login.stripChildSettings = function (li) {
    var buttons = li.querySelectorAll('label');
    for (var i = 0; i < buttons.length; ++i) {
        li.removeChild(buttons[i]);
    }
    var breaks = li.querySelectorAll('br');
    for (var i = 0; i < breaks.length; ++i) {
        li.removeChild(breaks[i]);
    }
    li.removeChild(li.querySelector('hr'));
    li.removeChild(li.querySelector('.define-child-code'));

    li.onclick = function() { pickCharacter(li) };
    addChildOrSensitiveFlag(li);
}

Tootsville.login.addChildOrSensitiveFlag = function (li) {
    var toot = li['data-toot'];
    if (toot.childP) {
        li.innerHTML += '<span class="child"><i class="fas fa-child fa-fw"></i> Child Account</span>';
    }
    if (toot.sensitiveP) {
        li.innerHTML += '<span class="sensitive"><i class="fas fa-chess-queen fa-fw"></i> Sensitive Player</span>';
    }
}

Tootsville.login.doneEditingSettings = function () {
    var toots = document.querySelectorAll('#toots-list>.toot');
    for (var i = 0; i < toots.length; ++i) {
        stripChildSettings(toots[i]);
    }
    document.querySelector('#toots-list').style.backgroundColor = '#ba9dca';
    document.querySelector('#toots-list>#new-toot').style.display = 'block';
    document.querySelector('#pick-toot>h2').innerHTML = 'Pick a Toot Character';
    document.querySelector('#pick-toot>p').innerHTML = 'Click or tap a character to play now.';
    document.querySelector('#edit-toot-settings').style.display = 'block';
    document.querySelector('#new-toot-hint').style.display = 'block';
    document.querySelector('#edit-toot-settings-done').style.display = 'none';
}

Tootsville.login.copyGoogleUserInfo = function () {
    Tootsville.login.googleIDToken = Tootsville.login.googleUser.getAuthResponse().id_token;
    Tootsville.login.storeGoogleCredentials();
    var profile = Tootsville.login.googleUser.getBasicProfile();
    if (! Tootsville.login.player) { Tootsville.login.player = {} ; }
    Tootsville.login.player.name = profile.getName();
    Tootsville.login.player.email = profile.getEmail();
    Tootsville.login.player.face = profile.getImageUrl();
}

Tootsville.login.onSignIn = function (googleUser) {
    Tootsville.login.googleUser = googleUser;

    if (Tootsville.login.googleUser && Tootsville.login.googleUser.isSignedIn()) {
        Tootsville.login.copyGoogleUserInfo();
        Tootsville.login.switchTootsView();
    } else {
        Tootsville.login.startSignIn();
    }
};
