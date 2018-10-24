var Tootsville = {
    /* Cluster name is hostname minus leading "play" */
    cluster: document.location.host.split('.').splice(1).join('.'),
    Worlds: {
        Tootanga: { Gravity: 9.81 },
        TheMoon: { Gravity: 0.32 },
        TheOtherMoon: { Gravity: 0.12 },
        ThePinkMoon: { Gravity: 0.08 }
    },
    character: null,
    player: null,
    privateKey: null,
    audio: { volume: 100,
             savedVolume: 100 },
    createBoxAvatar: function () {
    },
    brickController: function () {},
    Item: function (json) {
        this.avatar = json.avatar || Tootsville.createBoxAvatar();
        this.controller = Tootsville.brickController();
        this.referenceFrame = json.referenceFrame || null;
        this.name = json.name;
        this.itemP = true;
    },
    Character: function (json) {
        Item.call(this, json);
        this.controller = json.controller;
        this.characterP = true;
    },
    game: {},
    ui: {}
};

document.domain = Tootsville.cluster;

window.addEventListener('Tootsville/Login', function (ev) {
    Rollbar.configure({ payload: { person: { id: Tootsville.player && Tootsville.player.id,
                                             username: Tootsville.character && Tootsville.character.name,
                                             email: Tootsville.player && Tootsville.player.eMail }}});
}, false);

window.loadWatchdog1 = setTimeout(() => {
    if (Tootsville.ui.slowLoadingWatchdog) {
        Tootsville.ui.slowLoadingWatchdog();
    } else {
        setTimeout(window.loadWatchdog1, 300);
    }
}, 5000);
