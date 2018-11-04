var Tootsville = {
    /* Cluster name is hostname minus leading "play" */
    cluster: document.location.host.split('.').splice(1).join('.'),
    host:
    { www: 'https://www.' + Tootsville.cluster,
      play: 'https://play' + Tootsville.cluster,
      users: 'https://users' + Tootsville.cluster.replace('.org','.net'),
      gossip: 'https://gossip' + Tootsville.cluster.replace('.org','.net'),
      world: 'https://world' + Tootsville.cluster.replace('.org','.net') },
    Worlds:
    { Tootanga: { Gravity: 9.81 },
      TheMoon: { Gravity: 0.32 },
      TheOtherMoon: { Gravity: 0.12 },
      ThePinkMoon: { Gravity: 0.08 } },
    character: null,
    privateKey: null,
    audio: { volume: 80,
             savedVolume: 80 },
    createBoxAvatar: function () {},
    brickController: function () {},
    Item: function (json)
    { this.avatar = json.avatar || Tootsville.createBoxAvatar();
      this.controller = Tootsville.brickController();
      this.referenceFrame = json.referenceFrame || null;
      this.name = json.name;
      this.itemP = true; },
    Character: function (json)
    { Item.call(this, json);
      this.controller = json.controller;
      this.characterP = true; },
    game: {},
    ui: {} };

if (! (('test.tootsville.org' == Tootsville.cluster) ||
      ('qa.tootsville.org' == Tootsville.cluster) ||
       ('tootsville.org' == Tootsville.cluster)))
{ Tootsville.cluster = 'local';
  Tootsville.host.www = 'http://localhost:5001';
  Tootsville.host.play = 'http://localhost:5002';
  Tootsville.host.users = 'http://localhost:5000';
  Tootsville.host.gossip = 'http://localhost:5000';
  Tootsville.host.world = 'http://localhost:5000'; }

document.domain = Tootsville.cluster;

window.addEventListener('Tootsville/Login', function (ev)
 { Rollbar.configure({ payload: { person: { id: Tootsville.player && Tootsville.player.id,
                                            username: Tootsville.character && Tootsville.character.name,
                                            email: Tootsville.player && Tootsville.player.eMail }}}); },
                        false);

window.loadWatchdog1 = setTimeout(() => {
    if (Tootsville.ui.slowLoadingWatchdog)
        Tootsville.ui.slowLoadingWatchdog();
    else 
        setTimeout(window.loadWatchdog1, 300); },
                                  5000);
