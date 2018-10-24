/* -*- javascript -*- */
Tootsville.wardrobe = {

    valences: {
        head: [ 'wig', 'hat', 'helmet', 'pivitz' ],
        face: [ 'makeup', 'lashes', 'earring', 'glasses', 'trunkCover', 'trunkHold' ],
        chest: [ 'undershirt', 'dress', 'shirt', 'necklace', /* or necktie */
                 'vest', 'coat', 'scarf', 'overcoat' ],
        legs: [ 'socks', 'pants', 'skirt', 'shoes' ],
        leftArm: [ 'leftBracelet' ],
        rightArm: [ 'rightBracelet' ],
        unPhysical: [ 'readied' ]
    },

    valenceConflicts: {
        hat: ['helmet'],
        dress: ['shirt', 'pants', 'skirt'],
        helmet: ['hat'],
        shirt: ['dress'],
        pants: ['skirt', 'dress'],
        skirt: ['pants', 'dress']
    },

    findBaseSlot: function (slot)
    { for (var baseSlot in Tootsville.wardrobe.valences)
      { for (var j = 0; j < Tootsville.wardrobe.valences[ baseSlot ].length; ++j)
        { if (slot == Tootsville.wardrobe.valences[ baseSlot ][ j ])
          { return baseSlot; }}}}
    
    don: function (item, slot)
    { if (null == Tootsville.player || Tootsville.player.wearingP (item))
      { return; }
      if (null == slot || undefined == slot)
      { slot = item.valenceSlot; }
      if (null != Tootsville.player.wearing[ slot ])
      { Tootsville.wardrobe.doff (item); }
      var valences = Tootsville.wardrobe.valences[ Tootsville.wardrobe.findBaseSlot (slot) ];
      for (var i = 0; i < valences.length; ++i)
      { }
      /* TODO */
    },

    ready: function (item) {
        if (null == Tootsville.player || Tootsville.player.readiedP (item))
        { return; }
        if (Tootsville.player.wearingP (item))
        { Tootsville.wardrobe.doff (item); }
        Tootsville.wardrobe.don (item, 'readied');
    },

    doff: function (item) {
        if (null == Tootsville.player || (! Tootsville.player.wearingP(item)))
        { return; }
        /* TODO */
    },

    wearing: function () {
        /* TODO */
    },

    wearingP: function (item) {
        /* TODO */
    }

    refresh: function () {
        /* TODO */
    },

    proposeExchange: function(tradePartner, offerItems, demandItems) {
        if (! Tootsville.character) return; // XXX assert?
        return { exchange: {
            offers: [
                {
                    from: Tootsville.character.name,
                    items: Array.map(offerItems, Tootsville.Item.publicInfo)
                },
                {
                    from: tradePartner,
                    items: demandItems
                }
            ]
        } };
    },

    signExchange: function(exchangePacket) {
        exchangePacket.signedBy[Tootsville.character.name] = {
            when: performance.now(),
            sign: 'TODO'
        };
        return exchangePacket;
    },

    finalizeExchange: function(exchangePacket) {
        /* TODO */
        return false;
    },

    drop: function(item) {
        /* TODO */
        return false;
    },

    take: function(item) {
        /* TODO */
        return false;
    },

    inventory: function() {
        /* TODO */
        return [];
    },

    inventoryByKind: function(kind) {
        /* TODO */
        return [];
    }
};
