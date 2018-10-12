Tootsville.wardrobe = {

    don: function(item) {
        /* TODO */
    },

    don2: function(item) {
        /* TODO */
    },

    doff: function(item) {
        if (null == Tootsville.player || (! Tootsville.player.wearingP(item)))
        { return; }
        /* TODO */
    },

    wearing: function() {
        /* TODO */
    },

    refresh: function() {
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
