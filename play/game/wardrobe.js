Tootsville.wardrobe = {

    don: function(item) {
        /* TODO */
    },

    don2: function(item) {
        /* TODO */
    },

    doff: function(item) {
        if (! Tootsville.player.wearingP(item)) { return; }
        /* TODO */
    },

    wearing: function() {
        /* TODO */
    },

    refresh: function() {
        /* TODO */
    },

    proposeExchange: function(tradePartner, offerItems, demandItems) {
        return { exchange: {
            offers: [
                {
                    from: Tootsville.charName,
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
        exchangePacket.signedBy[Tootsville.charName] = {
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
