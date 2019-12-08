/* -*- js2 -*-*/

/**@license
 *
 * ./play/game/wardrobe.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
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

if (!('Tootsville' in window)) { Tootsville = {}; }

/**
 * Clothing  valences  and  conflicts,   encoded  for  Javascript  form.
 * Compare to `WEAR-SLOT', which  should be the canonical representation
 * and used to populate this.
 */
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
    } };

/**
 * Find a base slot in the 3D model for clothing to mount.
 */
Tootsville.wardrobe.findBaseSlot = function (slot)
{ for (var baseSlot in Tootsville.wardrobe.valences)
  { for (var j = 0; j < Tootsville.wardrobe.valences[ baseSlot ].length; ++j)
    { if (slot == Tootsville.wardrobe.valences[ baseSlot ][ j ])
      { return baseSlot; }}}
  return null; };


/**
 * Don an article of clothing on a wear slot.
 */
Tootsville.wardrobe.don = function (item, slot)
{ if (Tootsville.wardrobe.wearingP (item))
  { return; }
  if (null == slot || undefined == slot)
  { slot = item.valenceSlot; }
  if (null != Tootsville.player.wearing[ slot ])
  { Tootsville.wardrobe.doff (item); }
  var valences = Tootsville.wardrobe.valences[ Tootsville.wardrobe.findBaseSlot (slot) ];
  for (var i = 0; i < valences.length; ++i)
  { }
  /* TODO */
};

/**
 *
 */
Tootsville.wardrobe.ready = function (item) {
    if (Tootsville.wardrobe.readiedP (item))
    { return; }
    if (Tootsville.wardrobe.wearingP (item))
    { Tootsville.wardrobe.doff (item); }

    Tootsville.wardrobe.don (item, 'readied');
};


/**
 *
 */
Tootsville.wardrobe.readied = function () {
    return Tootsville.wardrobe.wearingAs ('readied');
};


/**
 *
 */
Tootsville.wardrobe.readiedP = function (item) {
    return Tootsville.wardrobe.readied () == item;
};


/**
 *
 */
Tootsville.wardrobe.doff = function (item) {
    if (! Tootsville.wardrobe.wearingP(item))
    { return; }
    /* TODO */
};


/**
 *
 */
Tootsville.wardrobe.wearing = function () {
    if (! Tootsville.player) { return []; }
    var set = {};
    for (var i = 0; i < Tootsville.wardrobe.valences; ++i)
    { for (var j = 0; j < Tootsville.wardrobe.valences[i].length; ++j)
      { var slot = Tootsville.wardrobe.valences[i][j];
        var equipped;
        if (equipped = Tootsville.player.equipped(slot))
        { set[ slot ] = equipped; }}}
    return set; };


/**
 *
 */
Tootsville.wardrobe.wearingP = function (item) {

};


/**
 *
 */
Tootsville.wardrobe.refresh = function () {
    /* TODO */
};


/**
 * Propose to exchange an item with a partner.
 *
 * Offer   @code{offerItems}    to   @code{tradePartner},    in   return
 * for @code{demandItems}.
 */
Tootsville.wardrobe.proposeExchange = function(tradePartner, offerItems, demandItems) {
    if (!('character' in Tootsville)) return {}; // XXX assert?
    return { exchange: {
        offers: [
            {
                from: Tootsville.character.name,
                items: Array.map(offerItems, Tootsville.Item.publicInfo)
            },
            {
                from: tradePartner,
                items: demandItems
            } ] } }; };


/**
 * Sign an exchange packet
 */
Tootsville.wardrobe.signExchange = function(exchangePacket) {
    exchangePacket.signedBy[Tootsville.character.name] = {
        when: performance.now(),
        sign: 'TODO'
    };
    return exchangePacket;
};


/**
 *
 */
Tootsville.wardrobe.finalizeExchange = function(exchangePacket) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.wardrobe.drop = function(item) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.wardrobe.take = function(item) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.wardrobe.inventory = function() {
    /* TODO */
    return [];
};


/**
 *
 */
Tootsville.wardrobe.inventoryByKind = function(kind) {
    /* TODO */
    return [];
};
