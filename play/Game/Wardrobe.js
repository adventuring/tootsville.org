/* -*- js2 -*-*/

/**@license
 *
 * play/Game/Wardrobe.js is part of Tootsville
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

if (!('Game' in Tootsville)) Tootsville.Game = {};
if (!('Wardrobe' in Tootsville.Game)) Tootsville.Game.Wardrobe = {};

/**
 * Clothing  valences  and  conflicts,   encoded  for  Javascript  form.
 * Compare to `WEAR-SLOT', which  should be the canonical representation
 * and used to populate this.
 */
Tootsville.Game.Wardrobe.valences = {
    head: [ 'wig', 'hat', 'helmet', 'pivitz' ],
    face: [ 'makeup', 'lashes', 'earring', 'glasses', 'trunkCover', 'trunkHold' ],
    chest: [ 'undershirt', 'dress', 'shirt', 'necklace', /* or necktie */
             'vest', 'coat', 'scarf', 'overcoat' ],
    legs: [ 'socks', 'pants', 'skirt', 'shoes' ],
    leftArm: [ 'leftBracelet' ],
    rightArm: [ 'rightBracelet' ],
    unPhysical: [ 'readied' ]
};

Tootsville.Game.Wardrobe.valenceConflicts = {
    hat: ['helmet'],
    dress: ['shirt', 'pants', 'skirt'],
    helmet: ['hat'],
    shirt: ['dress'],
    pants: ['skirt', 'dress'],
    skirt: ['pants', 'dress']
};

/**
 * Find a base slot in the 3D model for clothing to mount.
 */
Tootsville.Game.Wardrobe.findBaseSlot = function (slot)
{ for (var baseSlot in Tootsville.Game.Wardrobe.valences)
  { for (var j = 0; j < Tootsville.Game.Wardrobe.valences[ baseSlot ].length; ++j)
    { if (slot === Tootsville.Game.Wardrobe.valences[ baseSlot ][ j ])
      { return baseSlot; }}}
  return null; };


/**
 * Don an article of clothing on a wear slot.
 */
Tootsville.Game.Wardrobe.don = function (item, slot=null)
{ if (Tootsville.Game.Wardrobe.wearingP (item))
  { return; }
  if (!(slot)) slot = item.valenceSlot;
  if (null !== Tootsville.player.wearing[ slot ])
  { Tootsville.Game.Wardrobe.doff (item); }
  var valences = Tootsville.Game.Wardrobe.valences[ Tootsville.Game.Wardrobe.findBaseSlot (slot) ];
  for (var i = 0; i < valences.length; ++i)
  { }
  /* TODO */
};

/**
 *
 */
Tootsville.Game.Wardrobe.ready = function (item) {
    if (Tootsville.Game.Wardrobe.readiedP (item))
    { return; }
    if (Tootsville.Game.Wardrobe.wearingP (item))
    { Tootsville.Game.Wardrobe.doff (item); }

    Tootsville.Game.Wardrobe.don (item, 'readied');
};


/**
 *
 */
Tootsville.Game.Wardrobe.readied = function () {
    return Tootsville.Game.Wardrobe.wearingAs ('readied');
};


/**
 *
 */
Tootsville.Game.Wardrobe.readiedP = function (item) {
    return Tootsville.Game.Wardrobe.readied () === item;
};


/**
 *
 */
Tootsville.Game.Wardrobe.doff = function (item) {
    if (! Tootsville.Game.Wardrobe.wearingP(item))
    { return; }
    /* TODO */
};


/**
 *
 */
Tootsville.Game.Wardrobe.wearing = function () {
    if (! Tootsville.player) { return []; }
    var set = {};
    for (var i = 0; i < Tootsville.Game.Wardrobe.valences; ++i)
    { for (var j = 0; j < Tootsville.Game.Wardrobe.valences[i].length; ++j)
      { var slot = Tootsville.Game.Wardrobe.valences[i][j];
        var equipped;
        if (equipped = Tootsville.player.equipped(slot))
        { set[ slot ] = equipped; }}}
    return set; };


/**
 *
 */
Tootsville.Game.Wardrobe.wearingP = function (item) {

};


/**
 *
 */
Tootsville.Game.Wardrobe.refresh = function () {
    /* TODO */
};


/**
 * Propose to exchange an item with a partner.
 *
 * Offer   @code{offerItems}    to   @code{tradePartner},    in   return
 * for @code{demandItems}.
 */
Tootsville.Game.Wardrobe.proposeExchange = function(tradePartner, offerItems, demandItems) {
    if (!('character' in Tootsville)) return {}; // XXX assert?
    return { exchange: {
        offers: [
            {
                from: Tootsville.character,
                items: Array.map(offerItems, Tootsville.Item.publicInfo)
            },
            {
                from: tradePartner,
                items: demandItems
            } ] } }; };


/**
 * Sign an exchange packet
 */
Tootsville.Game.Wardrobe.signExchange = function(exchangePacket) {
    exchangePacket.signedBy[Tootsville.character.name] = {
        when: performance.now(),
        sign: 'TODO'
    };
    return exchangePacket;
};


/**
 *
 */
Tootsville.Game.Wardrobe.finalizeExchange = function(exchangePacket) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.Game.Wardrobe.drop = function(item) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.Game.Wardrobe.take = function(item) {
    /* TODO */
    return false;
};


/**
 *
 */
Tootsville.Game.Wardrobe.inventory = function() {
    /* TODO */
    return [];
};


/**
 *
 */
Tootsville.Game.Wardrobe.inventoryByKind = function(kind) {
    /* TODO */
    return [];
};
