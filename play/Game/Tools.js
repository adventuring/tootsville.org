/* -*- js2 -*- */
/* @license
 * play/Game/Tools.js is part of Tootsville
 *
 * Copyright © 2008-2017, Bruce-Robert  Pocock; Copyright © 2018-2021,
 * the Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software: you can redistribute it and/or modify
 * it  under the  terms of  the GNU  Affero General  Public License  as
 * published by the  Free Software Foundation; either version  3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the  hope that it will be useful, but
 * WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
 * Affero General Public License for more details.
 *
 * You should  have received a  copy of  the GNU Affero  General Public
 * License    along     with    this     program.    If     not,    see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 */

if (!('Game' in Tootsville)) { Tootsville.Game = {}; };
if (!('Tools' in Tootsville.Game)) { Tootsville.Game.Tools = {}; };

/**
 * Use a shovel on the given item.
 *
 * A shovel creates a hole, and may reveal any hidden object buried
 * there.
 *
 * Using a shovel can also fill a hole, hiding any objects in the
 * hole.
 */
Tootsville.Game.Tools.shovel = function (item, x, y, z)
{ /* TODO shovel */};

/**
 * Use an axe on the given item.
 *
 * Cut a branch or tree trunk.
 */
Tootsville.Game.Tools.axe = function (item, x, y, z)
{ /* TODO axe */};

/**
 * Use a pickaxe on the given item.
 *
 * Break apart a rock into smaller stones.
 */
Tootsville.Game.Tools.pickaxe = function (item, x, y, z)
{ /* TODO pickaxe */};

/**
 * Use a wrench on the given item.
 *
 * Given some building materials, combine them into a constructed
 * object.
 */
Tootsville.Game.Tools.wrench = function (item, x, y, z)
{ /* TODO hammer */ };

/**
 * Use a sewing kit on the given item.
 *
 * Used to create clothing from cloth.
 */
Tootsville.Game.Tools.sewingKit = function (item, x, y, z)
{ /* TODO sewing kit */ };

/**
 * Use a butterfly net on the given item.
 *
 * Attempt to capture a bug.
 */
Tootsville.Game.Tools.butterflyNet = function (item, x, y, z)
{ /* TODO butterfly net */ };

/**
 * Use a fishing rod on the given item.
 *
 * Attempt to catch a fish.
 */
Tootsville.Game.Tools.fishingRod = function (item, x, y, z)
{ /* TODO fishing rod */ };

/**
 * Use a paint brush on the given item.
 */
