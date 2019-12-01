/* -*- js2 -*-*/

/*@license
 *
 * ./play/scene/sky-builder.js is part of Tootsville
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

if (!('FurnitureBuilder' in Tootsville)) { Tootsville.FurnitureBuilder = { itemTemplates: {} }; }
if (!('itemTemplates' in Tootsville.FurnitureBuilder))
{ Tootsville.FurnitureBuilder.itemTemplates = {}; }

Tootsville.FurnitureBuilder.colorize = function (item, model, scene, finish)
{ };

Tootsville.FurnitureBuilder.build2 = function (item, model, scene, finish)
{};

Tootsville.FurnitureBuilder.loadItemTemplate = function (item, scene, finish)
{};

/**
 * Given an item's description, load, colorize, and position its model.
 *
 * Furniture items are described differently than avatars; the canonical
 * description is at `ITEM-INFO', qv., and `ITEM-TEMPLATE-INFO'.
 *
 * In the  front-end, for each  item template,  we load the  asset once,
 * then  clone  it for  each  item  instance.  There  is a  very  strong
 * likelihood  that we  could be  clever and  use instances  with unique
 * colorations, but I  (BRP) simply don't have the time  to learn how to
 * do that right now.
 */
Tootsville.FurnitureBuilder.build = function (item, scene, finish)
{};

