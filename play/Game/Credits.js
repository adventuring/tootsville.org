/* -*- js2 -*- */
/**@license
 * play/Game/Credits.js is part of Tootsville
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

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }

/**
 * The credits for the game as a string.
 *
 * Suitable  for an  ``about this  game'' dialog  box, credits  crawl, or
 * other presentation. This  is expected to run long, over  time, so take
 * some efforts to ensure that there  is scrolling or other affordance in
 * place to accomodate a very long message.
 */
Tootsville.Game.credits = function ()
{ console.log ("Tootsville V was brought to you by:\n\n" +
               "Chief Engineer: Bruce-Robert Pocock\n\n" +
               "Additional support:\n" +
               "Chris Brunner\n" +
               "Ali Dolan\n" +
               "Mariaelisa Greenwood\n" +
               "Levi Mc Call\n" +
               "Gian Ratnapala\n" +
               "Zephyr Salz\n" +
               "\nAnd all our Tootsville supporters!\n\n"); };
