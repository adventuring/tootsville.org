/* -*- js2 -*- */

/**@license
 *
 * play/Game/BallSystem.js is part of Tootsville
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
 * You  should have  received a  copy of  the GNU  Affero General  Public
 * License     along    with     this     program.     If    not,     see
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

if (!('Game' in Tootsville)) { Tootsville.Game = { BallSystem: {} }; }
if (!('BallSystem' in Tootsville.Game)) { Tootsville.Game.BallSystem = {}; }

if (!('allBalls' in Tootsville.Game.BallSystem))
{ Tootsville.Game.BallSystem.allBalls = []; }

/**
 * Update the position of all balls
 */
Tootsville.Game.BallSystem.updateBalls = function ()
{ for (let i = 0; i < Tootsville.Game.BallSystem.allBalls.length; ++i)
  { const ball = Tootsville.Game.BallSystem.allBalls [i];
    if (ball.course)
    { let finish = Tootsville.Game.Nav.moveEntityOnCourse (ball, ball.course);
      if (finish) { delete ball['course']; } } } };

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.BallSystem.fastForward = function (δT)
{ /* TODO */ };
