/* -*- js2 -*- */

/**@license
 *
 * play/Game/BallSystem.js is part of Tootsville
 *
 * Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 CIWTA; © 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA.
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
 * You can reach Interworldly Adventuring, LLC at https://interworldly.com/
 *
 */

if (!('Game' in Tootsville)) { Tootsville.Game = { BallSystem: {} }; }
if (!('BallSystem' in Tootsville.Game)) { Tootsville.Game.BallSystem = {}; }

if (!('allBalls' in Tootsville.Game.BallSystem))
{ Tootsville.Game.BallSystem.allBalls = []; }


/**
 *
 */
Tootsville.Game.BallSystem.register = function (entity, course)
{ entity.course = course;
  Tootsville.Game.BallSystem.allBalls.push (entity); };

/**
 *
 */
Tootsville.Game.BallSystem.remove = function (entity)
{ const index = Tootsville.Game.BallSystem.allBalls.indexOf (entity);
  if (index >= 0) { Tootsville.Game.BallSystem.allBalls.splice (index, 1); } };

/**
 * Update the position of all balls
 */
Tootsville.Game.BallSystem.updateBalls = function ()
{ for (let i = 0; i < Tootsville.Game.BallSystem.allBalls.length; ++i)
  { const ball = Tootsville.Game.BallSystem.allBalls [i];
    if (ball.course)
    { let finish = Tootsville.Game.Nav.moveEntityOnCourse (ball, ball.course);
      if (finish) { 
        // Ball has reached the end of its course
        // TODO: Implement bounce physics if lateral energy remains
        Tootsville.Game.BallSystem.remove(ball);
      } else { 
        // TODO: Implement friction to reduce speed over time
        // For now, continue normal movement
      } } } };

/**
 * Simulate the passage of Δt time (in seconds)
 */
Tootsville.Game.BallSystem.fastForward = function (δT)
{ // TODO: Implement time-based ball physics simulation
  // This would update ball positions based on elapsed time
  // For now, this is a placeholder for future implementation
  Tootsville.trace("BallSystem.fastForward called with δT:", δT);
};
