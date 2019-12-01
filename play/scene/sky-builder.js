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

if (!('SkyBuilder' in Tootsville)) { Tootsville.SkyBuilder = {}; }


/** 
 * The first layer of the sky is the ambient light of the scene, and the
 * color  of   the  sky  itself.   This  is  based  entirely   upon  the
 * sun position.
 *
 * This is always black in space (when atmosphereP is false).
 *
 * The  sky data  is  taken  from Tootsville.tank.sky  and  the scene  is
 * Tootsville.tank.scene.
 *
 * XXX  Some day,  using  a  GLSL shader  for  the  background would  be
 * awesome, but  that's more work  than is  valuable at this  stage (for
 * BRP). If  some volunteer  is excited  about the  notion, it  would be
 * a very nice touch. Keep in mind that the 18 hour day and 360 day year
 * will require  certain alterations  to any  stock routines  based upon
 * a 24 hour day and 365.2489 day year.
 */
Tootsville.SkyBuilder.setFirstSkyLayer = function (atmosphereP)
{ if (atmosphereP)
  { /* TODO observe sun position and set appropriately */
      Tootsville.tank.scene.clearColor = new BABYLON.Color3 (.7, .7, 1);
      Tootsville.tank.scene.ambientColor = new BABYLON.Color3 (.7, .7, .7);
      Tootsville.SkyBuilder.initCrappyDefaultLight (); }
  else
  { Tootsville.tank.scene.clearColor = new BABYLON.Color3 (0, 0, 0);
    Tootsville.tank.scene.ambientColor = new BABYLON.Color3 (0, 0, 0); } };

/**
 * The second layer of  the sky are the stars, which  are faded with the
 * relative brightness of  the background layer. ie:  We actually reduce
 * the opacity of the stars when the sun is up.
 */
Tootsville.SkyBuilder.setStarfield = function (atmosphereP)
{};

/**
 * Position the sun relative to the viewer
 */
Tootsville.SkyBuilder.setSun = function ()
{};

/**
 * Position the planet Chœrogryllum relative to the viewer. Used when the
 * player is in orbit or on one of the moons.
 */
Tootsville.SkyBuilder.setPlanet = function ()
{};

/**
 * Position  one  of the  moons  relative  to  the  viewer. The  moon  in
 * question's identity is passed in.
 */
Tootsville.SkyBuilder.setMoon = function (whichMoon)
{};

/**
 * Position      The      Moon      relative     to      the      viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setTheMoon = function ()
{ Tootsville.SkyBuilder.setMoon ('MOON'); };

/**
 * Position    The     Other    Moon    relative    to     the    viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setTheOtherMoon = function ()
{ Tootsville.SkyBuilder.setMoon ('OTHM'); };

/**
 * Position    The     Pink    Moon    relative    to     the    viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setThePinkMoon = function ()
{  Tootsville.SkyBuilder.setMoon ('PINK'); };

/**
 * Set up  clouds above the  terrain based on  the map from  the server.
 * Only used on Chœrogyryllum.
 */
Tootsville.SkyBuilder.setCloudCover = function ()
{};

/**
 * Set precipitation, if any. Only used on Chœrogryllum.
 */
Tootsville.SkyBuilder.setPrecipitation = function ()
{};

/**
 * Initialize a default  fill light source for the  game, until in-world
 * light sources are working.
 *
 * XXX Remove this once the game has its own lights.
 *
 * TODO The sky  loader should create lights for sun,  moons, and a base
 * light for stars.
 */
Tootsville.SkyBuilder.initCrappyDefaultLight = function ()
{ const light = new BABYLON.DirectionalLight (
    'sunlight',
    new BABYLON.Vector3 (0, -.5, -1),
    Tootsville.tank.scene);
  light.position = new BABYLON.Vector3 (20, 70, 120); /* TODO Sun position */
  Tootsville.tank.shadowGenerator = new BABYLON.ShadowGenerator (1024, light); };


/**
 * Build the  sky for the current  environment. Reads the sky  values at
 * Tootsville.tank.sky and affects the scene at Tootsville.tank.scene.
 *
 * Depending on the world in question, the sky may have these layers:
 *
 * @enumerate
 * @item
 * The first  layer is the  base color of  the sky. On  Chœrogyllum, this
 * varies from a  blue bordering on white at noon  (09:00) to pitch black
 * at  midnight  (00:00)  and  should have  redness  applied  at  sunset.
 * The  ambient  color  lighting  of  the  scene  is  likewise  affected.
 * In space, it is always black.
 *
 * @item
 * The second layer is a starfield, which fades with the sun's altitude.
 *
 * @item
 * The third layer has the sun.
 *
 * @item
 * The fourth layer has  The Moon, The Other Moon, and  the Pink Moon, as
 * appropriate, and Chœrogryllum, when in space.
 *
 * @item
 * The fifth layer is cloud cover (when on the planet).
 *
 * @item
 * Finally, the sixth layer is any precipitation effect.
 * @end enumerate
 */
Tootsville.SkyBuiulder.build = function (world)
{ Tootsville.SkyBuilder.setFirstSkyLayer ('CHOR' == world);
  Tootsville.SkyBuilder.setStarfield ('CHOR' == world);
  Tootsville.SkyBuilder.setSun ();
  if ('CHOR' != world) { Tootsville.SkyBuilder.setPlanet (); }
  if ('MOON' != world) { Tootsville.SkyBuilder.setTheMoon (); }
  if ('OTHM' != world) { Tootsville.SkyBuilder.setTheOtherMoon (); }
  if ('PINK' != world) { Tootsville.SkyBuilder.setThePinkMoon (); }
  if ('CHOR' == world)
  { Tootsville.SkyBuilder.setCloudCover ();
    Tootsville.SkyBuilder.setPrecipitation ();} };

/**
 * Fetch sky data from the game server
 *
 * Updates Tootsville.tank.sky
 *
 * TODO, we  should replicate the  logic for predicting the  altitude of
* the sun and moons from the  Lisp code in Javascript, however, the Lisp
* code itself is not final and we don't need to do that work twice.
 */
Tootsville.SkyBuilder.updateSkyData = function ()
{};
