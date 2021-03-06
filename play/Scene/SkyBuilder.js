/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/SkyBuilder.js is part of Tootsville
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

if (!('SkyBuilder' in Tootsville)) { Tootsville.SkyBuilder = {}; }

/**
 * The first layer of the sky is the ambient light of the scene, and the
 * color  of   the  sky  itself.   This  is  based  entirely   upon  the
 * sun position.
 *
 * This is always black in space (when atmosphereP is false).
 *
 * The  sky data  is  taken  from Tootsville.SkyBuilder.sky  and  the scene  is
 * Tootsville.Tank.scene.
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
  { const tod = Tootsville.SkyBuilder.sunY()/2000 + 1/2;
    Tootsville.Tank.scene.clearColor = new BABYLON.Color3 (.7 * tod, .7 * tod, 1 * tod);
    Tootsville.Tank.scene.ambientColor = new BABYLON.Color3 (.7 * tod, .7 * tod, .7 * tod); }
  else
  { Tootsville.Tank.scene.clearColor = new BABYLON.Color3 (0, 0, 0);
    Tootsville.Tank.scene.ambientColor = new BABYLON.Color3 (0, 0, 0); } };

/**
 * The second layer of  the sky are the stars, which  are faded with the
 * relative brightness of  the background layer. ie:  We actually reduce
 * the opacity of the stars when the sun is up. FIXME
 */
Tootsville.SkyBuilder.setStarfield = function (atmosphereP)
{ let starsP = true;
  if (atmosphereP && Tootsville.SkyBuilder.sunY() > 0)
  { starsP = false; }
  let layersP = (('layers' in Tootsville.Tank.scene) && (0 < Tootsville.Tank.scene.layers.length));
  if (starsP && (! layersP))
  { const starfield = new BABYLON.Layer("starfield",
                                        "https://jumbo.tootsville.org/Assets/Textures/5/Starfield.png",
                                        Tootsville.Tank.scene, true); }
  if (starsP || (! layersP))
  { return; }
  const layer = Tootsville.Tank.scene.layers[0];
  if (layer && 'starfield' === layer.name)
  { layer.dispose(); } };

/**
 * Position the sun relative to the viewer
 */
Tootsville.SkyBuilder.setSun = function ()
{ if (Tootsville.SkyBuilder.sun)
  { Tootsville.error ("Sun already exists"); return; }
  const sun = BABYLON.Mesh.CreateSphere ("The Sun", 6, 400, Tootsville.Tank.scene);
  const sunFire = new BABYLON.StandardMaterial("Nuclear Fire", Tootsville.Tank.scene);
  sunFire.emissiveColor = new BABYLON.Color3(1,.90,.95);
  sunFire.disableLighting = true;
  sun.isPickable = false;
  sun.material = sunFire;
  sun.position.x = Tootsville.SkyBuilder.sunX ();
  sun.position.y = Tootsville.SkyBuilder.sunY ();
  Tootsville.SkyBuilder.sun = sun;
  const sunLight = new BABYLON.DirectionalLight (
      'Sunlight', sun.position.clone ().negate (), Tootsville.Tank.scene);
  if (sun.position.y < 0) { sunLight.intensity = 0; }
  sunLight.position = sun.position.clone ();
  Tootsville.SkyBuilder.sunLight = sunLight;

  const fill = new BABYLON.HemisphericLight("Fill Light", new BABYLON.Vector3(-1, 1, 0), Tootsville.Tank.scene);
  Tootsville.SkyBuilder.fillLight = fill;

  const shadowGenerator = new BABYLON.ShadowGenerator (1024, sunLight);
  // /* TODO  adjust  shadow attributes  in  space  due to  no  atmospheric
  //  * scattering effects */
  shadowGenerator.bias = 0.00001;
  shadowGenerator.normalBias = 0.01;
  shadowGenerator.usePoissonSampling = true;
  shadowGenerator.useContactHardeningShadow = true;
  shadowGenerator.contactHardeningLightSizeUVRatio = 0.05;
  shadowGenerator.setDarkness(0.5);
  Tootsville.Tank.shadowGenerator = shadowGenerator;
};

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
{ let moon;
  let moonColor;
  if ('MOON' === whichMoon)
  { moon = BABYLON.Mesh.CreateSphere ("The Moon", 6, 300, Tootsville.Tank.scene);
    moonColor = new BABYLON.Color3 (1, .90, .95); }
  else if ('OTHM' === whichMoon)
  { moon = BABYLON.Mesh.CreateSphere ("The Other Moon", 6, 250, Tootsville.Tank.scene);
    moonColor = new BABYLON.Color3 (1, .90, .95); }
  else
  { moon = BABYLON.Mesh.CreateSphere ("The Pink Moon", 6, 200, Tootsville.Tank.scene);
    moonColor = new BABYLON.Color3 (1, .7, .7); }
  const moonCheese = new BABYLON.StandardMaterial("Moon Cheese " + whichMoon, Tootsville.Tank.scene);
  moonCheese.emissiveColor = moonColor;
  moonCheese.disableLighting = true;
  moon.material = moonCheese;
  moon.isPickable = false;
  return moon;
};

/**
 * Position      The      Moon      relative     to      the      viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setTheMoon = function ()
{ Tootsville.SkyBuilder.moon = Tootsville.SkyBuilder.setMoon ('MOON'); };

/**
 * Position    The     Other    Moon    relative    to     the    viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setTheOtherMoon = function ()
{ Tootsville.SkyBuilder.otherMoon = Tootsville.SkyBuilder.setMoon ('OTHM'); };

/**
 * Position    The     Pink    Moon    relative    to     the    viewer.
 * See `Tootsville.SkyBuilder.setMoon'.
 */
Tootsville.SkyBuilder.setThePinkMoon = function ()
{  Tootsville.SkyBuilder.pinkMoon = Tootsville.SkyBuilder.setMoon ('PINK'); };

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
 * Get the instantaneous position of the sun in Y
 *
 * Accurate to ±1 min (of time)
 */
Tootsville.SkyBuilder.sunY = function ()
{ const time = Tootsville.decodeTime ();
  return (Math.sin((((time.hour+time.min/60) / 18)*2*Math.PI - Math.PI/2)))/2 * 2000; };

/**
 * Get the instantaneous position of the sun in X
 *
 * Accurate to ±1 min (of time)
 */
Tootsville.SkyBuilder.sunX = function ()
{ const time = Tootsville.decodeTime ();
  return (Math.cos((((time.hour+time.min/60) / 18)*2*Math.PI - Math.PI/2)))/2 * 2000; };


Tootsville.SkyBuilder.moonΘ = function (period)
{ const time = Tootsville.decodeTime ();
  // FIXME
  return ((((time.hour+time.min/60) / 18 * period)*2*Math.PI - Math.PI/2)); };

Tootsville.SkyBuilder.updateMoon = function (model, period)
{ const θ = Tootsville.SkyBuilder.moonΘ (period);
  model.position.x = Math.sin (θ) /2 * 1900;
  model.position.y = Math.cos (θ) /2 * 1900; };

/**
 * Build the  sky for the current  environment. Reads the sky  values at
 * Tootsville.SkyBuilder.sky and affects the scene at Tootsville.Tank.scene.
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
Tootsville.SkyBuilder.build = function (world)
{ Tootsville.SkyBuilder.setFirstSkyLayer ('CHOR' === world);
  Tootsville.SkyBuilder.setStarfield ('CHOR' === world);
  Tootsville.SkyBuilder.setSun ();
  if ('CHOR' !== world) { Tootsville.SkyBuilder.setPlanet (); }
  if ('MOON' !== world) { Tootsville.SkyBuilder.setTheMoon (); }
  if ('OTHM' !== world) { Tootsville.SkyBuilder.setTheOtherMoon (); }
  if ('PINK' !== world) { Tootsville.SkyBuilder.setThePinkMoon (); }
  if ('CHOR' === world)
  { Tootsville.SkyBuilder.setCloudCover ();
    Tootsville.SkyBuilder.setPrecipitation ();}
  Tootsville.SkyBuilder.update ();
  setInterval ( () => { Tootsville.SkyBuilder.update (world); }, 60000); };

/**
 * Update sky positions and the like.
 *
 * XXX Some things aren't able to be updated yet.
 */
Tootsville.SkyBuilder.update = function (world=Tootsville.activity.world)
{ Tootsville.SkyBuilder.setFirstSkyLayer ('CHOR' === world);
  Tootsville.SkyBuilder.setStarfield ('CHOR' === world);
  const sun = Tootsville.SkyBuilder.sun;
  sun.position.x = Tootsville.SkyBuilder.sunX ();
  sun.position.y = Tootsville.SkyBuilder.sunY ();
  const sunLight = Tootsville.SkyBuilder.sunLight;
  sunLight.position = Tootsville.SkyBuilder.sun.position.clone ();
  const fill = Tootsville.SkyBuilder.fillLight;
  if (sun.position.y < 0)
  { sunLight.intensity = 0;
    fill.intensity = 1;
    fill.diffuse = new BABYLON.Color3(.5, .5, .7);
    fill.specular = new BABYLON.Color3(.5, .5, .5);
    fill.groundColor = new BABYLON.Color3(.25, .25, .35);}
  else
  { sunLight.intensity = 1;
    fill.intensity = 1;
    fill.diffuse = new BABYLON.Color3(.5, .5, .5);
    fill.specular = new BABYLON.Color3(.75, .75, .75);
    fill.groundColor = new BABYLON.Color3(.2, .2, .2);}
  Tootsville.SkyBuilder.sunLight.direction = Tootsville.SkyBuilder.sun.position.negate ();
  Tootsville.SkyBuilder.updateMoon (Tootsville.SkyBuilder.moon, 30);
  Tootsville.SkyBuilder.updateMoon (Tootsville.SkyBuilder.otherMoon, 40); // FIXME
  Tootsville.SkyBuilder.updateMoon (Tootsville.SkyBuilder.pinkMoon, 50); // FIXME
};

/**
 * Fetch sky data from the game server
 *
 * Updates Tootsville.SkyBuilder.sky
 */
Tootsville.SkyBuilder.updateSkyData = function ()
{};

/**
 *
 */
Tootsville.SkyBuilder.buildMatchingSky = function (sky)
{ console.warn ("unimplemented, not matching sky", sky); };


/**
 *
 */
Tootsville.SkyBuilder.buildMatchingWeather = function (weather)
{ console.warn ("unimplemented, not matching weather", weather); };
