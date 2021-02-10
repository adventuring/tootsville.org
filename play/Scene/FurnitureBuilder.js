/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/FurnitureBuilder.js is part of Tootsville
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

if (!('FurnitureBuilder' in Tootsville)) { Tootsville.FurnitureBuilder = { itemTemplates: {} }; }

if (!('itemTemplates' in Tootsville.FurnitureBuilder)) { Tootsville.FurnitureBuilder.itemTemplates = {}; }

/**
 * Colorize a furniture item
 */
Tootsville.FurnitureBuilder.colorize = function (item, model, scene, finish)
{ const baseMaterial = new BABYLON.StandardMaterial (item.baseColor, scene);
  baseMaterial.diffuseColor = new BABYLON.Color3.FromHexString (Tootsville.UI.interpretTootColor (item.baseColor));
  const altMaterial = new BABYLON.StandardMaterial (item.padColor, scene);
  altMaterial.diffuseColor = new BABYLON.Color3.FromHexString (Tootsville.UI.interpretTootColor (item.altColor));
  const meshes = model.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes [i];
    if (mesh.name.indexOf ('Base') >= 0)
    { mesh.material = baseMaterial; }
    else if (mesh.name.indexOf ('Alt') >= 0)
    { mesh.material = altMaterial; }
    else if (mesh.name.indexOf ('LightSource') >= 0)
    { mesh.material = mesh.material.clone ();
      if (! mesh.material.emissiveColor)
      { mesh.material.emissiveColor = mesh.material.diffuseColor; }
      const light = new BABYLON.PointLight (
          item.template.name + " " + item.uuid + ' Light', mesh.position.clone (), scene);
      mesh.addChild (light); }
    if (finish) { finish (model); } } };

/**
 * Enable the object to cast shadows in the scene
 */
Tootsville.FurnitureBuilder.enableShadows = function (object, scene)
{ if (Tootsville.Tank.shadowGenerator)
  { /* Tootsville.Tank.shadowGenerator.addShadowCaster (object); */
      Tootsville.Tank.shadowGenerator.getShadowMap ().renderList.push (
          Tootsville.Tank.getLargestChildMesh (object)); }; };

/**
 * Stash a reference to the item it the scene.items object.
 */
Tootsville.FurnitureBuilder.rememberItem = function (item, model, scene)
{ if (!('items' in scene)) { scene.items = {}; }
  scene.items [item.uuid] = Object.assign ({}, item);
  scene.items [item.uuid].model = model; };

/**
 * Finish construction  of the object  after it  has been loaded  by the
 * asset manager.
 */
Tootsville.FurnitureBuilder.build2 = function (item, model, scene, finish)
{ console.debug ("Building furniture " + item.template.name + " " + item.uuid);
  model.scaling = new BABYLON.Vector3 (parseFloat(item.scale.x),
                                       parseFloat(item.scale.y),
                                       parseFloat(item.scale.z));
  model.checkCollisions = true;
  try { Tootsville.FurnitureBuilder.rememberItem (item, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.FurnitureBuilder.colorize (item, model, scene, finish); } catch (e) { console.error (e); }
  if (item.growth)
  { try { Tootsville.Game.GrowthSystem.register (item); } catch (e) { console.error (e); } }
};

/**
 * Load an item template avatar from the assets server.
 */
Tootsville.FurnitureBuilder.loadItemTemplate = function (item, scene, finish)
{ let assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  console.debug ("fixing to load item template " + item.template.name);
  let loadTask = assetsManager.addMeshTask (
      "loading " + item.template.name, null,
      "https://jumbo.tootsville.org/Assets/Models/5/" + item.template.avatar + "/",
      item.template.avatar + ".glb");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.Mesh ("item/" + item.template.name + '#' + item.uuid, scene);
    let i;
    modelRoot.position = new BABYLON.Vector3 (parseFloat(item.position.x),
                                              parseFloat(item.position.y),
                                              parseFloat(item.position.z));
    modelRoot.rotate (BABYLON.Axis.Y, parseFloat(item.facing));
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { modelRoot.addChild (task.loadedMeshes [i]);
      task.loadedMeshes [i].renderOutline = true;
      task.loadedMeshes [i].outlineColor = BABYLON.Color3.Black ();  }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { modelRoot.addChild (task.loadedParticleSystems [i]); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { modelRoot.addChild (task.loadedSkeletons [i]); }
    console.debug ("Loaded item template avatar " + item.template.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    Tootsville.FurnitureBuilder.build2 (item, modelRoot, scene, finish); };
  assetsManager.load (); };

/**
 * WRITEME … TODO update furniture …
 */
Tootsville.FurnitureBuilder.update = function (item, model, scene, finish=undefined)
{ /* TODO */
    if (finish) { finish (model); } };

/**
 * Given an item's description, load, colorize, and position its model.
 *
 * Furniture items are described differently than avatars; the canonical
 * description is at `ITEM-INFO', qv., and `ITEM-TEMPLATE-INFO'.
 *
 */
Tootsville.FurnitureBuilder.build = function (item, scene=undefined, finish=undefined)
{ if (!scene) scene = Tootsville.Tank.scene;
  if (!scene) { console.error ("Can't build furniture without a scene", item);
                return; }
  if (scene.items && scene.items [item.uuid] && ('model' in scene.items [item.uuid]))
  { Tootsville.FurnitureBuilder.update (item, scene.items [item.uuid].model, scene, finish); }
  else
  { Tootsville.FurnitureBuilder.loadItemTemplate (item, scene, finish); } };




/*----------------------------------------*/

/**
 * Set ``material'' to a pixmap (PNG or JPEG) image
 */
Tootsville.FurnitureBuilder.setMaterialPixmapTexture = function (material, texture, scene) {
    material.diffuseColor = null;
    material.diffuseTexture = new BABYLON.Texture (
        'https://jumbo.tootsville.org/Assets/Textures/5/' + texture,
        scene);
};

/**
 * Set ``material'' to an SVG image
 */
Tootsville.FurnitureBuilder.setMaterialVectorTexture = function (material, texture, scene) {
    const vectorTexture = new BABYLON.DynamicTexture(texture, 1024, scene);
    const canvas = vectorTexture.getContext ();
    const image = new Image ();
    image.onload = function () { context.drawImage (image, 0, 0); };
    image.width = image.height = 1024;
    image.src = 'https://jumbo.tootsville.org/Assets/Textures/5/' + texture;
    material.diffuseColor = null;
    material.diffuseTexture = vectorTexture;
    return material; };

/**
 * Set ``material'' to a video texture.
 *
 * Four files will be used:
 *
 * @itemize
 * @item
 * The base filename ending in @code{mp4},
 * @item
 * The base filename ending in @code{webm},
 * @item
 * The base filename ending in @code{ogv},
 * @item
 * and a ``loading poster'' image with the base filename 
 * ending in @code{jpg}.
 * @end itemize
 */
Tootsville.FurnitureBuilder.setMaterialVideoTexture = function (material, texture, scene) {
    material.diffuseColor = null;
    let base = texture.match(/^(.*)\.(mp4|ogv|webm)$/)[1];
    material.diffuseTexture = new BABYLON.VideoTexture (
        texture,
        [ 'https://jumbo.tootsville.org/Assets/Textures/5/' + base + '.mp4',
          'https://jumbo.tootsville.org/Assets/Textures/5/' + base + '.webm',
          'https://jumbo.tootsville.org/Assets/Textures/5/' + base + '.ogv' ],
        scene, /* mipmap */ true, /* invertY */ false,
        /* sampling mode */ TRILINEAR_SAMPLINGMODE,
        /* settings */
        { autoPlay: true,
          loop: true,
          poster: 'https://jumbo.tootsville.org/Assets/Textures/5/' + base + '.jpg',
          muted: true });
    return material; };

/**
 * UNIMPLEMENTED special handling for Theater in Toot Square West screen
 */
Tootsville.FurnitureBuilder.theaterWestVideoTexture = function (material, scene) {
    // UNIMPLEMENTED Theater West video texture
};


/**
 * Items may have a material texture applied on a per-item basis. The
 * material must be named (case-insensitive) @code{map} and the texture
 * filename is given by the item's @code{specialTexture} field.
 *
 * @subsection Kinds of Special Textures
 *
 * Ordinarily, this will be a PNG or JPEG file located at
 * @code{https://jumbo.tootsville.org/Assets/Textures/5/} whose name
 * is given in @code{specialTexture} ending in @code{.png}
 * or @code{.jpg} or @code{.jpeg}.
 *
 * However, the special texture may also be an SVG image which will be
 * drawn onto a texture (probably at 1024×1024 px), if its name ends
 * in @code{.svg}.
 *
 * The special texture may also be a video stream, which will be
 * played on a silent loop, when its name ends
 * in @code{.mp4}, @code{.webm}, or @code{.ogv}.  Note that we will
 * attempt to load @emph{all} of the MPEG 4, WebM, and OGG Vorbis
 * forms of the video simultaneously in different browsers, so it is
 * not defined which of the two will be loaded by any particular user
 * (due to CODEC issues in browsers).  A third file, in @code{JPEG}
 * format and named ending in @code{.jpg} will be used as the
 * ``poster'' image texture until the video has loaded.
 *
 * @subsection Very Special Special Textures
 *
 * Other special cases can be specified with names that begin with the
 * character @code{#}, since that is forbidden in URLs (and used in
 * the operator command language in Tootsville as well). Currently,
 * only one such extension is defined, which is @code{#theater-west}.
 * 
 * @subsubsection #theater-west
 *
 * The special code @code{#theater-west} is used only for the movie
 * screen in the theater located in Toot Square West, which has some
 * special behaviors.
 *
 * First, the theater's current film is selected by a server-side
 * setting from the @code{Assets/Movies/5/} path.
 *
 * Second, the film plays at the same time offset for all users,
 * modulo the running length of the film.
 *
 * Third, the film plays with sound enabled.
 */
Tootsville.FurnitureBuilder.setMaterialTexture = function (material, texture, scene) {
    if (/\.png$/.test(texture))
        Tootsville.FurnitureBuilder.setMaterialPixmapTexture (material, texture, scene);
    else if (/\.jpe?g$/.test(texture))
        Tootsville.FurnitureBuilder.setMaterialPixmapTexture (material, texture, scene);
    else if (/\.svg$/.test(texture))
        Tootsville.FurnitureBuilder.setMaterialVectorTexture (material, texture, scene);
    else if (/\.(ogv|mp4|webm)$/.test(texture))
        Tootsville.FurnitureBuilder.setMaterialVideoTexture (material, texture, scene);
    else if ('#theater-west' === texture)
        Tootsville.FurnitureBuilder.theaterWestVideoTexture (material, scene);
    else
        console.warn ("Unsupported specialTexture value:", texture); };

/**
 * Make a colorizer function for a material for ``furniture''.
 *
 * Materials in an item's model can be recolored on a per-item basis
 * when they have specific (case-insensitive) names for those
 * materials.
 *
 * Color names are specific names understood by `PARSE-COLOR24' or the
 * HTML #RGB or #RRGGBB forms. Other formats understood by
 * `PARSE-COLOR24' are not understood by
 * `Tootsville.UI.interpretTootColor' and are not supported. The game
 * server will only send colors in these formats.
 *
 * Any material named @code{base} will have its diffuse color set to
 * the @code{baseColor} of the item, with its specular color set to a
 * lighter version of that same color.
 *
 * Any material named @code{alt} will likewise have its diffuse color
 * set to the @code{altColor} of that item, with its specular color
 * set to a lighter version of that same color.
 *
 * Finally, any material named @code{map} will have the diffuse color
 * set to a texture map based on the file named
 * in @code{specialTexture} of that item, if any. The specific file
 * will be loaded by `Tootsville.FurnitureBuilder.setMaterialTexture'
 * from a file whose path
 * is @code{https://jumbo.tootsville.org/Assets/Textures/5/} and whose
 * name on that path is given by @code{specialTexture}. It can be a
 * PNG, JPEG, or SVG file. See @code{setMaterialTexture} for details.
 */
Tootsville.FurnitureBuilder.makeFurnitureColorizeMaterial = function (furniture, scene) {
    return function (material) {
        if (!(material.name)) return;
        if ('base' == material.name.toLower () && furniture.baseColor)
            Tootsville.ModelLoader.setMaterialColor (material, furniture.baseColor);
        else if ('alt' == material.name.toLower () && furniture.altColor)
            Tootsville.ModelLoader.setMaterialColor (material, furniture.altColor);
        else if ('map' == material.name.toLower () && furniture.specialTexture)
            Tootsville.FurnitureBuilder.setMaterialTexture (material, furniture.specialTexture, scene);
            return;
    }; };

/**
 * Make a colorizer function for ``furniture'' for `Tootsville.ModelLoader.loadAndColorize'
 */
Tootsville.FurnitureBuilder.makeFurnitureColorizer = function (furniture, scene) {
    let colorizeMaterial = Tootsville.FurnitureBuilder.makeFurnitureColorizeMaterial (furniture, scene);
    return function (node) {
        if (!(node.materials)) return;
        for (let i = 0; i < node.materials.length; ++i)
            colorizeMaterial (node.materials [i], scene); }; };

/**
 *
 */
Tootsville.FurnitureBuilder.buildNew = function (furniture, scene=null, finish=null)
{ if (!scene) { scene = Tootsville.Tank.scene; }
  let colorizer = Tootsville.FurnitureBuilder.makeFurnitureColorizer (furniture, scene);
  return Tootsville.ModelLoader.loadAndColorize ('Items', furniture.avatar,
                                                 colorizer, scene); };
