/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/ModelLoader.js is part of Tootsville
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

if (!('Tootsville' in window)) Tootsville = {};
if (!('ModelLoader' in Tootsville)) Tootsville.ModelLoader = {};

if (!('avatarCache' in Tootsville.ModelLoader))
    Tootsville.ModelLoader.avatarCache = {};

/**
 * Create a loading promise for an asset
 */
Tootsville.ModelLoader.loadPromise = async (path, file, scene) => {
    return new Promise ( result => {
        BABYLON.SceneLoader.LoadAssetContainer
        (path, file, scene, function (container) {
            result (container); }); }); };

/**
 * Load the model ``kind''/5/``file''.glb only once, using a cache
 */
Tootsville.ModelLoader.loadModelOnce = async (kind, file, scene) => {
    let found = Tootsville.ModelLoader.avatarCache [kind + '/' + file];
    if (found) return found;
    console.debug("Loading " + kind + "/5/" + file + ".glb");
    let assets = await Tootsville.ModelLoader.loadPromise ('https://jumbo.tootsville.org/Assets/' + kind + '/5/',
                                                             file + '.glb', scene);
    console.debug("Loaded " + kind + "/5/" + file + ".glb", assets);
    Tootsville.ModelLoader.avatarCache[kind + '/' + file] = assets;
    return assets; };

/**
 * Recursive function used to apply ``colorizer'' to material children of ``node''
 */
Tootsville.ModelLoader.recursiveColorize = function (node, colorizer) {
    if (!(node)) return;
    const children = node.getChildren ();
    if (!(children)) return;
    for (let i = 0; i < children.length; ++i) {
        if (children [i].material)
            colorizer (children[i]);
        Tootsville.ModelLoader.recursiveColorize (children [i], colorizer); }};

/**
 *  Register the object to cast a shadow
 */
Tootsville.ModelLoader.enableShadows = function (object, scene) {
    if (!(Tootsville.Tank.shadowGenerator)) return;
    Tootsville.Tank.shadowGenerator.addShadowCaster (object);
    Tootsville.Tank.shadowGenerator.getShadowMap ().renderList.push (
        Tootsville.Tank.getLargestChildMesh (object)); };

/**
 * Load the ``file'' from /Assets/``kind''/5/ and apply ``colorizer''.
 *
 * Returns the asset collection loaded into ``scene''.
 */
Tootsville.ModelLoader.loadAndColorize = async (kind, file, colorizer, scene) => {
    let assets = await Tootsville.ModelLoader.loadModelOnce (kind, file, scene);
    console.debug ("Assets loaded for " + kind + "/5/" + file, assets);
    assets.addAllToScene ();
    let clone = assets.instantiateModelsToScene ();
    console.debug ("Cloned assets into scene " + kind + "/5/" + file + "; " + clone.rootNodes.length + " root nodes");
    if (0 === clone.rootNodes.length) {
        console.warn ("Cloned assets set is empty, clearing caché and trying again " + kind + "/5/" + file);
        delete Tootsville.ModelLoader.avatarCache [kind + '/' + file];
        return Tootsville.ModelLoader.loadAndColorize (kind, file, colorizer, scene); }
    Tootsville.ModelLoader.recursiveColorize (clone.rootNodes[0], colorizer);
    Tootsville.ModelLoader.enableShadows (clone, scene);
    return clone; };

/**
 * Set the color of ``material'' to the Tootsville color ``colorName''
 *
 * This is used by eg. the AvatarBuilder or FurnitureBuilder as a
 * shared convenience function in the implementation of their own
 * colorizers.
 */
Tootsville.ModelLoader.setMaterialColor = function (material, colorName) {
    const diffuseColor = Tootsville.UI.htmlColorToBabylon(
        Tootsville.UI.interpretTootColor (colorName));
    material.diffuseColor = diffuseColor;
    material.specularColor = Tootsville.UI.lightenColor(diffuseColor);
    return material;
};
