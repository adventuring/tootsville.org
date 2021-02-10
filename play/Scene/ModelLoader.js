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
    let assets = await Tootsville.ModelLoader.loadPromise ('https://jumbo.tootsville.org/Assets/' + kind + '/5/',
                                                             file + '.glb', scene);
    Tootsville.ModelLoader.avatarCache[kind + '/' + file] = assets;
    return assets; };

/**
 * Recursive function used to apply ``colorizer'' to material children of ``node''
 */
Tootsville.ModelLoader.recursiveColorize = function (node, colorizer, scene) {
    for (let i = 0; i < node.children.length; ++i)
        if (node.children [i].material)
            colorizer (node.children[i], scene);
        else if (node.children [i].children)
            Tootsville.ModelLoader.recursiveColorize (node, colorizer, scene); };
/**
 * Load the ``file'' from /Assets/``kind''/5/ and apply ``colorizer''.
 *
 * Returns the asset collection loaded into ``scene''.
 */
Tootsville.ModelLoader.loadAndColorize = async (kind, file, colorizer, scene) => {
    let assets = await Tootsville.ModelLoader.loadModelOnce (kind, file, scene);
    let clone = assets.instantiateModelsToScene ();
    for (let i = 0; i < clone.rootNodes.length; ++i)
        Tootsville.ModelLoader.recursiveColorize (clone.rootNodes[i], colorizer, scene);
    return assets; };

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
