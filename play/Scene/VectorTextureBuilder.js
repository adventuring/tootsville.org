/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/VectorTextureBuilder.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
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

if (!('Tank' in Tootsville)) { Tootsville.Tank = { VectorTextureBuilder: {} }; };
if (!('VectorTextureBuilder' in Tootsville.Tank)) { Tootsville.Tank.VectorTextureBuilder = {}; };


Tootsville.Tank.VectorTextureBuilder.build = function (svgData, name)
{ const svgXML = svgData.documentElement;
  const svgString = new XMLSerializer ().serializeToString (svgXML);
  const svgBase64 = 'data:image/svg+xml;base64,' + window.btoa (svgString);
  const svgTexture = BABYLON.Texture.LoadFromDataString ('data', svgBase64, Tootsville.Tank.scene);
  const svgMaterial = new BABYLON.PBRMaterial (name, Tootsville.Tank.scene);
  svgMaterial.albedoTexture = svgTexture;
  return svgMaterial; };

Tootsville.Tank.VectorTextureBuilder.buildPattern = function (patternName, baseColor, patternColor)
{ console.error ("VectorTextureBuilder.buildPattern is not written yet"); /* TODO */ };
