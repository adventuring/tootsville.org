/* -*- js2 -*-*/

/**@license
 *
 * ./play/UI/panels/camera.js is part of Tootsville
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

if (! ('Camera' in Tootsville.UI)) { Tootsville.UI.Camera = {}; };
Tootsville.UI.Camera.takeScreenShot = function ()
{ BABYLON.Tools.CreateScreenshot (Tootsville.Tank.engine,
                                  Tootsville.Tank.camera,
                                  1024,
                                  data => {
                                      let img = document.getElementById('camera-preview');
                                      img.height = 256;
                                      img.width = 256;
                                      img.src = data;
                                      img.alt = "Click to download";
                                      let a = document.getElementById('camera-link');
                                      a.href = data;
                                      a.setAttribute ("download", "Photo from Tootsville.jpg");
                                  },
                                  'image/jpeg'); };

Tootsville.UI.Camera.takeScreenShot ();
