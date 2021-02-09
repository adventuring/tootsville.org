/* -*- js2 -*-*/

/**@license
 *
 * ./play/UI/FurnitureMover.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { UI: { FurnitureMover: {} }}; }
if (!('UI' in Tootsville)) { Tootsville.UI = { FurnitureMover: {} }; }
if (!('FurnitureMover' in Tootsville.UI)) { Tootsville.UI.FurnitureMover = {}; }

Tootsville.UI.FurnitureMover.activeItem = undefined;
Tootsville.UI.FurnitureMover.decorations = undefined;
Tootsville.UI.FurnitureMover.mouseMode = null;

/**
 * Capture mouse events and translate based on mouseMode
 */
Tootsville.UI.FurnitureMover.dragHelper = function (event) {
    if ('position' === Tootsville.UI.FurnitureMover.mouseMode) {
        // TODO
    } else if ('rotation' === Tootsville.UI.FurnitureMover.mouseMode) {
        // TODO
    } else {
        /* How did we get here? Abandon any changes */
        Tootsville.UI.FurnitureMover.endArranging (false);
    }
};

Tootsville.UI.FurnitureMover.clickHelper = function (event) {
    // TODO
};

/**
 * Stop capturing the mouse movement
 */
Tootsville.UI.FurnitureMover.releaseMouseDrag = function (pointerId) {
    document.onpointermove = undefined;
    document.onpointerdown = undefined;
    document.releasePointerCapture (pointerId);
};

/**
 * Begin capturing the mouse movement
 */
Tootsville.UI.FurnitureMover.captureMouseDrag = function (pointerId) {
    document.onpointermove = Tootsville.UI.FurnitureBuilder.dragHelper;
    document.onpointerdown = Tootsville.UI.FurnitureBuilder.clickHelper;
    document.setPointerCapture (pointerId);
};

/**
 * Start rotating the active item
 */
Tootsville.UI.FurnitureMover.rotateItem = function (event) {
    Tootsville.UI.FurnitureMover.mouseMode = 'rotate';
    Tootsville.UI.FurnitureMover.captureMouseDrag (event.pointerId);
    // TODO alter decorations
};

/**
 * Start positioning the active item
 */
Tootsville.UI.FurnitureMover.positionItem = function (event) {
    Tootsville.UI.FurnitureMover.mouseMode = 'position';
    Tootsville.UI.FurnitureMover.captureMouseDrag (event.pointerId);
    // TODO alter decorations
};

/**
 * Close the furniture moving interface and commit or abandon changes
 */
Tootsville.UI.FurnitureMover.endArranging = function (keepChangesP) {
    Tootsville.UI.FurnitureMover.releaseMouseDrag ();
    Tootsville.UI.FurnitureMover.destroyDecorations ();
    if (keepChangesP) {
        // TODO, send changes to server
        // Tootsville.Util.infinity ( . . . );
    } else {
        const item = Tootsville.UI.FurnitureMover.activeItem;
        item.model.position = item.originalPosition;
        item.model.rotation = item.originalRotation;
    }
};

/**
 * Destroy the move and rotate decorations
 */
Tootsville.UI.FurnitureMover.destroyDecorations = function () {
    // TODO
};

/**
 * Add the move and rotate decorations to ``item''
 */
Tootsville.UI.FurnitureMover.addDecorations = function (item) {
    // TODO
};

/**
 * Begin arranging the position and facing of ``item''
 *
 * Adds UI buttons to translate or rotate the item
 */
Tootsville.UI.FurnitureMover.beginArranging = function (item) {
    Tootsville.UI.FurnitureMover.activeItem = item;
    item.originalPosition = item.model.position;
    item.originalRotation = item.model.rotation;
    Tootsville.UI.FurnitureMover.addDecorations (item);
};
