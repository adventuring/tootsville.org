/* -*- js2 -*- */
/* @license 
 * play/ui/gamepad-layouts.js is part of Tootsville
 *
 * Copyright © 2008-2017, Bruce-Robert  Pocock; Copyright © 2018, 2019,
 * the Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software: you can redistribute it and/or modify
 * it  under the  terms of  the GNU  Affero General  Public License  as
 * published by the  Free Software Foundation; either version  3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the  hope that it will be useful, but
 * WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
 * MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
 * Affero General Public License for more details.
 *
 * You should  have received a  copy of  the GNU Affero  General Public
 * License    along     with    this     program.    If     not,    see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 */

/**
 * The known layouts and mappings to names of several popular types of controllers.
 *
 * This provides a name for the type of controller based upon its USB ID
 * string  (hex codes),  as  well as  names for  its  buttons and  axes.
 * This will  allow the  end user  to configure their  device in  a more
 * user-friendly way; eg,  by identifying a button as  ``A'' rather than
 * ``button 0'' they will be more  easily able to identify the controls.
 * (Seriously, who can remember if ``Start'' is button 8 or 7?)
 *
 * Supported (so far) are all the gamepads I (BRP) use, which is to say:
 *
 * @itemize
 * @item
 * Generic NES-style USB gamepad
 * @item
 * Nintendo Switch USB gamepad
 * @item
 * SEGA Saturn style USB gamepad
 * @item
 * XBox 360 USB gamepad
 * @end itemize
 */
Tootsville.gamepadLayouts =
{ '081f-e401':
  { name: 'NES USB Gamepad',
    axes: { Dpad: [0, 1] },
    buttons: { A: 1, B: 0, Select: 8, Start: 9 } },
  '12bd-d013':
  { name: 'SEGA Saturn USB Gamepad',
    axes: { Dpad: [0, 1] },
    buttons: { A: 0, B: 1, C: 2,
               X: 3, Y: 4, Z: 5,
               Start: 8,
               L: 6, R: 7 } },
  '0e6f-011f':
  { name: 'XBox360 USB Gamepad',
    axes: { 'Left Stick': [ 0, 1 ],
            'Right Stick': [ 3, 4 ],
            Dpad: [ 6, 7 ],
            'Left Trigger': 2,
            'Right Trigger': 5 },
    buttons: { A: 0, B: 1, X: 2, Y: 3,
               L: 4, R: 5,
               'Left Stick': 9,
               'Right Stick': 10,
               Back: 6,
               Start: 7,
               'Circled X': 8 } },
  '20db-a711':
  { name: 'Nintendo Switch USB Gamepad',
    axes: { 'Left Stick': [0, 1],
            'Right Stick': [2, 3],
            Dpad: [4, 5] },
    buttons: { A: 2, B: 1, X: 3, Y: 0,
               L: 4, ZL: 6, R: 5, ZR: 7,
               '-': 8, '+': 9,
               'Left Stick': 10,
               'Right Stick': 11,
               Screenshot: 12,
               Home: 13 } } };
