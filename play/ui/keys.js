/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/keys.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { Ui: { KEYS: {} }}; }
if (!('UI' in Tootsville)) { Tootsville.UI = { Keys: {} }; }
if (!('Keys' in Tootsville.UI)) { Tootsville.UI.Keys = {}; }

Tootsville.UI.Keys.speakIt = function ()
{ document.getElementById('talk-speak').focus();
  return false; // Let <INPUT> edit handle it.
};

Tootsville.UI.Keys.clearTalk = function ()
{ document.getElementById('talk-speak').value = ''; };;

Tootsville.UI.Keys.bindings =
    { single: {
        Tab: null,
        Enter: 'speak-line',
        ArrowDown: null,
        ArrowLeft: 'backward-char',
        ArrowRight: 'forward-char',
        ArrowUp: null,
        End: 'end-of-line',
        Home: 'beginning-of-line',
        PageDown: null,
        PageUp: null,
        Backspace: 'delete-backward',
        Clear: 'clear',
        Copy: 'kill-ring-save',
        Cut: 'kill-region',
        Delete: 'delete',
        EraseEof: 'kill-line',
        Insert: null,
        Paste: 'yank',
        Redo: null,
        Undo: null,
        Accept: null,
        Again: null,
        Attn: null,
        Cancel: null,
        ContextMenu: 'toggle-control-panel',
        Escape: 'close-all-panels',
        Execute: 'execute-extended-command',
        Find: 'isearch',
        Finish: null,
        Help: 'help',
        Pause: null,
        Play: null,
        Props: 'show-paperdoll',
        Select: null,
        ZoomIn: null,
        ZoomOut: null,
        PrintScreen: 'show-camera',
        WakeUp: null,
        F1: 'help',
        F2: null,
        F3: null,
        F4: null,
        F5: null,
        F6: null,
        F7: null,
        F8: null,
        F9: null,
        F10: null,
        F11: null,
        F12: null,
        F13: null,
        F14: null,
        F15: null,
        F16: null,
        F17: null,
        F18: null,
        F19: null,
        F20: null,
        Soft1: null,
        Soft2: null,
        Soft3: null,
        Soft4: null,
        ChannelDown: null,
        ChannelUp: null,
        MediaFastForward: null,
        MediaPause: null,
        MediaPlay: null,
        MediaPlayPause: null,
        MediaRecord: null,
        MediaRewind: null,
        MediaStop: null,
        MediaTrackNext: null,
        MediaTrackPrevious: null
    },
      withControl:
      { 'a': 'beginning-of-line',
        'b': 'backward-word',
        'c': 'prefix C-c',
        'd': 'delete',
        'e': 'end-of-line',
        'f': 'forward-char',
        'g': 'keyboard-quit',
        'h': 'help',
        'k': 'kill-line',
        'n': 'next-history-line',
        'p': 'prior-history-line',
        'r': 'isearch-backward',
        's': 'isearch',
        't': 'transpose-chars',
        'w': 'kill-region',
        'x': 'prefix-C-x',
        'y': 'yank'
      },
      withMeta: {

      },
      withHyper: {},
      withSuper: {},
      afterControlX:
      { single: {},
        withControl: {},
        withMeta: {} },
      afterControlC:
      { single: {},
        withControl: {},
        withMeta: {} }};

Tootsville.UI.Keys.prefixed = false;

if (!('runCommand' in Tootsville.UI))
{ /* placeholder until Commands loads */
    Tootsville.UI.runCommand = function (binding, event) {}; }

Tootsville.UI.Keys.onKeypress = function (ev)
{ var coda = 'single';
  if (ev.altKey || ev.metaKey ) { coda = 'withMeta'; }
  else if (ev.ctrlKey) { coda = 'withControl'; }
  console.debug ("Key: " + coda + " " + ev.key);
  if (Tootsville.UI.Keys.prefixed)
  { Tootsville.UI.runCommand(Tootsville.UI.Keys.bindings["afterControl" + Tootsville.UI.Keys.prefixed][coda], ev); }
  else
  { Tootsville.UI.runCommand(Tootsville.UI.Keys.bindings[coda][ev.key], ev); } };
