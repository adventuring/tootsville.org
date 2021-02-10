/* -*- js2 -*-*/

/**@license
 *
 * ./play/UI/Keys.js is part of Tootsville
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

if (!('Tootsville' in window)) { Tootsville = { UI: { Keys: {} }}; }
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
        ArrowDown: 'arrow-down',
        ArrowLeft: 'arrow-left',
        ArrowRight: 'arrow-right',
        ArrowUp: 'arrow-up',
        End: 'end-of-line',
        Home: 'beginning-of-line',
        PageDown: 'mobile',
        PageUp: 'paperdoll',
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
        Cancel: 'keyboard-quit',
        ContextMenu: 'toggle-control-panel',
        Escape: 'close-all-panels',
        BrowserStop: 'close-all-panels',
        Execute: 'execute-extended-command',
        Find: 'isearch',
        Finish: null,
        Help: 'help',
        Pause: null,
        Play: null,
        Props: 'paperdoll',
        Select: null,
        ZoomIn: null,
        ZoomOut: null,
        PrintScreen: 'camera',
        WakeUp: null,
        F1: 'help',
        F2: 'paperdoll',
        F3: 'mobile',
        F4: 'end-of-line',
        F5: 'hide-talk-box',
        F6: 'contacts',
        F7: 'swap-items',
        F8: 'pick-expression',
        F9: 'camera',
        F10: 'control-panel',
        F11: 'toggle-fullscreen',
        F12: 'toggle-mute',
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
      withShift:
      {
          'PageUp': 'swap-items',
          'PageDown': 'camera',
          'Home': 'use-item',
          'F12': 'volume-up'
      },
      withControl:
      { 'a': 'beginning-of-line',
        'b': 'backward-char',
        'c': 'prefix-C-c',
        'd': 'delete',
        'e': 'end-of-line',
        'f': 'forward-char',
        'g': 'keyboard-quit',
        'h': 'help',
        'k': 'kill-line',
        'n': 'next-history-line',
        'p': 'prior-history-line',
        'q': 'reset-login',
        'r': 'isearch-backward',
        's': 'isearch',
        't': 'transpose-chars',
        'w': 'kill-region',
        'x': 'prefix-C-x',
        'y': 'yank',
        'PageDown': 'show-contacts',
        'PageUp': 'control-panel',
        'Home': 'hide-talk-box',
        ',': 'whisper',
        '.': 'shout',
        "'": 'talk',
        '/': 'talk',
        'F12': 'volume-down',
        'ArrowDown': 'volume-down',
        'ArrowUp': 'volume-up'
      },
      withMeta: {
          'c': 'control-panel',
          'm': 'mobile',
          'p': 'paperdoll',
          'h': 'hide-talk-box',
          'PageDown': 'show-contacts',
          'PageUp': 'control-panel',
          'Home': 'hide-talk-box',
          ',': 'whisper',
          '.': 'shout',
          "'": 'talk',
          '/': 'talk',
          'F12': 'volume-down',
          'ArrowDown': 'volume-down',
          'ArrowUp': 'volume-up'
      },
      withHyper: {},
      withSuper: {},
      afterControlX:
      { single: {
          'h': 'select-all'
      },
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

Tootsville.UI.Keys.onKeyDown = function (ev)
{ Tootsville.UI.talkSpeak = document.getElementById('talk-speak');
  var coda = 'single';
  if (ev.altKey || ev.metaKey ) { coda = 'withMeta'; }
  else if (ev.ctrlKey) { coda = 'withControl'; }
  else if (ev.shiftKey) { coda = 'withShift'; }
  if (Tootsville.UI.Keys.prefixed)
  { let binding = Tootsville.UI.Keys.prefixed ["afterControl" + Tootsville.UI.Keys.prefixed][coda][ev.key];
    if (ev.key.length > 1 || binding)
        console.debug ("Keypress afterControl" + Tootsville.UI.Keys.prefixed + " " + coda + " " + ev.key + " bound to " + binding);
    Tootsville.UI.runCommand(binding, ev); }
  else
  { let binding = Tootsville.UI.Keys.bindings [coda][ev.key];
    if (ev.key.length > 1 || binding)
        console.debug ("Keypress " + coda + " " + ev.key + " bound to " + binding);
    Tootsville.UI.runCommand(binding, ev); } };

// 

/**
 * Move the cursor backwards one character in the speaking box.
 */
Tootsville.UI.Keys.backwardChar = function (event)
{ var point = Tootsville.UI.talkSpeak.selectionStart - 1;
  point = Math.max (0, point);
  Tootsville.UI.talkSpeak.focus ();
  Tootsville.UI.talkSpeak.setSelectionRange (point, point); };

/**
 * Move the cursor back to the previous sentence start.
 *
 * Sentences are defined  to be delimited by  period, exclamation point,
 * or question mark.
 */
Tootsville.UI.Keys.backwardSentence = function (event)
{ /* TODO */ };

/**
 * Move the cursor back one word.
 *
 * A word is considered to consist of contiguous letters or digits.
 */
Tootsville.UI.Keys.backwardWord = function (event)
{ /* TODO */ };

/**
 * Move the cursor to the start of the line.
 */
Tootsville.UI.Keys.beginningOfLine = function (event)
{ Tootsville.UI.talkSpeak.focus ();
  Tootsville.UI.HUD.openTalkBox (event);
  Tootsville.UI.talkSpeak.setSelectionRange (0,0); };

/**
 * Set the speaking volume to shouting.
 */
Tootsville.UI.Keys.beginShouting = function (event)
{ /* TODO */ };

/**
 * Set the  speaking volume to  speaking normally (neither  shouting nor
 * whispering)
 */
Tootsville.UI.Keys.beginSpeaking = function (event)
{ /* TODO */ };

/**
 * Set the speaking volume to whispering.
 */
Tootsville.UI.Keys.beginWhispering = function (event)
{ /* TODO */ };

/**
 * Upcase the  first letter of the  word under the cursor,  and downcase
 * the remaining letters.
 */
Tootsville.UI.Keys.capitalizeWord = function (event)
{ /* TODO */ };

/**
 * Delete the character to the left of the cursor.
 */
Tootsville.UI.Keys.deleteBackwardChar = function (event)
{ /* TODO */ };

/**
 * Delete the character to the right of the cursor.
 */
Tootsville.UI.Keys.deleteChar = function (event)
{ /* TODO */ };

/**
 * Downcase the word under the cursor.
 */
Tootsville.UI.Keys.downcaseWord = function (event)
{ /* TODO */ };

/**
 * Move the cursor to after the end of the line.
 */
Tootsville.UI.Keys.endOfLine = function (event)
{ Tootsville.UI.talkSpeak.focus ();
  Tootsville.UI.HUD.openTalkBox (event);
  Tootsville.UI.talkSpeak.setSelectionRange (-1, -1); };

/**
 * Reserved for future use.
 *
 * Should prompt  for an extended command  to execute. This is  bound to
 * M-x and is analogous to the function in Emacs.
 */
Tootsville.UI.Keys.executeExtendedCommand = function (event)
{ /* TODO */ };

/**
 * Move the cursor forward one character.
 */
Tootsville.UI.Keys.forwardChar = function (event)
{ var point = Tootsville.UI.talkSpeak.selectionEnd + 1;
  Tootsville.UI.talkSpeak.focus ();
  Tootsville.UI.talkSpeak.setSelectionRange (point, point); };

/**
 *
 */
Tootsville.UI.Keys.forwardSentence = function (event)
{ /* TODO */ };

/**
 *
 */
Tootsville.UI.Keys.forwardWord = function (event)
{ /* TODO */ };

/**
 * Load the game help panel
 */
Tootsville.UI.Keys.help = function (event)
{ Tootsville.UI.HUD.showHUDPanel('help'); };

/**
 *
 */
Tootsville.UI.Keys.insertChar = function (event)
{ /* TODO */ };

/**
 *
 */
Tootsville.UI.Keys.isearch = function (event)
{ /* TODO */ };

/**
 *
 */
Tootsville.UI.Keys.isearchBackward = function (event)
{ /* TODO */ };

/**
 * Quit keyboard action. Currently only resets the prefix keys.
 */
Tootsville.UI.Keys.keyboardQuit = function (event)
{ /* TODO */
    Tootsville.UI.Keys.prefixed = undefined; };

/**
 * Delete the entire contents of the speaking box.
 */
Tootsville.UI.Keys.killLine = function (event)
{ Tootsville.UI.talkSpeak.value = '';
  Tootsville.UI.talkSpeak.focus (); };

/**
 * Kill (cut) the selected region
 */
Tootsville.UI.Keys.killRegion = function (event)
{ const text = document.getElementById('talk-speak');
  navigator.clipboard.writeText (text.value.substr(text.selectionStart, text.selectionEnd));
  text.setRangeText (''); };

/**
 *
 */
Tootsville.UI.Keys.killRingSave = function (event)
{ /* TODO */ };

/**
 * Remove the current or previous sentence
 */
Tootsville.UI.Keys.killSentence = function (event)
{ /* TODO */ };

/**
 * Remove the current or previous word
 */
Tootsville.UI.Keys.killWord = function (event)
{ /* TODO */ };

/**
 * Navigate to the next line in the history of spoken lines.
 *
 * TODO. Currently just clears the input box.
 */
Tootsville.UI.Keys.nextHistoryLine = function (event)
{ document.getElementById('talk-speak').value = ''; };

/**
 * Sets the C-c prefix
 */
Tootsville.UI.Keys.prefixCc = function (event)
{ Tootsville.UI.Keys.prefixed = 'C'; };

/**
 * Sets the C-x prefix
 */
Tootsville.UI.Keys.prefixCx = function (event)
{ Tootsville.UI.Keys.prefixed = 'X'; };

/**
 * Move back through the history of spoken lines.
 *
 * TODO. Currently only recalls the last submitted text.
 */
Tootsville.UI.Keys.priorHistoryLine = function (event)
{ document.getElementById ('talk-speak').value = Tootsville.UI.recallText; };

/**
 * Select the entire buffer
 */
Tootsville.UI.Keys.selectAll = function (event)
{ document.getElementById('talk-speak').select (); };

/**
 * Speak the line currently in the buffer.
 *
 * TODO: If a Parrot message is open, instead dismiss it.
 */
Tootsville.UI.Keys.speakLine = function (event)
{ let el = document.getElementById ('talk-speak');
  Tootsville.UI.say (el.value);
  console.info ("Speaking: «" + el.value + "»");
  Tootsville.UI.recallText = el.value;
  el.value = ''; };

/**
 *
 */
Tootsville.UI.Keys.textEntry = function (event)
{ /* TODO */ };

/**
 * Switch the characters before and after the cursor
 */
Tootsville.UI.Keys.transposeChars = function (event)
{ /* TODO */ };

/**
 * Switch the word under the cursor with the prior word.
 *
 * When on a non-word character, switch the words before and after it.
 */
Tootsville.UI.Keys.transposeWords = function (event)
{ /* TODO */ };

/**
 *
 */
Tootsville.UI.Keys.upcaseWord = function (event)
{ /* TODO */ };

/**
 * Yank (paste) from the system's clipboard
 */
Tootsville.UI.Keys.yank = function (event)
{ const text = document.getElementById('talk-speak');
  text.setRangeText (navigator.clipboard.readText ()); };

/**
 *
 */
Tootsville.UI.Keys.yankPop = function (event)
{ /* TODO */ };

/**
 *
 */
Tootsville.UI.Keys.arrowLeft = function (event)
{ if (document.activeElement === Tootsville.UI.talkSpeak)
  { Tootsville.UI.Keys.backwardChar (event); }
  else
  { Tootsville.UI.takeOneStep (-1, 0); } };

/**
 *
 */
Tootsville.UI.Keys.arrowRight = function (event)
{ if (document.activeElement === Tootsville.UI.talkSpeak)
  { Tootsville.UI.Keys.forwardChar (event); }
  else
  { Tootsville.UI.takeOneStep (1, 0); } };

/**
 *
 */
Tootsville.UI.Keys.arrowUp = function (event)
{ if (document.activeElement === Tootsville.UI.talkSpeak)
  { Tootsville.UI.Keys.priorHistoryLine (event); }
  else
  { Tootsville.UI.takeOneStep (0, 1); } };

/**
 *
 */
Tootsville.UI.Keys.arrowDown = function (event)
{ if (document.activeElement === Tootsville.UI.talkSpeak)
  { Tootsville.UI.Keys.nextHistoryLine (event); }
  else
  { Tootsville.UI.takeOneStep (0, -1); } };
