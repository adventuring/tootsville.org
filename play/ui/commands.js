/* -*- js2 -*-*/

/*@license
 *
 * ./play/ui/commands.js is part of Tootsville
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
if ( (!('ui' in Tootsville)) ||
     (!('keys' in Tootsville.ui)) ||
     (!('hud' in Tootsville.ui)))
{ throw new Error("Can't map without keys and HUD"); }

Tootsville.ui.commands =
    { 'beginning-of-line': Tootsville.ui.keys.beginningOfLine,
      'end-of-line': Tootsville.ui.keys.endOfLine,
      'backward-char': Tootsville.ui.keys.backwardChar,
      'forward-char': Tootsville.ui.keys.forwardChar,
      'prefix-C-c': Tootsville.ui.keys.prefixCc,
      'prefix-C-x': Tootsville.ui.keys.prefixCx,
      'delete-char': Tootsville.ui.keys.deleteChar,
      'delete-backward-char': Tootsville.ui.keys.deleteBackwardChar,
      'keyboard-quit': Tootsville.ui.keys.keyboardQuit,
      'help': Tootsville.ui.keys.help,
      'kill-line': Tootsville.ui.keys.killLine,
      'next-history-line': Tootsville.ui.keys.nextHistoryLine,
      'prior-history-line': Tootsville.ui.keys.priorHistoryLine,
      'isearch': Tootsville.ui.keys.isearch,
      'isearch-backward': Tootsville.ui.keys.isearchBackward,
      'transpose-chars': Tootsville.ui.keys.transposeChars,
      'kill-region': Tootsville.ui.keys.killRegion,
      'yank': Tootsville.ui.keys.yank,
      'speak-line': Tootsville.ui.keys.speakLine,
      'begin-speaking': Tootsville.ui.keys.beginSpeaking,
      'begin-whispering': Tootsville.ui.keys.beginWhispering,
      'begin-shouting': Tootsville.ui.keys.beginShouting,
      'clear-hud': Tootsville.ui.hud.clearHUD,
      'control-panel': Tootsville.ui.hud.showControlPanel,
      'toggle-control-panel': Tootsville.ui.hud.toggleControlPanel,
      'contacts': Tootsville.ui.hud.showContacts,
      'mobile': Tootsville.ui.hud.showMobile,
      'camera': Tootsville.ui.hud.showCamera,
      'open-talk': Tootsville.ui.openTalkBox,
      'toggle-talk': Tootsville.ui.toggleTalkBox,
      'close-talk': Tootsville.ui.closeTalkBox,
      'text-entry': Tootsville.ui.keys.textEntry,
      'backward-word': Tootsville.ui.keys.backwardWord,
      'forward-word': Tootsville.ui.keys.forwardWord,
      'capitalize-word': Tootsville.ui.keys.capitalizeWord,
      'upcase-word': Tootsville.ui.keys.upcaseWord,
      'downcase-word': Tootsville.ui.keys.downcaseWord,
      'backward-sentence': Tootsville.ui.keys.backwardSentence,
      'forward-sentence': Tootsville.ui.keys.forwardSentence,
      'select-all': Tootsville.ui.keys.selectAll,
      'kill-sentence': Tootsville.ui.keys.killSentence,
      'kill-word': Tootsville.ui.keys.killWord,
      'transpose-words': Tootsville.ui.keys.transposeWords,
      'kill-ring-save': Tootsville.ui.keys.killRingSave,
      'execute-extended-command': Tootsville.ui.keys.executeExtendedCommand,
      'yank-pop': Tootsville.ui.keys.yankPop,
      'insert-char': Tootsville.ui.keys.insertChar
    };

Tootsville.ui.runCommand = function (command, event)
{ var cmd = Tootsville.ui.commands[ command ];
  if (cmd) { (cmd)(event); } };
