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
     (!('keys' in Tootsville.UI)) ||
     (!('hud' in Tootsville.UI)))
{ throw new Error("Can't map without keys and HUD"); }

/**
 *
 */
Tootsville.UI.commands =
    { 'beginning-of-line': Tootsville.UI.keys.beginningOfLine,
      'end-of-line': Tootsville.UI.keys.endOfLine,
      'backward-char': Tootsville.UI.keys.backwardChar,
      'forward-char': Tootsville.UI.keys.forwardChar,
      'prefix-C-c': Tootsville.UI.keys.prefixCc,
      'prefix-C-x': Tootsville.UI.keys.prefixCx,
      'delete-char': Tootsville.UI.keys.deleteChar,
      'delete-backward-char': Tootsville.UI.keys.deleteBackwardChar,
      'keyboard-quit': Tootsville.UI.keys.keyboardQuit,
      'help': Tootsville.UI.keys.help,
      'kill-line': Tootsville.UI.keys.killLine,
      'next-history-line': Tootsville.UI.keys.nextHistoryLine,
      'prior-history-line': Tootsville.UI.keys.priorHistoryLine,
      'isearch': Tootsville.UI.keys.isearch,
      'isearch-backward': Tootsville.UI.keys.isearchBackward,
      'transpose-chars': Tootsville.UI.keys.transposeChars,
      'kill-region': Tootsville.UI.keys.killRegion,
      'yank': Tootsville.UI.keys.yank,
      'speak-line': Tootsville.UI.keys.speakLine,
      'begin-speaking': Tootsville.UI.keys.beginSpeaking,
      'begin-whispering': Tootsville.UI.keys.beginWhispering,
      'begin-shouting': Tootsville.UI.keys.beginShouting,
      'clear-hud': Tootsville.UI.HUD.clearHUD,
      'control-panel': Tootsville.UI.HUD.showControlPanel,
      'toggle-control-panel': Tootsville.UI.HUD.toggleControlPanel,
      'contacts': Tootsville.UI.HUD.showContacts,
      'mobile': Tootsville.UI.HUD.showMobile,
      'camera': Tootsville.UI.HUD.showCamera,
      'open-talk': Tootsville.UI.openTalkBox,
      'toggle-talk': Tootsville.UI.toggleTalkBox,
      'close-talk': Tootsville.UI.closeTalkBox,
      'text-entry': Tootsville.UI.keys.textEntry,
      'backward-word': Tootsville.UI.keys.backwardWord,
      'forward-word': Tootsville.UI.keys.forwardWord,
      'capitalize-word': Tootsville.UI.keys.capitalizeWord,
      'upcase-word': Tootsville.UI.keys.upcaseWord,
      'downcase-word': Tootsville.UI.keys.downcaseWord,
      'backward-sentence': Tootsville.UI.keys.backwardSentence,
      'forward-sentence': Tootsville.UI.keys.forwardSentence,
      'select-all': Tootsville.UI.keys.selectAll,
      'kill-sentence': Tootsville.UI.keys.killSentence,
      'kill-word': Tootsville.UI.keys.killWord,
      'transpose-words': Tootsville.UI.keys.transposeWords,
      'kill-ring-save': Tootsville.UI.keys.killRingSave,
      'execute-extended-command': Tootsville.UI.keys.executeExtendedCommand,
      'yank-pop': Tootsville.UI.keys.yankPop,
      'insert-char': Tootsville.UI.keys.insertChar
    };

/**
 *
 */
Tootsville.UI.runCommand = function (command, event)
{ var cmd = Tootsville.UI.commands[ command ];
  if (cmd) { (cmd)(event); } };
