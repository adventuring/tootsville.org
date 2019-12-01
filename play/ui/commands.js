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
if ( (!('UI' in Tootsville)) ||
     (!('Keys' in Tootsville.UI)) ||
     (!('HUD' in Tootsville.UI)))
{ throw new Error("Can't map without keys and HUD"); }

/**
 *
 */
Tootsville.UI.commands =
    { 'beginning-of-line': Tootsville.UI.Keys.beginningOfLine,
      'end-of-line': Tootsville.UI.Keys.endOfLine,
      'backward-char': Tootsville.UI.Keys.backwardChar,
      'forward-char': Tootsville.UI.Keys.forwardChar,
      'prefix-C-c': Tootsville.UI.Keys.prefixCc,
      'prefix-C-x': Tootsville.UI.Keys.prefixCx,
      'delete-char': Tootsville.UI.Keys.deleteChar,
      'delete-backward-char': Tootsville.UI.Keys.deleteBackwardChar,
      'keyboard-quit': Tootsville.UI.Keys.keyboardQuit,
      'help': Tootsville.UI.Keys.help,
      'kill-line': Tootsville.UI.Keys.killLine,
      'next-history-line': Tootsville.UI.Keys.nextHistoryLine,
      'prior-history-line': Tootsville.UI.Keys.priorHistoryLine,
      'isearch': Tootsville.UI.Keys.isearch,
      'isearch-backward': Tootsville.UI.Keys.isearchBackward,
      'transpose-chars': Tootsville.UI.Keys.transposeChars,
      'kill-region': Tootsville.UI.Keys.killRegion,
      'yank': Tootsville.UI.Keys.yank,
      'speak-line': Tootsville.UI.Keys.speakLine,
      'begin-speaking': Tootsville.UI.Keys.beginSpeaking,
      'begin-whispering': Tootsville.UI.Keys.beginWhispering,
      'begin-shouting': Tootsville.UI.Keys.beginShouting,
      'clear-hud': Tootsville.UI.HUD.clearHUD,
      'control-panel': Tootsville.UI.HUD.showControlPanel,
      'toggle-control-panel': Tootsville.UI.HUD.toggleControlPanel,
      'contacts': Tootsville.UI.HUD.showContacts,
      'mobile': Tootsville.UI.HUD.showMobile,
      'camera': Tootsville.UI.HUD.showCamera,
      'open-talk': Tootsville.UI.openTalkBox,
      'toggle-talk': Tootsville.UI.toggleTalkBox,
      'close-talk': Tootsville.UI.closeTalkBox,
      'text-entry': Tootsville.UI.Keys.textEntry,
      'backward-word': Tootsville.UI.Keys.backwardWord,
      'forward-word': Tootsville.UI.Keys.forwardWord,
      'capitalize-word': Tootsville.UI.Keys.capitalizeWord,
      'upcase-word': Tootsville.UI.Keys.upcaseWord,
      'downcase-word': Tootsville.UI.Keys.downcaseWord,
      'backward-sentence': Tootsville.UI.Keys.backwardSentence,
      'forward-sentence': Tootsville.UI.Keys.forwardSentence,
      'select-all': Tootsville.UI.Keys.selectAll,
      'kill-sentence': Tootsville.UI.Keys.killSentence,
      'kill-word': Tootsville.UI.Keys.killWord,
      'transpose-words': Tootsville.UI.Keys.transposeWords,
      'kill-ring-save': Tootsville.UI.Keys.killRingSave,
      'execute-extended-command': Tootsville.UI.Keys.executeExtendedCommand,
      'yank-pop': Tootsville.UI.Keys.yankPop,
      'insert-char': Tootsville.UI.Keys.insertChar
    };

/**
 *
 */
Tootsville.UI.runCommand = function (command, event)
{ var cmd = Tootsville.UI.commands[ command ];
  if (cmd) { (cmd)(event); } };
