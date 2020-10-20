/* -*- js2 -*-*/

/**@license
 *
 * ./play/ui/commands.js is part of Tootsville
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
if ( (!('UI' in Tootsville)) ||
     (!('Keys' in Tootsville.UI)) ||
     (!('HUD' in Tootsville.UI)))
{ throw new Error("Can't map without keys and HUD"); }

/**
 *
 */
Tootsville.UI.commands =
    {
        'arrow-down': ev => { Tootsville.UI.Keys.arrowDown (ev); },
        'arrow-left': ev => { Tootsville.UI.Keys.arrowLeft (ev); },
        'arrow-right': ev => { Tootsville.UI.Keys.arrowRight (ev); },
        'arrow-up': ev => { Tootsville.UI.Keys.arrowUp (ev); },
        'backward-char': ev => { Tootsville.UI.Keys.backwardChar (ev); },
        'backward-sentence': ev => { Tootsville.UI.Keys.backwardSentence (ev); },
        'backward-word': ev => { Tootsville.UI.Keys.backwardWord (ev); },
        'beginning-of-line': ev => { Tootsville.UI.Keys.beginningOfLine (ev); },
        'begin-shouting': ev => { Tootsville.UI.Keys.beginShouting (ev); },
        'begin-speaking': ev => { Tootsville.UI.Keys.beginSpeaking (ev); },
        'begin-whispering': ev => { Tootsville.UI.Keys.beginWhispering (ev); },
        'camera': ev => { Tootsville.UI.HUD.showCamera (ev); },
        'capitalize-word': ev => { Tootsville.UI.Keys.capitalizeWord (ev); },
        'clear-hud': ev => { Tootsville.UI.HUD.clearHUD (ev); },
        'close-talk': ev => { Tootsville.UI.closeTalkBox (ev); },
        'close-all-panels': ev => { Tootsville.UI.HUD.closePanel (); },
        'contacts': ev => { Tootsville.UI.HUD.showContacts (ev); },
        'control-panel': ev => { Tootsville.UI.HUD.showControlPanel (ev); },
        'delete-backward-char': ev => { Tootsville.UI.Keys.deleteBackwardChar (ev); },
        'delete-char': ev => { Tootsville.UI.Keys.deleteChar (ev); },
        'downcase-word': ev => { Tootsville.UI.Keys.downcaseWord (ev); },
        'end-of-line': ev => { Tootsville.UI.Keys.endOfLine (ev); },
        'execute-extended-command': ev => { Tootsville.UI.Keys.executeExtendedCommand (ev); },
        'forward-char': ev => { Tootsville.UI.Keys.forwardChar (ev); },
        'forward-sentence': ev => { Tootsville.UI.Keys.forwardSentence (ev); },
        'forward-word': ev => { Tootsville.UI.Keys.forwardWord (ev); },
        'help': ev => { Tootsville.UI.Keys.help (ev); },
        'hide-talk-box': ev => { Tootsville.UI.HUD.closeTalkBox (ev); },
        'insert-char': ev => { Tootsville.UI.Keys.insertChar (ev); },
        'isearch': ev => { Tootsville.UI.Keys.isearch (ev); },
        'isearch-backward': ev => { Tootsville.UI.Keys.isearchBackward (ev); },
        'keyboard-quit': ev => { Tootsville.UI.Keys.keyboardQuit (ev); },
        'kill-line': ev => { Tootsville.UI.Keys.killLine (ev); },
        'kill-region': ev => { Tootsville.UI.Keys.killRegion (ev); },
        'kill-ring-save': ev => { Tootsville.UI.Keys.killRingSave (ev); },
        'kill-sentence': ev => { Tootsville.UI.Keys.killSentence (ev); },
        'kill-word': ev => { Tootsville.UI.Keys.killWord (ev); },
        'mobile': ev => { Tootsville.UI.HUD.showMobile (ev); },
        'next-history-line': ev => { Tootsville.UI.Keys.nextHistoryLine (ev); },
        'open-talk': ev => { Tootsville.UI.openTalkBox (ev); },
        'paperdoll': ev => { Tootsville.UI.HUD.openPaperdoll (ev); },
        'prefix-C-c': ev => { Tootsville.UI.Keys.prefixCc (ev); },
        'prefix-C-x': ev => { Tootsville.UI.Keys.prefixCx (ev); },
        'prior-history-line': ev => { Tootsville.UI.Keys.priorHistoryLine (ev); },
        'reset-login': ev => { Tootsville.Login.quit (ev); },
        'select-all': ev => { Tootsville.UI.Keys.selectAll (ev); },
        'speak-line': ev => { Tootsville.UI.Keys.speakLine (ev); },
        'text-entry': ev => { Tootsville.UI.Keys.textEntry (ev); },
        'toggle-control-panel': ev => { Tootsville.UI.HUD.toggleControlPanel (ev); },
        'toggle-mute': ev => { Tootsville.UI.Audio.volumeMute (); },
        'toggle-talk': ev => { Tootsville.UI.toggleTalkBox (ev); },
        'transpose-chars': ev => { Tootsville.UI.Keys.transposeChars (ev); },
        'transpose-words': ev => { Tootsville.UI.Keys.transposeWords (ev); },
        'upcase-word': ev => { Tootsville.UI.Keys.upcaseWord (ev); },
        'volume-up': ev => { Tootsville.UI.Audio.volumeUp (); },
        'volume-down': ev => { Tootsville.UI.Audio.volumeDown (); },
        'yank': ev => { Tootsville.UI.Keys.yank (ev); },
        'yank-pop': ev => { Tootsville.UI.Keys.yankPop (ev); }
    };

/**
 *
 */
Tootsville.UI.runCommand = function (command, event)
{ var cmd = Tootsville.UI.commands[ command ];
  if (cmd)
  { (cmd)(event);
    /* Beat up browsers trying to steal our keystrokes */
    event.preventDefault ();
    event.stopPropagation ();
    event.cancelBubble = true; }
  else if (undefined === cmd)
  { return; }
  else { console.warn ("Keypress: No such command as " + command); }  };
