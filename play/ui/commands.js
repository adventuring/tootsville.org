if (! 'Tootsville' in window) { Tootsville = { ui: { keys: {}}}; }
else if (! 'ui' in Tootsville) { Tootsville.ui = { keys: {}}; }
else if (! 'keys' in Tootsville) { Tootsville.ui.keys = {}; }

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
          'control-panel': Tootsville.hud.showControlPanel,
          'toggle-control-panel': Tootsville.hud.toggleControlPanel,
          'contacts': Tootsville.hud.showContacts,
          'mobile': Tootsville.hud.showMobile,
          'camera': Tootsville.hud.showCamera,
          'open-talk': Tootsville.ui.openTalkBox,
          'toggle-talk': Tootsville.ui.toggleTalkBox,
          'close-talk': Tootsville.ui.closeTalkBox,
          'text-entry': Tootsville.ui.keys.textEntry,
          'backward-word': Tootsville.ui.keys.backwardWord,
          'forward-word': Tootsville.ui.keys.forwardWord,
          'capitalize-word': Tootsville.ui.keys.capitalizeWord,
          'upcase-word': Tootsville.ui.keys.upcaseWord,
          'downcase-word': Tootsville.ui.keys.downcaseWord,
          'backward-sentence': Tootsville.ui.kays.backwardSentence,
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
  if (cmd) { (cmd)(event); } }

