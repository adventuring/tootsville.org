if (!('Tootsville' in window)) { Tootsville = { ui: { keys: {} }}; }
if (!('ui' in Tootsville)) { Tootsville.ui = { keys: {} }; }
if (!('keys' in Tootsville.ui)) { Tootsville.ui.keys = {}; }

Tootsville.ui.keys.speakIt = function ()
{ document.getElementById('talk-speak').focus();
  return false; // Let <INPUT> edit handle it.
};

Tootsville.ui.keys.clearTalk = function ()
{ document.getElementById('talk-speak').value = ''; };;

Tootsville.ui.keys.bindings =
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

Tootsville.ui.keys.prefixed = false;

if (!('runCommand' in Tootsville.ui))
{ /* placeholder until Commands loads */
    Tootsville.ui.runCommand = function (binding, event) {}}

Tootsville.ui.keys.onKeypress = function (ev)
{ var coda = 'single';
  if (ev.altKey || ev.metaKey ) { coda = 'withMeta'; }
  else if (ev.ctrlKey) { coda = 'withControl'; }
  if (Tootsville.ui.keys.prefixed)
  { Tootsville.ui.runCommand(Tootsville.ui.keys.bindings["afterControl" + Tootsville.ui.keys.prefixed][coda], ev); }
  else
  { Tootsville.ui.runCommand(Tootsville.ui.keys.bindings[coda], ev); }}

document.addEventListener('keypress', Tootsville.ui.keys.onKeypress);
