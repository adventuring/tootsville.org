(function () {

    function done (ev) {
        ev.preventDefault();
    }
    
    Tootsville.ui.keys = {

        speakIt: function() {
            document.getElementById('talk-speak').focus();
            return false; // Let <INPUT> edit handle it.
        },

        clearTalk: function() {
            document.getElementById('talk-speak').value = '';
        },

        onKeyDown: function (ev) {
            // TODO: numeric keypad to arrow mapping
            if (ev.char != "") {
                if (ev.ctrlKey || ev.altKey || ev.metaKey) {
                    // TODO
                } else {
                    return Tootsville.ui.keys.speakIt();
                }
            } else {
                if (ev.key == "Escape" || ev.key == "Attn" ||
                    ev.key == "Cancel" ) {
                    if (Tootsville.hud.closePanel()) { done(ev); }
                } else if (ev.key == "Props") {
                    Tootsville.hud.toggleHUDPanel('control-panel');
                    done(ev);
                } else if (ev.key == "ContextMenu") {
                    Tootsville.hud.showHUDPanel('control-panel');
                    done(ev);
                } else if (ev.key == "PageUp") {
                    if (ev.ctrlKey) {
                        Tootsville.hud.toggleHUDPanel('control-panel');
                        done(ev);
                    } else {
                        if (Tootsville.hud.closePanel()) { done(ev); }
                    }
                } else if (ev.key == "PageDown") {
                    Tootsville.hud.showHUDPanel('contacts');
                    done(ev);
                } else if (ev.key == "Home") {
                    if (Tootsville.ui.talkBoxOpenP) {
                        return Tootsville.ui.keys.speakIt();
                    } else {
                        // TODO: Open talk box
                    }
                } else if (ev.key == "End") {
                    if (Tootsville.ui.talkBoxOpenP) {
                        // TODO: Close talk box
                    } else {
                        // TODO: "bump" animation to evidence that it's closed
                    }
                } else if (ev.key == "Clear") {
                    Tootsville.ui.keys.clearTalk();
                    done(ev);
                } else
                    // keys to *always* pass to input.
                    if (ev.key == "Backspace" ||
                        ev.key == "Copy" || ev.key == "Cut" ||
                        ev.key == "Delete" || ev.key == "ExSel" ||
                        ev.key == "Paste" || ev.key == "Redo" ||
                        ev.key == "Undo" || ev.key == "AllCandidates" ||
                        ev.key == "Alphanumeric" || ev.key == "CodeInput" ||
                        ev.key == "Compose" || ev.key == "Convert" ||
                        ev.key == "Dead" || ev.key == "FinalMode" ||
                        ev.key == "GroupFirst" || ev.key == "GroupLast" ||
                        ev.key == "GroupNext" || ev.key == "GroupPrevious" ||
                        ev.key == "ModeChange" || ev.key == "NextCandidate" ||
                        ev.key == "NonConvert" || ev.key == "PreviousCandidate" ||
                        ev.key == "Process" || ev.key == "SingleCandidate" ||
                        ev.key == "AltGraph"
                       ) {
                        return speakIt();
                    } else if (ev.key == "CrSel" || ev.key == "Accept" || ev.key == "Select") {
                        // TODO
                    } else if (ev.key == "Insert") {
                        // TODO
                    } else if (ev.key == "Help" || ev.key == "F1") {
                        Tootsville.hud.showHUDPanel('help');
                    } else if (ev.key == "PrintScreen") {
                        // TODO
                    } else if (ev.key == "BrowserBack") {
                        done(ev);
                    } else if (ev.key == "BrowserStop") {
                        // TODO
                    } else if (ev.key == "New" || ev.key == "F5") {
                        // TODO
                    } else if (ev.key == "Execute" || ev.key == "F7") {
                        // TODO
                    } else if (ev.key == "MailSend" || ev.key == "F8") {
                        Tootsville.hud.showHUDPanel('debug');
                    }

            }
        }

    };

    document.addEventListener('keydown', Tootsville.ui.keys.onKeyDown);
})();
