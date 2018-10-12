(function () {

    function done(ev) {
        ev.preventDefault();
    }

    function speakIt() {
        document.getElementById('talk-speak').focus();
        return false; // Let <INPUT> edit handle it.
    }

    function clearTalk() {
        document.getElementById('talk-speak').value = '';
    }

    document.addEventListener('keydown', function (ev) {
        /// DEBUG console.log(ev);
        // TODO: numeric keypad to arrow mapping
        if (ev.char != "") {
            if (ev.ctrlKey || ev.altKey || ev.metaKey) {
                // TODO
            } else {
                return speakIt();
            }
        } else {
            if (ev.key == "Escape" || ev.key == "Attn" ||
                ev.key == "Cancel" || ev.key == "Props" ) {
                if (Tootsville.hud.closePanel()) { done(ev); }
            } if (ev.key == "ContextMenu") {
                showHUDPanel('control-panel');
                done(ev);
            } else if (ev.key == "PageUp") {
                if (ev.ctrlKey) {
                    toggleHUDPanel('control-panel');
                    done(ev);
                } else {
                    if (Tootsville.hud.closePanel()) { done(ev); }
                }
            } else if (ev.key == "PageDown") {
                showHUDPanel('contacts');
                done(ev);
            } else if (ev.key == "Home") {
                if (Tootsville.ui.talkBoxOpenP) {
                    return speakIt();
                } else {
                    // TODO
                }
            } else if (ev.key == "End") {
                if (Tootsville.ui.talkBoxOpenP) {
                    return speakIt();
                } else {
                    // TODO
                }
            } else if (ev.key == "Clear") {
                clearTalk();
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
                    showHUDPanel('help');
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
                    // TODO
                }

        }
    });

})();
