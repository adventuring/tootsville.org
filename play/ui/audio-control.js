Tootsville.audio = {
    currentVolume: 100,
    savedVolume: 100,
    volumeUp: function() {
        setVolume(Math.min(100, 10 + Tootsville.audio.currentVolume));
    },
    volumeDown: function() {
        setVolume(Math.max(0, (Tootsville.audio.currentVolume - 10)));
    },

    volumeMute: function() {
        if (Tootsville.audio.currentVolume < 9) {
            Tootsville.audio.setVolume(Tootsville.audio.savedVolume);
        } else {
            Tootsville.audio.savedVolume = Tootsville.audio.currentVolume;
            Tootsville.audio.setVolume(0);
        }
    },

    updateVolumeUI: function() {
        Tootsville.audio.updateVolumeSlider();
        Tootsville.audio.updateVolumeMuteIcon();
    },

    setVolume: function(newVolume) {
        Tootsville.audio.currentVolume = newVolume;
        // TODO: apply to sound system;
        Tootsville.audio.updateVolumeUI();
    },

    updateVolumeSlider: function() {
        var slider = document.getElementById('volume-slider');
        if (slider) {
            slider.value = Tootsville.audio.currentVolume;
            slider.disabled = false;
        }
    },

    updateVolumeMuteIcon: function() {
        var muteIcon = document.getElementById('mute-icon');
        if (Tootsville.audio.currentVolume < 9) {
            muteIcon.style.color = 'red';
        } else {
            muteIcon.style.color = 'black';
        }
    }

};
