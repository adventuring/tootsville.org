/**
 * AudioAutoplayFix.js - Audio Autoplay Policy Fix
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Handles modern browser autoplay policies and provides user interaction-based audio initialization.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.UI.AudioAutoplayFix = {
  
  /**
   * Audio context state
   */
  audioContext: null,
  gainNode: null,
  isAudioEnabled: false,
  pendingAudioElements: [],
  userInteracted: false,
  
  /**
   * Initialize the audio autoplay fix system
   */
  initialize: function() {
    console.log("AudioAutoplayFix: Initializing audio autoplay fix system");
    
    // Create audio context
    this.createAudioContext();
    
    // Set up event listeners for user interaction
    this.setupUserInteractionListeners();
    
    // Check if audio is already enabled
    this.checkAudioState();
    
    // Set up periodic checks
    this.setupPeriodicChecks();
  },
  
  /**
   * Create audio context with fallbacks
   */
  createAudioContext: function() {
    try {
      // Try to create audio context
      this.audioContext = new (window.AudioContext || window.webkitAudioContext)();
      this.gainNode = this.audioContext.createGain();
      this.gainNode.connect(this.audioContext.destination);
      
      console.log("AudioAutoplayFix: Audio context created successfully");
    } catch (error) {
      console.error("AudioAutoplayFix: Failed to create audio context:", error);
      this.audioContext = null;
    }
  },
  
  /**
   * Set up event listeners for user interaction
   */
  setupUserInteractionListeners: function() {
    const interactionEvents = [
      'click', 'touchstart', 'touchend', 'keydown', 'keyup', 
      'mousedown', 'mouseup', 'pointerdown', 'pointerup'
    ];
    
    const handleUserInteraction = () => {
      if (!this.userInteracted) {
        this.userInteracted = true;
        this.enableAudio();
        console.log("AudioAutoplayFix: User interaction detected, enabling audio");
      }
    };
    
    // Add listeners to document
    interactionEvents.forEach(event => {
      document.addEventListener(event, handleUserInteraction, { 
        once: true, 
        passive: true 
      });
    });
    
    // Add listeners to specific interactive elements
    const interactiveElements = document.querySelectorAll(
      'button, a, input, select, textarea, [role="button"], [tabindex]'
    );
    
    interactiveElements.forEach(element => {
      interactionEvents.forEach(event => {
        element.addEventListener(event, handleUserInteraction, { 
          once: true, 
          passive: true 
        });
      });
    });
  },
  
  /**
   * Check current audio state
   */
  checkAudioState: function() {
    if (this.audioContext) {
      // Check if audio context is suspended
      if (this.audioContext.state === 'suspended') {
        console.log("AudioAutoplayFix: Audio context is suspended");
        this.isAudioEnabled = false;
      } else if (this.audioContext.state === 'running') {
        console.log("AudioAutoplayFix: Audio context is running");
        this.isAudioEnabled = true;
        this.processPendingAudio();
      }
    }
  },
  
  /**
   * Set up periodic checks for audio state
   */
  setupPeriodicChecks: function() {
    setInterval(() => {
      this.checkAudioState();
    }, 5000); // Check every 5 seconds
  },
  
  /**
   * Enable audio after user interaction
   */
  enableAudio: function() {
    if (!this.audioContext) {
      console.warn("AudioAutoplayFix: No audio context available");
      return;
    }
    
    // Resume audio context if suspended
    if (this.audioContext.state === 'suspended') {
      this.audioContext.resume().then(() => {
        console.log("AudioAutoplayFix: Audio context resumed successfully");
        this.isAudioEnabled = true;
        this.processPendingAudio();
      }).catch(error => {
        console.error("AudioAutoplayFix: Failed to resume audio context:", error);
      });
    }
    
    // Enable all existing audio elements
    this.enableAllAudioElements();
  },
  
  /**
   * Enable all existing audio elements
   */
  enableAllAudioElements: function() {
    const audioElements = document.querySelectorAll('audio');
    
    audioElements.forEach(audio => {
      this.enableAudioElement(audio);
    });
  },
  
  /**
   * Enable a specific audio element
   * @param {HTMLAudioElement} audioElement - The audio element to enable
   */
  enableAudioElement: function(audioElement) {
    if (!audioElement) return;
    
    // Set volume to current volume setting
    if (Tootsville.UI.Audio && Tootsville.UI.Audio.currentVolume !== undefined) {
      audioElement.volume = Tootsville.UI.Audio.currentVolume / 100;
    }
    
    // Try to play if it has autoplay attribute
    if (audioElement.hasAttribute('autoplay') && !audioElement.played.length) {
      this.playAudioElement(audioElement);
    }
  },
  
  /**
   * Play an audio element with error handling
   * @param {HTMLAudioElement} audioElement - The audio element to play
   * @returns {Promise} Promise that resolves when audio starts playing
   */
  playAudioElement: function(audioElement) {
    if (!audioElement) return Promise.reject(new Error("No audio element provided"));
    
    return new Promise((resolve, reject) => {
      // Check if audio is already playing
      if (!audioElement.paused) {
        resolve();
        return;
      }
      
      // Set up event listeners
      const onCanPlay = () => {
        audioElement.removeEventListener('canplay', onCanPlay);
        audioElement.removeEventListener('error', onError);
        
        // Try to play
        const playPromise = audioElement.play();
        
        if (playPromise !== undefined) {
          playPromise.then(() => {
            console.log("AudioAutoplayFix: Audio element started playing successfully");
            resolve();
          }).catch(error => {
            console.warn("AudioAutoplayFix: Failed to play audio element:", error);
            // Add to pending list for later retry
            this.addPendingAudio(audioElement);
            reject(error);
          });
        } else {
          resolve();
        }
      };
      
      const onError = (error) => {
        audioElement.removeEventListener('canplay', onCanPlay);
        audioElement.removeEventListener('error', onError);
        console.error("AudioAutoplayFix: Audio element error:", error);
        reject(error);
      };
      
      audioElement.addEventListener('canplay', onCanPlay);
      audioElement.addEventListener('error', onError);
      
      // Load audio if not already loaded
      if (audioElement.readyState === 0) {
        audioElement.load();
      }
    });
  },
  
  /**
   * Add audio element to pending list
   * @param {HTMLAudioElement} audioElement - The audio element to add
   */
  addPendingAudio: function(audioElement) {
    if (!this.pendingAudioElements.includes(audioElement)) {
      this.pendingAudioElements.push(audioElement);
      console.log("AudioAutoplayFix: Added audio element to pending list");
    }
  },
  
  /**
   * Process all pending audio elements
   */
  processPendingAudio: function() {
    if (!this.isAudioEnabled || this.pendingAudioElements.length === 0) {
      return;
    }
    
    console.log(`AudioAutoplayFix: Processing ${this.pendingAudioElements.length} pending audio elements`);
    
    const pendingElements = [...this.pendingAudioElements];
    this.pendingAudioElements = [];
    
    pendingElements.forEach(audioElement => {
      this.playAudioElement(audioElement).catch(error => {
        console.warn("AudioAutoplayFix: Failed to play pending audio element:", error);
        // Re-add to pending list if it's still valid
        if (audioElement.parentNode) {
          this.addPendingAudio(audioElement);
        }
      });
    });
  },
  
  /**
   * Create a new audio element with autoplay fix
   * @param {Object} options - Audio element options
   * @param {string} options.src - Audio source URL
   * @param {boolean} options.autoplay - Whether to autoplay
   * @param {boolean} options.loop - Whether to loop
   * @param {number} options.volume - Volume level (0-1)
   * @returns {HTMLAudioElement} The created audio element
   */
  createAudioElement: function(options = {}) {
    const audio = document.createElement('audio');
    
    if (options.src) {
      audio.src = options.src;
    }
    
    if (options.autoplay) {
      audio.autoplay = true;
    }
    
    if (options.loop) {
      audio.loop = true;
    }
    
    if (options.volume !== undefined) {
      audio.volume = options.volume;
    } else if (Tootsville.UI.Audio && Tootsville.UI.Audio.currentVolume !== undefined) {
      audio.volume = Tootsville.UI.Audio.currentVolume / 100;
    }
    
    // Add to document
    document.body.appendChild(audio);
    
    // Try to enable/play if audio is enabled
    if (this.isAudioEnabled) {
      this.enableAudioElement(audio);
    } else {
      this.addPendingAudio(audio);
    }
    
    return audio;
  },
  
  /**
   * Update volume for all audio elements
   * @param {number} volume - New volume level (0-100)
   */
  updateVolume: function(volume) {
    const audioElements = document.querySelectorAll('audio');
    
    audioElements.forEach(audio => {
      audio.volume = volume / 100;
    });
    
    // Update gain node if available
    if (this.gainNode) {
      this.gainNode.gain.setValueAtTime(volume / 100, this.audioContext.currentTime);
    }
  },
  
  /**
   * Get audio state information
   * @returns {Object} Audio state information
   */
  getAudioState: function() {
    return {
      isEnabled: this.isAudioEnabled,
      userInteracted: this.userInteracted,
      contextState: this.audioContext ? this.audioContext.state : 'unavailable',
      pendingCount: this.pendingAudioElements.length,
      totalAudioElements: document.querySelectorAll('audio').length
    };
  },
  
  /**
   * Show audio permission request UI
   */
  showAudioPermissionUI: function() {
    // Create audio permission request element
    const permissionUI = document.createElement('div');
    permissionUI.id = 'audio-permission-ui';
    permissionUI.style.cssText = `
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      background: white;
      border: 2px solid #007bff;
      border-radius: 10px;
      padding: 20px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      z-index: 10000;
      text-align: center;
      max-width: 400px;
    `;
    
    permissionUI.innerHTML = `
      <h3>Enable Audio</h3>
      <p>Click the button below to enable audio in Tootsville.</p>
      <button id="enable-audio-btn" style="
        background: #007bff;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
      ">Enable Audio</button>
      <button id="disable-audio-btn" style="
        background: #6c757d;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        cursor: pointer;
        font-size: 16px;
        margin-left: 10px;
      ">No Thanks</button>
    `;
    
    document.body.appendChild(permissionUI);
    
    // Add event listeners
    document.getElementById('enable-audio-btn').addEventListener('click', () => {
      this.enableAudio();
      permissionUI.remove();
    });
    
    document.getElementById('disable-audio-btn').addEventListener('click', () => {
      permissionUI.remove();
    });
  },
  
  /**
   * Clean up resources
   */
  cleanup: function() {
    // Close audio context
    if (this.audioContext) {
      this.audioContext.close();
    }
    
    // Clear pending elements
    this.pendingAudioElements = [];
    
    console.log("AudioAutoplayFix: Cleanup completed");
  }
};

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    Tootsville.UI.AudioAutoplayFix.initialize();
  });
} else {
  Tootsville.UI.AudioAutoplayFix.initialize();
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = Tootsville.UI.AudioAutoplayFix;
}
