/**
 * TootSpeechService.js - Modern Toot Speech Synthesis with Pitch and Duration Shifting
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Upgrades the legacy WaWa system to modern Web Audio API standards
 * with enhanced pitch shifting, duration control, and cross-browser compatibility.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 2.0.0
 */

import { EventEmitter } from 'events';

/**
 * Modern Toot Speech Synthesis Service
 * 
 * Handles pitch shifting and duration control for Toot speech using
 * modern Web Audio API features compatible with latest Firefox.
 */
class TootSpeechService extends EventEmitter {
  constructor() {
    super();
    
    // Audio context and nodes
    this.audioContext = null;
    this.gainNode = null;
    this.masterGain = null;
    
    // Speech configuration
    this.config = {
      baseVolume: 0.8,
      speechVolume: 0.6,
      pitchRange: { min: 0.5, max: 2.0 },
      durationRange: { min: 0.3, max: 3.0 },
      crossfadeDuration: 0.1,
      bufferCache: new Map(),
      maxCacheSize: 50
    };
    
    // Speech state
    this.isSpeaking = false;
    this.currentSpeech = null;
    this.speechQueue = [];
    
    // Audio assets
    this.audioAssets = {
      trumpet: 'https://jumbo.tootsville.org/Assets/Voices/5/cc0-elephant-trumpet-trimmed.webm',
      bellow: 'https://jumbo.tootsville.org/Assets/Voices/5/cc0-elephant-bellow-trimmed.webm'
    };
    
    // Initialize the service
    this.initialize();
  }

  /**
   * Initialize the audio context and nodes
   */
  async initialize() {
    try {
      // Create audio context with modern API
      this.audioContext = new (window.AudioContext || window.webkitAudioContext)();
      
      // Create gain nodes for volume control
      this.masterGain = this.audioContext.createGain();
      this.gainNode = this.audioContext.createGain();
      
      // Connect the audio graph
      this.gainNode.connect(this.masterGain);
      this.masterGain.connect(this.audioContext.destination);
      
      // Set initial volumes
      this.masterGain.gain.setValueAtTime(this.config.baseVolume, this.audioContext.currentTime);
      this.gainNode.gain.setValueAtTime(this.config.speechVolume, this.audioContext.currentTime);
      
      // Resume audio context if suspended
      if (this.audioContext.state === 'suspended') {
        await this.audioContext.resume();
      }
      
      console.log('TootSpeechService: Initialized successfully');
      this.emit('initialized');
      
    } catch (error) {
      console.error('TootSpeechService: Failed to initialize:', error);
      this.emit('error', { message: 'Failed to initialize audio context', error });
    }
  }

  /**
   * Set master volume
   * @param {number} volume - Volume level (0-1)
   */
  setMasterVolume(volume) {
    if (!this.masterGain) return;
    
    const clampedVolume = Math.max(0, Math.min(1, volume));
    this.masterGain.gain.setValueAtTime(clampedVolume, this.audioContext.currentTime);
    this.emit('volumeChanged', { volume: clampedVolume });
  }

  /**
   * Set speech volume
   * @param {number} volume - Volume level (0-1)
   */
  setSpeechVolume(volume) {
    if (!this.gainNode) return;
    
    const clampedVolume = Math.max(0, Math.min(1, volume));
    this.gainNode.gain.setValueAtTime(clampedVolume, this.audioContext.currentTime);
    this.emit('speechVolumeChanged', { volume: clampedVolume });
  }

  /**
   * Load and cache audio buffer
   * @param {string} url - Audio file URL
   * @returns {Promise<AudioBuffer>} Audio buffer
   */
  async loadAudioBuffer(url) {
    // Check cache first
    if (this.config.bufferCache.has(url)) {
      return this.config.bufferCache.get(url);
    }
    
    try {
      // Fetch audio data
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Failed to fetch audio: ${response.status}`);
      }
      
      const arrayBuffer = await response.arrayBuffer();
      
      // Decode audio data
      const audioBuffer = await this.audioContext.decodeAudioData(arrayBuffer);
      
      // Cache the buffer
      this.cacheAudioBuffer(url, audioBuffer);
      
      return audioBuffer;
      
    } catch (error) {
      console.error('TootSpeechService: Failed to load audio buffer:', error);
      throw error;
    }
  }

  /**
   * Cache audio buffer with size management
   * @param {string} url - Audio file URL
   * @param {AudioBuffer} buffer - Audio buffer
   */
  cacheAudioBuffer(url, buffer) {
    // Remove oldest entries if cache is full
    if (this.config.bufferCache.size >= this.config.maxCacheSize) {
      const firstKey = this.config.bufferCache.keys().next().value;
      this.config.bufferCache.delete(firstKey);
    }
    
    this.config.bufferCache.set(url, buffer);
  }

  /**
   * Create audio source with pitch and duration control
   * @param {AudioBuffer} buffer - Audio buffer
   * @param {Object} options - Playback options
   * @returns {AudioBufferSourceNode} Audio source node
   */
  createAudioSource(buffer, options = {}) {
    const {
      pitch = 1.0,
      duration = null,
      volume = 1.0,
      startTime = 0,
      loop = false
    } = options;
    
    // Create source node
    const source = this.audioContext.createBufferSource();
    source.buffer = buffer;
    
    // Create gain node for this source
    const sourceGain = this.audioContext.createGain();
    sourceGain.gain.setValueAtTime(volume, this.audioContext.currentTime);
    
    // Calculate playback rate based on pitch and duration
    let playbackRate = pitch;
    
    if (duration !== null) {
      const originalDuration = buffer.duration;
      playbackRate = originalDuration / duration;
    }
    
    // Apply playback rate with pitch correction
    source.playbackRate.setValueAtTime(playbackRate, this.audioContext.currentTime);
    
    // Connect the audio graph
    source.connect(sourceGain);
    sourceGain.connect(this.gainNode);
    
    // Set loop if requested
    source.loop = loop;
    
    return { source, gain: sourceGain };
  }

  /**
   * Play a single audio sound with pitch shifting
   * @param {string} soundType - Type of sound ('trumpet' or 'bellow')
   * @param {Object} options - Playback options
   * @returns {Promise<AudioBufferSourceNode>} Audio source node
   */
  async playSound(soundType, options = {}) {
    try {
      const url = this.audioAssets[soundType];
      if (!url) {
        throw new Error(`Unknown sound type: ${soundType}`);
      }
      
      const buffer = await this.loadAudioBuffer(url);
      const { source, gain } = this.createAudioSource(buffer, options);
      
      // Start playback
      source.start();
      
      // Set up cleanup
      source.onended = () => {
        this.emit('soundEnded', { soundType, options });
      };
      
      return source;
      
    } catch (error) {
      console.error('TootSpeechService: Failed to play sound:', error);
      this.emit('error', { message: 'Failed to play sound', error });
      throw error;
    }
  }

  /**
   * Calculate speech parameters from text
   * @param {string} text - Text to analyze
   * @returns {Object} Speech parameters
   */
  calculateSpeechParameters(text) {
    const words = text.split(' ');
    const wordCount = words.length;
    
    // Calculate duration based on word complexity
    let totalDuration = 0;
    let totalPitch = 0;
    
    words.forEach(word => {
      const wordLength = word.length;
      let wordDuration = 0;
      let wordPitch = 1.0;
      
      // Analyze each character
      for (let i = 0; i < wordLength; i++) {
        const char = word[i].toLowerCase();
        
        if ('0123456789&'.includes(char)) {
          wordDuration += 5;
          wordPitch += 0.1; // Numbers get higher pitch
        } else if ('aeiouyw'.includes(char)) {
          wordDuration += 2;
          wordPitch += 0.05; // Vowels get slightly higher pitch
        } else {
          wordDuration += 1;
        }
      }
      
      totalDuration += wordDuration;
      totalPitch += wordPitch;
    });
    
    // Calculate averages
    const avgDuration = Math.max(0.3, Math.min(3.0, totalDuration / wordCount));
    const avgPitch = Math.max(0.5, Math.min(2.0, totalPitch / wordCount));
    
    // Adjust for punctuation
    const hasExclamation = text.includes('!');
    const hasQuestion = text.includes('?');
    
    let finalPitch = avgPitch;
    let finalDuration = avgDuration;
    
    if (hasExclamation) {
      finalPitch *= 1.2;
      finalDuration *= 0.8;
    } else if (hasQuestion) {
      finalPitch *= 1.1;
      finalDuration *= 1.1;
    }
    
    return {
      pitch: finalPitch,
      duration: finalDuration,
      wordCount,
      hasExclamation,
      hasQuestion
    };
  }

  /**
   * Speak text using Toot speech synthesis
   * @param {string} text - Text to speak
   * @param {Object} options - Speech options
   * @returns {Promise<void>}
   */
  async speak(text, options = {}) {
    if (!text || text.trim().length === 0) {
      return;
    }
    
    try {
      // Stop any current speech
      this.stopSpeaking();
      
      // Calculate speech parameters
      const params = this.calculateSpeechParameters(text);
      
      // Determine sound type based on punctuation
      const soundType = params.hasExclamation || params.hasQuestion ? 'bellow' : 'trumpet';
      
      // Play the speech
      const source = await this.playSound(soundType, {
        pitch: params.pitch,
        duration: params.duration,
        volume: options.volume || 1.0,
        ...options
      });
      
      this.isSpeaking = true;
      this.currentSpeech = { text, source, params };
      
      this.emit('speechStarted', { text, params });
      
      // Set up completion handler
      source.onended = () => {
        this.isSpeaking = false;
        this.currentSpeech = null;
        this.emit('speechEnded', { text, params });
      };
      
    } catch (error) {
      console.error('TootSpeechService: Failed to speak:', error);
      this.emit('error', { message: 'Failed to speak text', error });
    }
  }

  /**
   * Stop current speech
   */
  stopSpeaking() {
    if (this.currentSpeech && this.currentSpeech.source) {
      try {
        this.currentSpeech.source.stop();
      } catch (error) {
        // Source might already be stopped
        console.warn('TootSpeechService: Error stopping speech:', error);
      }
    }
    
    this.isSpeaking = false;
    this.currentSpeech = null;
    this.emit('speechStopped');
  }

  /**
   * Queue speech for sequential playback
   * @param {string} text - Text to speak
   * @param {Object} options - Speech options
   */
  queueSpeech(text, options = {}) {
    this.speechQueue.push({ text, options });
    
    if (!this.isSpeaking) {
      this.processSpeechQueue();
    }
  }

  /**
   * Process the speech queue
   */
  async processSpeechQueue() {
    if (this.speechQueue.length === 0 || this.isSpeaking) {
      return;
    }
    
    const { text, options } = this.speechQueue.shift();
    
    try {
      await this.speak(text, options);
    } catch (error) {
      console.error('TootSpeechService: Failed to process queued speech:', error);
    }
  }

  /**
   * Clear speech queue
   */
  clearSpeechQueue() {
    this.speechQueue = [];
    this.emit('speechQueueCleared');
  }

  /**
   * Get current speech status
   * @returns {Object} Speech status
   */
  getSpeechStatus() {
    return {
      isSpeaking: this.isSpeaking,
      currentSpeech: this.currentSpeech ? {
        text: this.currentSpeech.text,
        params: this.currentSpeech.params
      } : null,
      queueLength: this.speechQueue.length,
      audioContextState: this.audioContext?.state || 'unknown'
    };
  }

  /**
   * Clean up resources
   */
  dispose() {
    this.stopSpeaking();
    this.clearSpeechQueue();
    
    if (this.audioContext) {
      this.audioContext.close();
    }
    
    this.config.bufferCache.clear();
    this.emit('disposed');
  }

  /**
   * Resume audio context (for autoplay policy compliance)
   */
  async resume() {
    if (this.audioContext && this.audioContext.state === 'suspended') {
      await this.audioContext.resume();
      this.emit('resumed');
    }
  }

  /**
   * Suspend audio context (for power saving)
   */
  async suspend() {
    if (this.audioContext && this.audioContext.state === 'running') {
      await this.audioContext.suspend();
      this.emit('suspended');
    }
  }
}

// Create singleton instance
const tootSpeechService = new TootSpeechService();

export default tootSpeechService;
export { TootSpeechService };
