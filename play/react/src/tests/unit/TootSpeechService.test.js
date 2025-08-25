/**
 * TootSpeechService.test.js - Unit tests for Toot Speech Synthesis Service
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the TootSpeechService to ensure
 * pitch shifting and duration control work correctly across modern browsers.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { TootSpeechService } from '../../services/TootSpeechService';

// Mock Web Audio API
class MockAudioContext {
  constructor() {
    this.state = 'running';
    this.currentTime = 0;
    this.destination = {};
  }
  
  createGain() {
    return {
      gain: {
        setValueAtTime: jest.fn(),
        value: 1.0
      },
      connect: jest.fn()
    };
  }
  
  createBufferSource() {
    return {
      buffer: null,
      playbackRate: {
        setValueAtTime: jest.fn(),
        value: 1.0
      },
      connect: jest.fn(),
      start: jest.fn(),
      stop: jest.fn(),
      onended: null,
      loop: false
    };
  }
  
  async decodeAudioData(arrayBuffer) {
    return {
      duration: 2.0,
      numberOfChannels: 2,
      sampleRate: 44100
    };
  }
  
  async resume() {
    this.state = 'running';
  }
  
  async suspend() {
    this.state = 'suspended';
  }
  
  close() {
    this.state = 'closed';
  }
}

// Mock fetch
global.fetch = jest.fn();

// Mock EventEmitter
class MockEventEmitter {
  constructor() {
    this.events = {};
  }
  
  on(event, listener) {
    if (!this.events[event]) {
      this.events[event] = [];
    }
    this.events[event].push(listener);
  }
  
  off(event, listener) {
    if (this.events[event]) {
      this.events[event] = this.events[event].filter(l => l !== listener);
    }
  }
  
  emit(event, ...args) {
    if (this.events[event]) {
      this.events[event].forEach(listener => listener(...args));
    }
  }
}

// Mock the EventEmitter import
jest.mock('events', () => ({
  EventEmitter: MockEventEmitter
}));

describe('TootSpeechService', () => {
  let service;
  let mockAudioContext;
  
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create mock audio context
    mockAudioContext = new MockAudioContext();
    
    // Mock global AudioContext
    global.AudioContext = jest.fn(() => mockAudioContext);
    global.webkitAudioContext = jest.fn(() => mockAudioContext);
    
    // Mock fetch response
    global.fetch.mockResolvedValue({
      ok: true,
      arrayBuffer: jest.fn().mockResolvedValue(new ArrayBuffer(1024))
    });
    
    // Create service instance
    service = new TootSpeechService();
  });
  
  afterEach(() => {
    if (service) {
      service.dispose();
    }
  });
  
  describe('Initialization', () => {
    test('should initialize audio context successfully', async () => {
      await service.initialize();
      
      expect(service.audioContext).toBeDefined();
      expect(service.masterGain).toBeDefined();
      expect(service.gainNode).toBeDefined();
      expect(service.isInitialized).toBe(true);
    });
    
    test('should handle initialization errors gracefully', async () => {
      // Mock AudioContext to throw error
      global.AudioContext = jest.fn(() => {
        throw new Error('AudioContext not supported');
      });
      
      const errorSpy = jest.spyOn(console, 'error').mockImplementation();
      
      await service.initialize();
      
      expect(errorSpy).toHaveBeenCalled();
      expect(service.audioContext).toBeNull();
      
      errorSpy.mockRestore();
    });
    
    test('should resume suspended audio context', async () => {
      mockAudioContext.state = 'suspended';
      
      await service.initialize();
      
      expect(mockAudioContext.resume).toHaveBeenCalled();
    });
  });
  
  describe('Volume Control', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should set master volume correctly', () => {
      service.setMasterVolume(0.5);
      
      expect(service.masterGain.gain.setValueAtTime).toHaveBeenCalledWith(0.5, 0);
    });
    
    test('should clamp volume values', () => {
      service.setMasterVolume(2.0); // Above max
      expect(service.masterGain.gain.setValueAtTime).toHaveBeenCalledWith(1.0, 0);
      
      service.setMasterVolume(-0.5); // Below min
      expect(service.masterGain.gain.setValueAtTime).toHaveBeenCalledWith(0.0, 0);
    });
    
    test('should set speech volume correctly', () => {
      service.setSpeechVolume(0.7);
      
      expect(service.gainNode.gain.setValueAtTime).toHaveBeenCalledWith(0.7, 0);
    });
  });
  
  describe('Audio Buffer Management', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should load and cache audio buffer', async () => {
      const url = 'https://example.com/test.webm';
      const buffer = await service.loadAudioBuffer(url);
      
      expect(buffer).toBeDefined();
      expect(buffer.duration).toBe(2.0);
      expect(service.config.bufferCache.has(url)).toBe(true);
    });
    
    test('should return cached buffer on subsequent loads', async () => {
      const url = 'https://example.com/test.webm';
      
      // First load
      await service.loadAudioBuffer(url);
      
      // Mock fetch to throw error (should not be called)
      global.fetch.mockRejectedValue(new Error('Should not be called'));
      
      // Second load should use cache
      const buffer = await service.loadAudioBuffer(url);
      
      expect(buffer).toBeDefined();
      expect(global.fetch).toHaveBeenCalledTimes(1); // Only called once
    });
    
    test('should handle fetch errors', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      await expect(service.loadAudioBuffer('https://example.com/error.webm'))
        .rejects.toThrow('Network error');
    });
    
    test('should handle decode errors', async () => {
      mockAudioContext.decodeAudioData = jest.fn().mockRejectedValue(new Error('Decode error'));
      
      await expect(service.loadAudioBuffer('https://example.com/test.webm'))
        .rejects.toThrow('Decode error');
    });
    
    test('should manage cache size', async () => {
      // Fill cache beyond limit
      for (let i = 0; i < 60; i++) {
        const url = `https://example.com/test${i}.webm`;
        await service.loadAudioBuffer(url);
      }
      
      expect(service.config.bufferCache.size).toBeLessThanOrEqual(service.config.maxCacheSize);
    });
  });
  
  describe('Audio Source Creation', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should create audio source with default parameters', () => {
      const buffer = { duration: 2.0 };
      const { source, gain } = service.createAudioSource(buffer);
      
      expect(source.buffer).toBe(buffer);
      expect(source.playbackRate.setValueAtTime).toHaveBeenCalledWith(1.0, 0);
      expect(gain.gain.setValueAtTime).toHaveBeenCalledWith(1.0, 0);
    });
    
    test('should create audio source with custom pitch', () => {
      const buffer = { duration: 2.0 };
      const { source } = service.createAudioSource(buffer, { pitch: 1.5 });
      
      expect(source.playbackRate.setValueAtTime).toHaveBeenCalledWith(1.5, 0);
    });
    
    test('should create audio source with custom duration', () => {
      const buffer = { duration: 2.0 };
      const { source } = service.createAudioSource(buffer, { duration: 1.0 });
      
      // playbackRate = originalDuration / targetDuration = 2.0 / 1.0 = 2.0
      expect(source.playbackRate.setValueAtTime).toHaveBeenCalledWith(2.0, 0);
    });
    
    test('should create audio source with custom volume', () => {
      const buffer = { duration: 2.0 };
      const { gain } = service.createAudioSource(buffer, { volume: 0.8 });
      
      expect(gain.gain.setValueAtTime).toHaveBeenCalledWith(0.8, 0);
    });
  });
  
  describe('Speech Parameter Calculation', () => {
    test('should calculate parameters for simple text', () => {
      const params = service.calculateSpeechParameters('Hello world');
      
      expect(params.wordCount).toBe(2);
      expect(params.pitch).toBeGreaterThan(0.5);
      expect(params.pitch).toBeLessThan(2.0);
      expect(params.duration).toBeGreaterThan(0.3);
      expect(params.duration).toBeLessThan(3.0);
      expect(params.hasExclamation).toBe(false);
      expect(params.hasQuestion).toBe(false);
    });
    
    test('should handle exclamation marks', () => {
      const params = service.calculateSpeechParameters('Hello world!');
      
      expect(params.hasExclamation).toBe(true);
      expect(params.hasQuestion).toBe(false);
      expect(params.pitch).toBeGreaterThan(1.0); // Should be increased
    });
    
    test('should handle question marks', () => {
      const params = service.calculateSpeechParameters('How are you?');
      
      expect(params.hasExclamation).toBe(false);
      expect(params.hasQuestion).toBe(true);
      expect(params.pitch).toBeGreaterThan(1.0); // Should be increased
    });
    
    test('should handle numbers and special characters', () => {
      const params = service.calculateSpeechParameters('Count 1, 2, 3 & 4!');
      
      expect(params.wordCount).toBe(6);
      // Numbers and special chars should increase pitch
      expect(params.pitch).toBeGreaterThan(1.0);
    });
    
    test('should handle empty text', () => {
      const params = service.calculateSpeechParameters('');
      
      expect(params.wordCount).toBe(0);
      expect(params.pitch).toBe(1.0);
      expect(params.duration).toBe(0.3); // Minimum duration
    });
  });
  
  describe('Speech Synthesis', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should speak text successfully', async () => {
      const speakSpy = jest.spyOn(service, 'playSound').mockResolvedValue({
        onended: null
      });
      
      await service.speak('Hello world');
      
      expect(speakSpy).toHaveBeenCalledWith('trumpet', expect.objectContaining({
        pitch: expect.any(Number),
        duration: expect.any(Number)
      }));
      expect(service.isSpeaking).toBe(true);
      expect(service.currentSpeech).toBeDefined();
    });
    
    test('should use bellow sound for exclamations', async () => {
      const speakSpy = jest.spyOn(service, 'playSound').mockResolvedValue({
        onended: null
      });
      
      await service.speak('Hello world!');
      
      expect(speakSpy).toHaveBeenCalledWith('bellow', expect.any(Object));
    });
    
    test('should use bellow sound for questions', async () => {
      const speakSpy = jest.spyOn(service, 'playSound').mockResolvedValue({
        onended: null
      });
      
      await service.speak('How are you?');
      
      expect(speakSpy).toHaveBeenCalledWith('bellow', expect.any(Object));
    });
    
    test('should stop current speech before starting new one', async () => {
      const stopSpy = jest.spyOn(service, 'stopSpeaking');
      
      // Start first speech
      jest.spyOn(service, 'playSound').mockResolvedValue({
        onended: null
      });
      
      await service.speak('First speech');
      
      // Start second speech
      await service.speak('Second speech');
      
      expect(stopSpy).toHaveBeenCalled();
    });
    
    test('should handle empty text', async () => {
      const speakSpy = jest.spyOn(service, 'playSound');
      
      await service.speak('');
      
      expect(speakSpy).not.toHaveBeenCalled();
    });
    
    test('should handle speech errors', async () => {
      const errorSpy = jest.spyOn(console, 'error').mockImplementation();
      jest.spyOn(service, 'playSound').mockRejectedValue(new Error('Playback error'));
      
      await service.speak('Test text');
      
      expect(errorSpy).toHaveBeenCalled();
      expect(service.isSpeaking).toBe(false);
      
      errorSpy.mockRestore();
    });
  });
  
  describe('Speech Queue Management', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should queue speech when not speaking', () => {
      service.queueSpeech('Queued text');
      
      expect(service.speechQueue).toHaveLength(1);
      expect(service.speechQueue[0].text).toBe('Queued text');
    });
    
    test('should process queue when speech ends', async () => {
      const speakSpy = jest.spyOn(service, 'speak').mockResolvedValue();
      
      // Queue speech
      service.queueSpeech('Queued text');
      
      // Process queue
      await service.processSpeechQueue();
      
      expect(speakSpy).toHaveBeenCalledWith('Queued text', {});
      expect(service.speechQueue).toHaveLength(0);
    });
    
    test('should not process queue when already speaking', async () => {
      service.isSpeaking = true;
      const speakSpy = jest.spyOn(service, 'speak').mockResolvedValue();
      
      service.queueSpeech('Queued text');
      await service.processSpeechQueue();
      
      expect(speakSpy).not.toHaveBeenCalled();
    });
    
    test('should clear speech queue', () => {
      service.queueSpeech('Text 1');
      service.queueSpeech('Text 2');
      
      expect(service.speechQueue).toHaveLength(2);
      
      service.clearSpeechQueue();
      
      expect(service.speechQueue).toHaveLength(0);
    });
  });
  
  describe('Speech Control', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should stop current speech', () => {
      service.isSpeaking = true;
      service.currentSpeech = {
        source: {
          stop: jest.fn()
        }
      };
      
      service.stopSpeaking();
      
      expect(service.currentSpeech.source.stop).toHaveBeenCalled();
      expect(service.isSpeaking).toBe(false);
      expect(service.currentSpeech).toBeNull();
    });
    
    test('should handle stop when no speech is playing', () => {
      const consoleSpy = jest.spyOn(console, 'warn').mockImplementation();
      
      service.stopSpeaking();
      
      expect(consoleSpy).toHaveBeenCalled();
      
      consoleSpy.mockRestore();
    });
  });
  
  describe('Audio Context Management', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should resume suspended audio context', async () => {
      mockAudioContext.state = 'suspended';
      
      await service.resume();
      
      expect(mockAudioContext.resume).toHaveBeenCalled();
    });
    
    test('should suspend running audio context', async () => {
      mockAudioContext.state = 'running';
      
      await service.suspend();
      
      expect(mockAudioContext.suspend).toHaveBeenCalled();
    });
  });
  
  describe('Status and Information', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should return speech status', () => {
      service.isSpeaking = true;
      service.currentSpeech = { text: 'Test', params: { pitch: 1.0 } };
      service.speechQueue = [{ text: 'Queued' }];
      
      const status = service.getSpeechStatus();
      
      expect(status.isSpeaking).toBe(true);
      expect(status.currentSpeech.text).toBe('Test');
      expect(status.queueLength).toBe(1);
      expect(status.audioContextState).toBe('running');
    });
  });
  
  describe('Resource Cleanup', () => {
    beforeEach(async () => {
      await service.initialize();
    });
    
    test('should dispose resources correctly', () => {
      service.isSpeaking = true;
      service.currentSpeech = { source: { stop: jest.fn() } };
      service.speechQueue = [{ text: 'Queued' }];
      
      service.dispose();
      
      expect(service.currentSpeech.source.stop).toHaveBeenCalled();
      expect(service.isSpeaking).toBe(false);
      expect(service.speechQueue).toHaveLength(0);
      expect(service.config.bufferCache.size).toBe(0);
      expect(mockAudioContext.close).toHaveBeenCalled();
    });
  });
  
  describe('Cross-browser Compatibility', () => {
    test('should use webkitAudioContext as fallback', async () => {
      // Mock AudioContext to not exist
      global.AudioContext = undefined;
      
      await service.initialize();
      
      expect(global.webkitAudioContext).toHaveBeenCalled();
    });
    
    test('should handle missing Web Audio API', async () => {
      // Mock both AudioContext and webkitAudioContext to not exist
      global.AudioContext = undefined;
      global.webkitAudioContext = undefined;
      
      const errorSpy = jest.spyOn(console, 'error').mockImplementation();
      
      await service.initialize();
      
      expect(errorSpy).toHaveBeenCalled();
      expect(service.audioContext).toBeNull();
      
      errorSpy.mockRestore();
    });
  });
});
