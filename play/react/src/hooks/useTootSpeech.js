/**
 * useTootSpeech.js - React Hook for Toot Speech Synthesis
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides React components with easy access to Toot speech synthesis
 * with pitch and duration shifting capabilities.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import tootSpeechService from '../services/TootSpeechService';

/**
 * React hook for Toot speech synthesis
 * 
 * @param {Object} options - Hook options
 * @param {boolean} options.autoInitialize - Whether to auto-initialize the service
 * @param {number} options.defaultVolume - Default speech volume (0-1)
 * @returns {Object} Speech synthesis methods and state
 * 
 * @example
 * const { speak, stopSpeaking, isSpeaking, speechStatus } = useTootSpeech();
 * 
 * // Speak text
 * speak("Hello, world!");
 * 
 * // Stop current speech
 * stopSpeaking();
 */
export const useTootSpeech = (options = {}) => {
  const {
    autoInitialize = true,
    defaultVolume = 0.8
  } = options;
  
  // State
  const [isSpeaking, setIsSpeaking] = useState(false);
  const [speechStatus, setSpeechStatus] = useState(null);
  const [isInitialized, setIsInitialized] = useState(false);
  const [error, setError] = useState(null);
  
  // Refs
  const serviceRef = useRef(tootSpeechService);
  
  // Initialize service
  useEffect(() => {
    if (!autoInitialize) return;
    
    const service = serviceRef.current;
    
    const handleInitialized = () => {
      setIsInitialized(true);
      setError(null);
      
      // Set default volume
      if (defaultVolume !== undefined) {
        service.setSpeechVolume(defaultVolume);
      }
    };
    
    const handleError = (error) => {
      setError(error);
      console.error('useTootSpeech: Service error:', error);
    };
    
    const handleSpeechStarted = () => {
      setIsSpeaking(true);
      setSpeechStatus(service.getSpeechStatus());
    };
    
    const handleSpeechEnded = () => {
      setIsSpeaking(false);
      setSpeechStatus(service.getSpeechStatus());
    };
    
    const handleSpeechStopped = () => {
      setIsSpeaking(false);
      setSpeechStatus(service.getSpeechStatus());
    };
    
    // Set up event listeners
    service.on('initialized', handleInitialized);
    service.on('error', handleError);
    service.on('speechStarted', handleSpeechStarted);
    service.on('speechEnded', handleSpeechEnded);
    service.on('speechStopped', handleSpeechStopped);
    
    // Initialize if not already done
    if (!service.audioContext) {
      service.initialize();
    } else {
      setIsInitialized(true);
    }
    
    // Cleanup
    return () => {
      service.off('initialized', handleInitialized);
      service.off('error', handleError);
      service.off('speechStarted', handleSpeechStarted);
      service.off('speechEnded', handleSpeechEnded);
      service.off('speechStopped', handleSpeechStopped);
    };
  }, [autoInitialize, defaultVolume]);
  
  // Update speech status periodically
  useEffect(() => {
    if (!isInitialized) return;
    
    const updateStatus = () => {
      setSpeechStatus(serviceRef.current.getSpeechStatus());
    };
    
    const interval = setInterval(updateStatus, 100);
    
    return () => clearInterval(interval);
  }, [isInitialized]);
  
  // Speech methods
  const speak = useCallback(async (text, options = {}) => {
    try {
      setError(null);
      await serviceRef.current.speak(text, options);
    } catch (err) {
      setError(err);
      throw err;
    }
  }, []);
  
  const stopSpeaking = useCallback(() => {
    serviceRef.current.stopSpeaking();
  }, []);
  
  const queueSpeech = useCallback((text, options = {}) => {
    serviceRef.current.queueSpeech(text, options);
  }, []);
  
  const clearSpeechQueue = useCallback(() => {
    serviceRef.current.clearSpeechQueue();
  }, []);
  
  const setVolume = useCallback((volume) => {
    serviceRef.current.setSpeechVolume(volume);
  }, []);
  
  const setMasterVolume = useCallback((volume) => {
    serviceRef.current.setMasterVolume(volume);
  }, []);
  
  const resume = useCallback(async () => {
    await serviceRef.current.resume();
  }, []);
  
  const suspend = useCallback(async () => {
    await serviceRef.current.suspend();
  }, []);
  
  return {
    // State
    isSpeaking,
    speechStatus,
    isInitialized,
    error,
    
    // Methods
    speak,
    stopSpeaking,
    queueSpeech,
    clearSpeechQueue,
    setVolume,
    setMasterVolume,
    resume,
    suspend,
    
    // Service reference (for advanced usage)
    service: serviceRef.current
  };
};

/**
 * Hook for speech with automatic cleanup
 * 
 * @param {Object} options - Hook options
 * @returns {Object} Speech synthesis methods and state
 */
export const useTootSpeechWithCleanup = (options = {}) => {
  const speech = useTootSpeech(options);
  
  // Auto-cleanup on unmount
  useEffect(() => {
    return () => {
      speech.stopSpeaking();
      speech.clearSpeechQueue();
    };
  }, [speech]);
  
  return speech;
};

/**
 * Hook for speech with volume control
 * 
 * @param {Object} options - Hook options
 * @returns {Object} Speech synthesis methods and state with volume control
 */
export const useTootSpeechWithVolume = (options = {}) => {
  const [volume, setVolumeState] = useState(options.defaultVolume || 0.8);
  const speech = useTootSpeech(options);
  
  const setVolume = useCallback((newVolume) => {
    setVolumeState(newVolume);
    speech.setVolume(newVolume);
  }, [speech]);
  
  return {
    ...speech,
    volume,
    setVolume
  };
};

export default useTootSpeech;
