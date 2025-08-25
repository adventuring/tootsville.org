/**
 * TootSpeechPanel.js - Toot Speech Synthesis Control Panel
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides a UI for testing and controlling Toot speech synthesis
 * with pitch and duration shifting capabilities.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useEffect } from 'react';
import { useTootSpeechWithVolume } from '../../hooks/useTootSpeech';
import './TootSpeechPanel.css';

/**
 * TootSpeechPanel component for speech synthesis control
 * 
 * @component
 * @returns {JSX.Element} Speech control panel
 */
const TootSpeechPanel = () => {
  const [inputText, setInputText] = useState('');
  const [customPitch, setCustomPitch] = useState(1.0);
  const [customDuration, setCustomDuration] = useState(1.0);
  const [useCustomParams, setUseCustomParams] = useState(false);
  const [speechHistory, setSpeechHistory] = useState([]);
  
  const {
    speak,
    stopSpeaking,
    queueSpeech,
    clearSpeechQueue,
    isSpeaking,
    speechStatus,
    isInitialized,
    error,
    volume,
    setVolume
  } = useTootSpeechWithVolume({ defaultVolume: 0.8 });

  // Add speech to history
  const addToHistory = (text, params) => {
    const entry = {
      id: Date.now(),
      text,
      params,
      timestamp: new Date().toLocaleTimeString()
    };
    
    setSpeechHistory(prev => [entry, ...prev.slice(0, 9)]); // Keep last 10 entries
  };

  // Handle speech start
  useEffect(() => {
    if (speechStatus?.currentSpeech) {
      addToHistory(speechStatus.currentSpeech.text, speechStatus.currentSpeech.params);
    }
  }, [speechStatus?.currentSpeech]);

  // Handle speak button click
  const handleSpeak = async () => {
    if (!inputText.trim()) return;
    
    const options = useCustomParams ? {
      pitch: customPitch,
      duration: customDuration
    } : {};
    
    try {
      await speak(inputText, options);
    } catch (err) {
      console.error('Failed to speak:', err);
    }
  };

  // Handle queue button click
  const handleQueue = () => {
    if (!inputText.trim()) return;
    
    const options = useCustomParams ? {
      pitch: customPitch,
      duration: customDuration
    } : {};
    
    queueSpeech(inputText, options);
  };

  // Handle preset phrases
  const handlePreset = (phrase) => {
    setInputText(phrase);
  };

  // Handle volume change
  const handleVolumeChange = (e) => {
    const newVolume = parseFloat(e.target.value);
    setVolume(newVolume);
  };

  if (!isInitialized) {
    return (
      <div className="toot-speech-panel">
        <div className="speech-panel-header">
          <h3>Toot Speech Synthesis</h3>
        </div>
        <div className="speech-panel-content">
          <p>Initializing audio system...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="toot-speech-panel">
      <div className="speech-panel-header">
        <h3>Toot Speech Synthesis</h3>
        <div className="speech-status">
          {isSpeaking ? (
            <span className="status-speaking">Speaking...</span>
          ) : (
            <span className="status-idle">Idle</span>
          )}
        </div>
      </div>

      <div className="speech-panel-content">
        {/* Error display */}
        {error && (
          <div className="speech-error">
            <strong>Error:</strong> {error.message}
          </div>
        )}

        {/* Volume control */}
        <div className="volume-control">
          <label htmlFor="volume-slider">Volume: {Math.round(volume * 100)}%</label>
          <input
            id="volume-slider"
            type="range"
            min="0"
            max="1"
            step="0.01"
            value={volume}
            onChange={handleVolumeChange}
          />
        </div>

        {/* Text input */}
        <div className="text-input-section">
          <label htmlFor="speech-text">Text to speak:</label>
          <textarea
            id="speech-text"
            value={inputText}
            onChange={(e) => setInputText(e.target.value)}
            placeholder="Enter text for Toot speech synthesis..."
            rows={3}
          />
        </div>

        {/* Custom parameters */}
        <div className="custom-params">
          <label>
            <input
              type="checkbox"
              checked={useCustomParams}
              onChange={(e) => setUseCustomParams(e.target.checked)}
            />
            Use custom parameters
          </label>
          
          {useCustomParams && (
            <div className="param-controls">
              <div className="param-control">
                <label htmlFor="pitch-slider">
                  Pitch: {customPitch.toFixed(2)}
                </label>
                <input
                  id="pitch-slider"
                  type="range"
                  min="0.5"
                  max="2.0"
                  step="0.01"
                  value={customPitch}
                  onChange={(e) => setCustomPitch(parseFloat(e.target.value))}
                />
              </div>
              
              <div className="param-control">
                <label htmlFor="duration-slider">
                  Duration: {customDuration.toFixed(2)}s
                </label>
                <input
                  id="duration-slider"
                  type="range"
                  min="0.3"
                  max="3.0"
                  step="0.1"
                  value={customDuration}
                  onChange={(e) => setCustomDuration(parseFloat(e.target.value))}
                />
              </div>
            </div>
          )}
        </div>

        {/* Control buttons */}
        <div className="control-buttons">
          <button
            onClick={handleSpeak}
            disabled={!inputText.trim() || isSpeaking}
            className="btn-speak"
          >
            {isSpeaking ? 'Speaking...' : 'Speak'}
          </button>
          
          <button
            onClick={handleQueue}
            disabled={!inputText.trim()}
            className="btn-queue"
          >
            Queue
          </button>
          
          <button
            onClick={stopSpeaking}
            disabled={!isSpeaking}
            className="btn-stop"
          >
            Stop
          </button>
          
          <button
            onClick={clearSpeechQueue}
            disabled={!speechStatus?.queueLength}
            className="btn-clear"
          >
            Clear Queue ({speechStatus?.queueLength || 0})
          </button>
        </div>

        {/* Preset phrases */}
        <div className="preset-phrases">
          <h4>Preset Phrases</h4>
          <div className="preset-buttons">
            <button onClick={() => handlePreset('Hello, world!')}>
              Hello, world!
            </button>
            <button onClick={() => handlePreset('How are you today?')}>
              How are you today?
            </button>
            <button onClick={() => handlePreset('Welcome to Tootsville!')}>
              Welcome to Tootsville!
            </button>
            <button onClick={() => handlePreset('This is amazing!')}>
              This is amazing!
            </button>
            <button onClick={() => handlePreset('Numbers: 1, 2, 3, 4, 5')}>
              Numbers: 1, 2, 3, 4, 5
            </button>
            <button onClick={() => handlePreset('Special chars: & @ # $ %')}>
              Special chars: & @ # $ %
            </button>
          </div>
        </div>

        {/* Speech history */}
        {speechHistory.length > 0 && (
          <div className="speech-history">
            <h4>Recent Speech</h4>
            <div className="history-list">
              {speechHistory.map(entry => (
                <div key={entry.id} className="history-entry">
                  <div className="history-time">{entry.timestamp}</div>
                  <div className="history-text">{entry.text}</div>
                  {entry.params && (
                    <div className="history-params">
                      Pitch: {entry.params.pitch?.toFixed(2)}, 
                      Duration: {entry.params.duration?.toFixed(2)}s
                    </div>
                  )}
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Status information */}
        <div className="status-info">
          <h4>Status Information</h4>
          <div className="status-details">
            <div>Audio Context: {speechStatus?.audioContextState || 'unknown'}</div>
            <div>Queue Length: {speechStatus?.queueLength || 0}</div>
            <div>Current Speech: {speechStatus?.currentSpeech?.text || 'None'}</div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default TootSpeechPanel;
