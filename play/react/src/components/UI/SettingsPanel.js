/**
 * SettingsPanel.js - Game settings and configuration panel
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides settings management for graphics, audio, controls,
 * and other game configuration options.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState } from 'react';
import { useGameStore } from '../../stores/GameStore';
import './SettingsPanel.css';

/**
 * SettingsPanel component for game configuration
 * 
 * @component
 * @returns {JSX.Element} Settings management interface
 */
const SettingsPanel = () => {
  const { settings, updateSettings } = useGameStore();
  const [isOpen, setIsOpen] = useState(false);
  const [activeTab, setActiveTab] = useState('graphics');
  
  const togglePanel = () => {
    setIsOpen(!isOpen);
  };
  
  const handleSettingChange = (category, key, value) => {
    updateSettings(category, key, value);
  };
  
  const handleResetSettings = () => {
    if (window.confirm('Are you sure you want to reset all settings to default?')) {
      // Reset to default settings
      updateSettings('reset', 'all', true);
    }
  };
  
  const tabs = [
    { id: 'graphics', label: 'Graphics', icon: '🎨' },
    { id: 'audio', label: 'Audio', icon: '🔊' },
    { id: 'controls', label: 'Controls', icon: '🎮' },
    { id: 'interface', label: 'Interface', icon: '⚙️' },
    { id: 'accessibility', label: 'Accessibility', icon: '♿' }
  ];
  
  const renderGraphicsSettings = () => (
    <div className="settings-section">
      <h4>Graphics Quality</h4>
      <div className="setting-item">
        <label>Quality Preset</label>
        <select 
          value={settings?.graphics?.quality || 'medium'}
          onChange={(e) => handleSettingChange('graphics', 'quality', e.target.value)}
        >
          <option value="low">Low</option>
          <option value="medium">Medium</option>
          <option value="high">High</option>
          <option value="ultra">Ultra</option>
        </select>
      </div>
      
      <div className="setting-item">
        <label>Performance Monitor</label>
        <div className="setting-description">
          Shows a turtle icon when frame rate drops below 25 FPS
        </div>
      </div>
      
      <div className="setting-item">
        <label>
          <input 
            type="checkbox"
            checked={settings?.graphics?.vsync || false}
            onChange={(e) => handleSettingChange('graphics', 'vsync', e.target.checked)}
          />
          V-Sync
        </label>
      </div>
      
      <div className="setting-item">
        <label>
          <input 
            type="checkbox"
            checked={settings?.graphics?.antialiasing || false}
            onChange={(e) => handleSettingChange('graphics', 'antialiasing', e.target.checked)}
          />
          Anti-aliasing
        </label>
      </div>
    </div>
  );
  
  const renderAudioSettings = () => (
    <div className="settings-section">
      <h4>Audio Settings</h4>
      <div className="setting-item">
        <label>Master Volume</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.masterVolume || 50}
          onChange={(e) => handleSettingChange('audio', 'masterVolume', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.masterVolume || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>Music Volume</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.musicVolume || 50}
          onChange={(e) => handleSettingChange('audio', 'musicVolume', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.musicVolume || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>SFX Volume</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.sfxVolume || 50}
          onChange={(e) => handleSettingChange('audio', 'sfxVolume', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.sfxVolume || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>Speech Volume</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.speechVolume || 50}
          onChange={(e) => handleSettingChange('audio', 'speechVolume', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.speechVolume || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>Voice Chat (Incoming)</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.voiceChatIncoming || 50}
          onChange={(e) => handleSettingChange('audio', 'voiceChatIncoming', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.voiceChatIncoming || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>Voice Chat (Outgoing)</label>
        <input 
          type="range"
          min="0"
          max="100"
          value={settings?.audio?.voiceChatOutgoing || 50}
          onChange={(e) => handleSettingChange('audio', 'voiceChatOutgoing', parseInt(e.target.value))}
        />
        <span className="setting-value">{settings?.audio?.voiceChatOutgoing || 50}%</span>
      </div>
      
      <div className="setting-item">
        <label>
          <input 
            type="checkbox"
            checked={settings?.audio?.mute || false}
            onChange={(e) => handleSettingChange('audio', 'mute', e.target.checked)}
          />
          Mute All Audio
        </label>
      </div>
    </div>
  );
  
  const renderControlsSettings = () => (
    <div className="settings-section">
      <h4>Control Settings</h4>
      <div className="setting-item">
        <div className="setting-description">
          Mouse and keyboard controls are handled by your operating system.
        </div>
      </div>
      
      <div className="setting-item">
        <label>Gamepad Enabled</label>
        <input 
          type="checkbox"
          checked={settings?.controls?.gamepadEnabled || false}
          onChange={(e) => handleSettingChange('controls', 'gamepadEnabled', e.target.checked)}
        />
      </div>
      
      <div className="setting-item">
        <label>Touch Controls</label>
        <input 
          type="checkbox"
          checked={settings?.controls?.touchControls || false}
          onChange={(e) => handleSettingChange('controls', 'touchControls', e.target.checked)}
        />
      </div>
    </div>
  );
  
  const renderInterfaceSettings = () => (
    <div className="settings-section">
      <h4>Interface Settings</h4>
      <div className="setting-item">
        <label>UI Scale</label>
        <select 
          value={settings?.interface?.uiScale || 'normal'}
          onChange={(e) => handleSettingChange('interface', 'uiScale', e.target.value)}
        >
          <option value="small">Small (75% - More screen space for game world)</option>
          <option value="normal">Normal (100% - Standard interface size)</option>
          <option value="large">Large (125% - Easier to read on high-DPI displays)</option>
        </select>
        <div className="setting-description">
          Adjusts the size of interface elements. Small scale provides more screen space for the game world, 
          while large scale improves readability on high-resolution displays. Normal scale is optimized for 
          baseline machines (4-year-old Chromebook or similar PC).
        </div>
      </div>
      
      <div className="setting-item">
        <div className="setting-description">
          Performance monitoring shows a turtle icon when frame rate drops below 25 FPS.
          Chat interface is always fully opaque for optimal readability.
        </div>
      </div>
    </div>
  );
  
  const renderAccessibilitySettings = () => (
    <div className="settings-section">
      <h4>Accessibility</h4>
      <div className="setting-item">
        <label>High Contrast Mode</label>
        <input 
          type="checkbox"
          checked={settings?.accessibility?.highContrast || false}
          onChange={(e) => handleSettingChange('accessibility', 'highContrast', e.target.checked)}
        />
      </div>
      
      <div className="setting-item">
        <label>Large Text</label>
        <input 
          type="checkbox"
          checked={settings?.accessibility?.largeText || false}
          onChange={(e) => handleSettingChange('accessibility', 'largeText', e.target.checked)}
        />
      </div>
      
      <div className="setting-item">
        <label>Screen Reader Support</label>
        <input 
          type="checkbox"
          checked={settings?.accessibility?.screenReader || false}
          onChange={(e) => handleSettingChange('accessibility', 'screenReader', e.target.checked)}
        />
      </div>
      
      <div className="setting-item">
        <label>Reduce Motion</label>
        <input 
          type="checkbox"
          checked={settings?.accessibility?.reduceMotion || false}
          onChange={(e) => handleSettingChange('accessibility', 'reduceMotion', e.target.checked)}
        />
      </div>
      
      <div className="setting-item">
        <label>Color Blind Support</label>
        <select 
          value={settings?.accessibility?.colorBlindMode || 'none'}
          onChange={(e) => handleSettingChange('accessibility', 'colorBlindMode', e.target.value)}
        >
          <option value="none">None</option>
          <option value="protanopia">Protanopia</option>
          <option value="deuteranopia">Deuteranopia</option>
          <option value="tritanopia">Tritanopia</option>
        </select>
      </div>
    </div>
  );
  
  const renderTabContent = () => {
    switch (activeTab) {
      case 'graphics':
        return renderGraphicsSettings();
      case 'audio':
        return renderAudioSettings();
      case 'controls':
        return renderControlsSettings();
      case 'interface':
        return renderInterfaceSettings();
      case 'accessibility':
        return renderAccessibilitySettings();
      default:
        return renderGraphicsSettings();
    }
  };
  
  return (
    <div className="settings-panel">
      {/* Settings Toggle Button */}
      <button 
        className="settings-toggle"
        onClick={togglePanel}
        title={isOpen ? 'Close Settings' : 'Open Settings'}
      >
        ⚙️
      </button>
      
      {/* Settings Window */}
      {isOpen && (
        <div className="settings-window">
          <div className="settings-header">
            <h3>Settings</h3>
            <div className="settings-controls">
              <button 
                className="settings-reset"
                onClick={handleResetSettings}
                title="Reset to Default"
              >
                🔄
              </button>
              <button 
                className="settings-close"
                onClick={togglePanel}
                title="Close Settings"
              >
                ×
              </button>
            </div>
          </div>
          
          <div className="settings-content">
            <div className="settings-tabs">
              {tabs.map(tab => (
                <button
                  key={tab.id}
                  className={`settings-tab ${activeTab === tab.id ? 'active' : ''}`}
                  onClick={() => setActiveTab(tab.id)}
                >
                  <span className="tab-icon">{tab.icon}</span>
                  <span className="tab-label">{tab.label}</span>
                </button>
              ))}
            </div>
            
            <div className="settings-body">
              {renderTabContent()}
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default SettingsPanel;



