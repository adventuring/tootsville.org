/**
 * @fileoverview Main game HUD (Heads-Up Display) for Tootsville V
 * @module GameHUD
 * @author Interworldly Adventuring, LLC
 * @copyright © 2008-2017 BRPocock, © 2018-2024 CIWTA, © 2024-2025 IAL
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useState, useEffect } from 'react';
import useGameStore from '../../stores/GameStore';
import PlayerStats from './PlayerStats';
import ChatSystem from './ChatSystem';
import InventoryPanel from './InventoryPanel';
import MapSystem from './MapSystem';
import SettingsPanel from './SettingsPanel';

/**
 * Main game HUD component that overlays the 3D world
 * 
 * @function GameHUD
 * @description Provides the primary user interface for game interaction
 * @param {Object} props - Component properties
 * @param {boolean} props.showDebug - Whether to show debug information
 * @returns {JSX.Element} The complete HUD interface
 * 
 * @example
 * <GameHUD showDebug={false} />
 * 
 * @see {@link PlayerStats} for character statistics display
 * @see {@link ChatSystem} for communication interface
 * @see {@link InventoryPanel} for item management
 * @see {@link MapSystem} for world navigation
 */
const GameHUD = ({ showDebug = false }) => {
  const { character, activity, uiState } = useGameStore(state => ({
    character: state.character,
    activity: state.activity,
    uiState: state.uiState
  }));

  const [activePanel, setActivePanel] = useState('none');

  /**
   * Handle keyboard shortcuts for UI navigation
   * @function handleKeyPress
   * @private
   * @param {KeyboardEvent} event - Keyboard event
   */
  const handleKeyPress = (event) => {
    switch (event.key) {
      case 'i':
        setActivePanel(activePanel === 'inventory' ? 'none' : 'inventory');
        break;
      case 'm':
        setActivePanel(activePanel === 'map' ? 'none' : 'map');
        break;
      case 'c':
        setActivePanel(activePanel === 'chat' ? 'none' : 'chat');
        break;
      case 'Escape':
        setActivePanel('none');
        break;
      default:
        break;
    }
  };

  useEffect(() => {
    document.addEventListener('keydown', handleKeyPress);
    return () => document.removeEventListener('keydown', handleKeyPress);
  }, [activePanel]);

  /**
   * Handle panel close
   * @function handlePanelClose
   * @private
   * @param {string} panelName - Name of panel to close
   */
  const handlePanelClose = (panelName) => {
    setActivePanel(activePanel === panelName ? 'none' : activePanel);
  };

  return (
    <div className="game-hud">
      {/* Top status bar */}
      <div className="hud-top-bar">
        <PlayerStats character={character} />
        
        <div className="hud-controls">
          <button
            className={`hud-button ${activePanel === 'inventory' ? 'active' : ''}`}
            onClick={() => setActivePanel(activePanel === 'inventory' ? 'none' : 'inventory')}
            title="Inventory (I)"
          >
            <i className="icon-inventory"></i>
          </button>
          
          <button
            className={`hud-button ${activePanel === 'map' ? 'active' : ''}`}
            onClick={() => setActivePanel(activePanel === 'map' ? 'none' : 'map')}
            title="Map (M)"
          >
            <i className="icon-map"></i>
          </button>
          
          <button
            className={`hud-button ${activePanel === 'chat' ? 'active' : ''}`}
            onClick={() => setActivePanel(activePanel === 'chat' ? 'none' : 'chat')}
            title="Chat (C)"
          >
            <i className="icon-chat"></i>
          </button>
          
          <button
            className={`hud-button ${activePanel === 'settings' ? 'active' : ''}`}
            onClick={() => setActivePanel(activePanel === 'settings' ? 'none' : 'settings')}
            title="Settings"
          >
            <i className="icon-settings"></i>
          </button>
        </div>
      </div>

      {/* Bottom action bar */}
      <div className="hud-bottom-bar">
        <ActionBar />
      </div>

      {/* Chat system */}
      <div className={`chat-container ${activePanel === 'chat' ? 'open' : ''}`}>
        <ChatSystem onClose={() => handlePanelClose('chat')} />
      </div>

      {/* Inventory panel */}
      {activePanel === 'inventory' && (
        <InventoryPanel onClose={() => handlePanelClose('inventory')} />
      )}

      {/* Map system */}
      {activePanel === 'map' && (
        <MapSystem onClose={() => handlePanelClose('map')} />
      )}

      {/* Settings panel */}
      {activePanel === 'settings' && (
        <SettingsPanel onClose={() => handlePanelClose('settings')} />
      )}

      {/* Debug information */}
      {showDebug && <DebugPanel />}

      {/* Notifications */}
      <NotificationSystem />
    </div>
  );
};

/**
 * Action bar for quick access to common actions
 * 
 * @function ActionBar
 * @description Provides quick access buttons for common game actions
 * @returns {JSX.Element} Action bar component
 * 
 * @example
 * <ActionBar />
 */
const ActionBar = () => {
  const { character } = useGameStore(state => ({ character: state.character }));

  /**
   * Handle action button click
   * @function handleAction
   * @private
   * @param {string} action - Action to perform
   */
  const handleAction = (action) => {
    switch (action) {
      case 'jump':
        useGameStore.getState().performAction('jump');
        break;
      case 'sit':
        useGameStore.getState().performAction('sit');
        break;
      case 'wave':
        useGameStore.getState().performAction('wave');
        break;
      case 'dance':
        useGameStore.getState().performAction('dance');
        break;
      default:
        console.log('Unknown action:', action);
    }
  };

  return (
    <div className="action-bar">
      <button
        className="action-button"
        onClick={() => handleAction('jump')}
        title="Jump"
      >
        <i className="icon-jump"></i>
      </button>
      
      <button
        className="action-button"
        onClick={() => handleAction('sit')}
        title="Sit"
      >
        <i className="icon-sit"></i>
      </button>
      
      <button
        className="action-button"
        onClick={() => handleAction('wave')}
        title="Wave"
      >
        <i className="icon-wave"></i>
      </button>
      
      <button
        className="action-button"
        onClick={() => handleAction('dance')}
        title="Dance"
      >
        <i className="icon-dance"></i>
      </button>
    </div>
  );
};

/**
 * Debug panel for development and testing
 * 
 * @function DebugPanel
 * @description Displays debug information for development purposes
 * @returns {JSX.Element} Debug information panel
 * 
 * @example
 * <DebugPanel />
 */
const DebugPanel = () => {
  const { activity, character, avatars, items } = useGameStore(state => ({
    activity: state.activity,
    character: state.character,
    avatars: state.avatars,
    items: state.items
  }));

  return (
    <div className="debug-panel">
      <h3>Debug Information</h3>
      <div className="debug-info">
        <div>Position: ({activity.lat.toFixed(2)}, {activity.alt.toFixed(2)}, {activity.long.toFixed(2)})</div>
        <div>World: {activity.world}</div>
        <div>Avatars Nearby: {avatars.length}</div>
        <div>Items Nearby: {items.length}</div>
        <div>FPS: {useGameStore.getState().performance.fps}</div>
        <div>Memory: {useGameStore.getState().performance.memory}MB</div>
      </div>
    </div>
  );
};

/**
 * Notification system for game events
 * 
 * @function NotificationSystem
 * @description Manages and displays game notifications and alerts
 * @returns {JSX.Element} Notification container
 * 
 * @example
 * <NotificationSystem />
 */
const NotificationSystem = () => {
  const { notifications } = useGameStore(state => ({ notifications: state.notifications }));

  /**
   * Handle notification dismissal
   * @function handleDismiss
   * @private
   * @param {string} id - Notification ID
   */
  const handleDismiss = (id) => {
    useGameStore.getState().dismissNotification(id);
  };

  return (
    <div className="notification-container">
      {notifications.map(notification => (
        <div
          key={notification.id}
          className={`notification ${notification.type}`}
        >
          <div className="notification-content">
            <span className="notification-title">{notification.title}</span>
            <span className="notification-message">{notification.message}</span>
          </div>
          <button
            className="notification-dismiss"
            onClick={() => handleDismiss(notification.id)}
          >
            ×
          </button>
        </div>
      ))}
    </div>
  );
};

export default GameHUD;
