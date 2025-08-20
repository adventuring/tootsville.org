/**
 * App.js - Main React application for Tootsville client
 * Integrates all components and provides the primary application interface
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useEffect, useState } from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls, Stats } from '@react-three/drei';
import clientBindings from './bindings/ClientBindings';
import { GameWorld } from './components/GameWorld';
import { GameHUD } from './components/UI/GameHUD';
import useGameStore from './stores/GameStore';
import { getGameText, getErrorText, getUIText } from './constants/UIText';
import './App.css';

/**
 * @component App
 * @description Main application component for Tootsville client
 * 
 * This component serves as the root of the React application, integrating
 * all game components, UI elements, and bindings. It provides the primary
 * interface for user interaction and manages the overall application state.
 * 
 * @example
 * // Basic usage
 * <App />
 * 
 * @example
 * // With custom configuration
 * <App 
 *   initialCluster="test"
 *   enableDebug={true}
 *   customBindings={myBindings}
 * />
 */
function App({ 
  initialCluster = 'test', 
  enableDebug = false,
  customBindings = null 
}) {
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);
  const { connection, actions } = useGameStore();

  /**
   * @method initializeApplication
   * @private
   * @description Initialize the application and establish server connection
   */
  const initializeApplication = async () => {
    try {
      setIsLoading(true);
      setError(null);

      // Initialize bindings
      const bindings = customBindings || clientBindings;
      
      // Set up event handlers
      bindings.communication.on('connected', () => {
        actions.setConnectionStatus(true);
        setIsLoading(false);
      });

      bindings.communication.on('disconnected', () => {
        actions.setConnectionStatus(false);
      });

      bindings.communication.on('error', (errorData) => {
        setError(errorData.message || 'Connection error');
        setIsLoading(false);
      });

      // Connect to server
      await bindings.communication.connect(initialCluster);

    } catch (err) {
      setError(err.message || 'Failed to initialize application');
      setIsLoading(false);
    }
  };

  /**
   * @method handleError
   * @private
   * @param {Error} err - Error object
   * @description Handle application errors
   */
  const handleError = (err) => {
    console.error('Application error:', err);
    setError(err.message || 'An unexpected error occurred');
  };

  // Initialize application on mount
  useEffect(() => {
    initializeApplication().catch(handleError);

    // Cleanup on unmount
    return () => {
      if (clientBindings) {
        clientBindings.communication.disconnect();
      }
    };
  }, []);

  // Show loading screen
  if (isLoading) {
    return (
      <div className="app-loading">
              <div className="loading-content">
        <h1>Tootsville</h1>
        <div className="loading-spinner"></div>
        <p>{getGameText('LOADING')}</p>
      </div>
      </div>
    );
  }

  // Show error screen
  if (error) {
    return (
      <div className="app-error">
              <div className="error-content">
        <h1>{getErrorText('CONNECTION_ERROR')}</h1>
        <p>{error}</p>
        <button onClick={initializeApplication}>
          {getGameText('RETRY_CONNECTION')}
        </button>
      </div>
      </div>
    );
  }

  return (
    <div className="app">
      {/* 3D Game Canvas */}
      <div className="game-canvas">
        <Canvas
          camera={{ 
            position: [0, 10, 20], 
            fov: 75,
            near: 0.1,
            far: 1000
          }}
          onCreated={({ gl, scene, camera }) => {
            // Set up renderer
            gl.setClearColor(0x87CEEB); // Sky blue
            gl.shadowMap.enabled = true;
            gl.shadowMap.type = THREE.PCFSoftShadowMap;
            
            // Set up scene
            scene.fog = new THREE.Fog(0x87CEEB, 50, 200);
            
            // Store references for bindings
            if (clientBindings.components.gameWorld) {
              clientBindings.components.gameWorld.setRenderer(gl);
              clientBindings.components.gameWorld.setScene(scene);
              clientBindings.components.gameWorld.setCamera(camera);
            }
          }}
          onError={handleError}
        >
          {/* Game World */}
          <GameWorld />
          
          {/* Camera Controls */}
          <OrbitControls 
            enablePan={true}
            enableZoom={true}
            enableRotate={true}
            maxPolarAngle={Math.PI / 2}
            minDistance={5}
            maxDistance={100}
          />
          
          {/* Lighting */}
          <ambientLight intensity={0.4} />
          <directionalLight
            position={[10, 10, 5]}
            intensity={1}
            castShadow
            shadow-mapSize-width={2048}
            shadow-mapSize-height={2048}
            shadow-camera-far={50}
            shadow-camera-left={-10}
            shadow-camera-right={10}
            shadow-camera-top={10}
            shadow-camera-bottom={-10}
          />
          
          {/* Debug Stats */}
          {enableDebug && <Stats />}
        </Canvas>
      </div>

      {/* Game HUD */}
      <GameHUD />

      {/* Connection Status Indicator */}
      <div className={`connection-status ${connection.isConnected ? 'connected' : 'disconnected'}`}>
        <div className="status-indicator"></div>
        <span>{connection.isConnected ? getGameText('CONNECTED') : getGameText('DISCONNECTED')}</span>
      </div>

      {/* Debug Panel */}
      {enableDebug && (
        <div className="debug-panel">
          <h3>{getUIText('DEBUG', 'DEBUG_INFORMATION')}</h3>
          <div className="debug-content">
            <p><strong>{getUIText('DEBUG', 'CONNECTION_STATUS')}</strong> {connection.isConnected ? 'Yes' : 'No'}</p>
            <p><strong>{getUIText('DEBUG', 'CLUSTER')}</strong> {initialCluster}</p>
            <p><strong>{getUIText('DEBUG', 'RECONNECT_ATTEMPTS')}</strong> {connection.reconnectAttempts}</p>
            <button onClick={() => clientBindings.communication.disconnect()}>
              {getUIText('DEBUG', 'DISCONNECT')}
            </button>
            <button onClick={() => clientBindings.communication.connect(initialCluster)}>
              {getUIText('DEBUG', 'RECONNECT')}
            </button>
          </div>
        </div>
      )}
    </div>
  );
}

export default App;
