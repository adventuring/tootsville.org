/**
 * LoadingScreen.js - Application loading screen
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Displays loading states during application initialization.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React from 'react';
import './LoadingScreen.css';

/**
 * Loading screen component
 * @returns {JSX.Element} LoadingScreen component
 */
const LoadingScreen = () => {
  return (
    <div className="loading-screen">
      <div className="loading-container">
        <div className="loading-logo">
          <h1>Tootsville</h1>
        </div>
        
        <div className="loading-spinner">
          <div className="spinner"></div>
        </div>
        
        <div className="loading-text">
          <p>Loading the world...</p>
          <p className="loading-subtitle">Please wait while we prepare your adventure</p>
        </div>
        
        <div className="loading-progress">
          <div className="progress-bar">
            <div className="progress-fill"></div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LoadingScreen;
