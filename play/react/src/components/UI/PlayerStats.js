/**
 * PlayerStats.js - Player statistics display component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Displays player statistics including currency (Peanuts, Fairy Dust)
 * and appearance customization options.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { useGameStore } from '../../stores/GameStore';
import './PlayerStats.css';

/**
 * PlayerStats component for displaying player statistics
 * 
 * @component
 * @returns {JSX.Element} Player statistics display
 */
const PlayerStats = () => {
  const { player } = useGameStore();
  
  if (!player) {
    return null;
  }
  
  const {
    peanuts = 0,
    fairyDust = 0,
    appearance = {}
  } = player;
  
  const {
    skinColor = 'default',
    pattern = 'none',
    patternColor = 'default',
    clothing = {}
  } = appearance;
  
  return (
    <div className="player-stats">
      <div className="stats-header">
        <h3>Player Stats</h3>
      </div>
      
      <div className="stats-content">
        {/* Currency */}
        <div className="stat-item">
          <span className="stat-label">Peanuts</span>
          <span className="stat-value">{peanuts} 🥜</span>
        </div>
        
        <div className="stat-item">
          <span className="stat-label">Fairy Dust</span>
          <span className="stat-value">{fairyDust} ✨</span>
        </div>
        
        {/* Appearance */}
        <div className="appearance-section">
          <h4>Appearance</h4>
          
          <div className="stat-item">
            <span className="stat-label">Skin Color</span>
            <span className="stat-value">{skinColor}</span>
          </div>
          
          {pattern !== 'none' && (
            <div className="stat-item">
              <span className="stat-label">Pattern</span>
              <span className="stat-value">{pattern} ({patternColor})</span>
            </div>
          )}
          
          {/* Clothing slots */}
          <div className="clothing-section">
            <h5>Clothing</h5>
            {Object.entries(clothing).map(([slot, item]) => (
              <div key={slot} className="stat-item">
                <span className="stat-label">{slot}</span>
                <span className="stat-value">{item?.name || 'None'}</span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default PlayerStats;



