/**
 * MapSystem.js - Game world map display component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides an interactive map of the game world showing player location,
 * points of interest, and navigation features.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useRef, useEffect } from 'react';
import { useGameStore } from '../../stores/GameStore';
import './MapSystem.css';

/**
 * MapSystem component for displaying the game world map
 * 
 * @component
 * @returns {JSX.Element} Interactive map interface
 */
const MapSystem = () => {
  const { player, world, isConnected } = useGameStore();
  const [isOpen, setIsOpen] = useState(false);
  const [zoom, setZoom] = useState(1);
  const [center, setCenter] = useState({ x: 0, y: 0 });
  const [selectedPOI, setSelectedPOI] = useState(null);
  const mapRef = useRef(null);
  
  const toggleMap = () => {
    setIsOpen(!isOpen);
    if (!isOpen && player) {
      // Center map on player when opening
      setCenter({ x: player.x || 0, y: player.y || 0 });
    }
  };
  
  const handleZoomIn = () => {
    setZoom(prev => Math.min(prev * 1.2, 3));
  };
  
  const handleZoomOut = () => {
    setZoom(prev => Math.max(prev / 1.2, 0.5));
  };
  
  const handleMapClick = (e) => {
    if (!mapRef.current) return;
    
    const rect = mapRef.current.getBoundingClientRect();
    const x = (e.clientX - rect.left) / zoom;
    const y = (e.clientY - rect.top) / zoom;
    
    // Convert to world coordinates
    const worldX = (x - center.x) * 100;
    const worldY = (y - center.y) * 100;
    
    console.log(`Clicked at world coordinates: (${worldX}, ${worldY})`);
  };
  
  const handlePOIClick = (poi) => {
    setSelectedPOI(poi);
  };
  
  const getPlayerMarker = () => {
    if (!player) return null;
    
    return (
      <div 
        className="player-marker"
        style={{
          left: `${player.x || 0}%`,
          top: `${player.y || 0}%`,
          transform: `translate(-50%, -50%) scale(${zoom})`
        }}
        title={`Player: ${player.name || 'Unknown'}`}
      >
        👤
      </div>
    );
  };
  
  const getPOIMarkers = () => {
    if (!world?.pointsOfInterest) return [];
    
    return world.pointsOfInterest.map(poi => (
      <div
        key={poi.id}
        className={`poi-marker ${poi.type}`}
        style={{
          left: `${poi.x}%`,
          top: `${poi.y}%`,
          transform: `translate(-50%, -50%) scale(${zoom})`
        }}
        onClick={() => handlePOIClick(poi)}
        title={poi.name}
      >
        {poi.icon || '📍'}
      </div>
    ));
  };
  
  const getTerrainOverlay = () => {
    if (!world?.terrain) return null;
    
    return (
      <div className="terrain-overlay">
        {world.terrain.map((region, index) => (
          <div
            key={index}
            className={`terrain-region ${region.type}`}
            style={{
              left: `${region.x}%`,
              top: `${region.y}%`,
              width: `${region.width}%`,
              height: `${region.height}%`
            }}
            title={region.name}
          />
        ))}
      </div>
    );
  };
  
  return (
    <div className="map-system">
      {/* Map Toggle Button */}
      <button 
        className="map-toggle"
        onClick={toggleMap}
        title={isOpen ? 'Close Map' : 'Open Map'}
      >
        🗺️
      </button>
      
      {/* Map Window */}
      {isOpen && (
        <div className="map-window">
          <div className="map-header">
            <h3>World Map</h3>
            <div className="map-controls">
              <button 
                className="map-zoom-in"
                onClick={handleZoomIn}
                title="Zoom In"
              >
                +
              </button>
              <span className="zoom-level">{Math.round(zoom * 100)}%</span>
              <button 
                className="map-zoom-out"
                onClick={handleZoomOut}
                title="Zoom Out"
              >
                -
              </button>
              <button 
                className="map-close"
                onClick={toggleMap}
                title="Close Map"
              >
                ×
              </button>
            </div>
          </div>
          
          <div className="map-content">
            <div 
              ref={mapRef}
              className="map-canvas"
              style={{ transform: `scale(${zoom})` }}
              onClick={handleMapClick}
            >
              {/* Terrain Overlay */}
              {getTerrainOverlay()}
              
              {/* Points of Interest */}
              {getPOIMarkers()}
              
              {/* Player Marker */}
              {getPlayerMarker()}
              
              {/* Grid Lines */}
              <div className="map-grid">
                {Array.from({ length: 10 }, (_, i) => (
                  <React.Fragment key={i}>
                    <div 
                      className="grid-line horizontal"
                      style={{ top: `${i * 10}%` }}
                    />
                    <div 
                      className="grid-line vertical"
                      style={{ left: `${i * 10}%` }}
                    />
                  </React.Fragment>
                ))}
              </div>
            </div>
          </div>
          
          {/* Map Legend */}
          <div className="map-legend">
            <div className="legend-item">
              <span className="legend-icon">👤</span>
              <span className="legend-text">Player</span>
            </div>
            <div className="legend-item">
              <span className="legend-icon">🏠</span>
              <span className="legend-text">Town</span>
            </div>
            <div className="legend-item">
              <span className="legend-icon">🏰</span>
              <span className="legend-text">Castle</span>
            </div>
            <div className="legend-item">
              <span className="legend-icon">🌲</span>
              <span className="legend-text">Forest</span>
            </div>
            <div className="legend-item">
              <span className="legend-icon">🏔️</span>
              <span className="legend-text">Mountain</span>
            </div>
          </div>
          
          {/* POI Details */}
          {selectedPOI && (
            <div className="poi-details">
              <div className="poi-header">
                <span className="poi-icon">{selectedPOI.icon || '📍'}</span>
                <h4>{selectedPOI.name}</h4>
                <button 
                  className="poi-close"
                  onClick={() => setSelectedPOI(null)}
                >
                  ×
                </button>
              </div>
              <p className="poi-description">{selectedPOI.description}</p>
              {selectedPOI.distance && (
                <p className="poi-distance">Distance: {selectedPOI.distance}m</p>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  );
};

export default MapSystem;



