/**
 * @fileoverview Main 3D game world component for Tootsville V
 * @module GameWorld
 * @author Interworldly Adventuring, LLC
 * @copyright Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useRef, useEffect, useMemo } from 'react';
import { Canvas, useFrame, useThree } from '@react-three/fiber';
import { OrbitControls, Sky, Environment, Stats } from '@react-three/drei';
import { Vector3, Color } from 'three';
import useGameStore from '../stores/GameStore';
import WorldTerrain from './WorldTerrain';
import AvatarManager from './AvatarManager';
import ItemManager from './ItemManager';
import WeatherSystem from './WeatherSystem';

/**
 * Main 3D scene component that renders the Tootsville game world
 * 
 * @function GameWorld
 * @description Renders the complete 3D environment including terrain, avatars, items, and weather
 * @param {Object} props - Component properties
 * @param {boolean} props.showStats - Whether to display performance statistics
 * @param {boolean} props.showControls - Whether to show orbit controls for debugging
 * @param {Object} props.cameraSettings - Camera configuration options
 * @returns {JSX.Element} The 3D scene canvas
 * 
 * @example
 * <GameWorld 
 *   showStats={true}
 *   showControls={false}
 *   cameraSettings={{ fov: 75, near: 0.1, far: 1000 }}
 * />
 * 
 * @see {@link WorldTerrain} for terrain rendering
 * @see {@link AvatarManager} for character management
 * @see {@link ItemManager} for item rendering
 * @see {@link WeatherSystem} for environmental effects
 */
const GameWorld = ({ 
  showStats = false, 
  showControls = false,
  cameraSettings = { fov: 75, near: 0.1, far: 1000 }
}) => {
  const { activity, world } = useGameStore(state => ({
    activity: state.activity,
    world: state.activity.world
  }));

  /**
   * Camera position calculation based on player activity
   * @function calculateCameraPosition
   * @private
   * @returns {Vector3} Camera position vector
   */
  const calculateCameraPosition = useMemo(() => {
    return new Vector3(
      activity.lat,
      activity.alt + 10, // 10 units above player
      activity.long
    );
  }, [activity.lat, activity.long, activity.alt]);

  return (
    <div style={{ width: '100vw', height: '100vh' }}>
      <Canvas
        camera={{
          position: calculateCameraPosition.toArray(),
          fov: cameraSettings.fov,
          near: cameraSettings.near,
          far: cameraSettings.far
        }}
        gl={{
          antialias: true,
          alpha: false,
          powerPreference: "high-performance"
        }}
        shadows
      >
        {/* Performance monitoring */}
        {showStats && <Stats />}
        
        {/* Lighting setup */}
        <ambientLight intensity={0.4} />
        <directionalLight
          position={[10, 10, 5]}
          intensity={1}
          castShadow
          shadow-mapSize-width={2048}
          shadow-mapSize-height={2048}
        />
        
        {/* Environment */}
        <Sky sunPosition={[100, 20, 100]} />
        <Environment preset="sunset" />
        
        {/* World components */}
        <WorldTerrain world={world} />
        <AvatarManager />
        <ItemManager />
        <WeatherSystem world={world} />
        
        {/* Camera controls */}
        {showControls && (
          <OrbitControls
            enablePan={true}
            enableZoom={true}
            enableRotate={true}
            maxDistance={1000}
            minDistance={1}
          />
        )}
      </Canvas>
    </div>
  );
};

/**
 * Camera controller component that follows the player
 * @function PlayerCamera
 * @description Automatically positions and orients the camera to follow the player character
 * @param {Object} props - Component properties
 * @param {number} props.followDistance - Distance behind the player to maintain
 * @param {number} props.heightOffset - Height offset above the player
 * @returns {null} This component doesn't render anything
 */
const PlayerCamera = ({ followDistance = 15, heightOffset = 8 }) => {
  const { camera } = useThree();
  const { activity, character } = useGameStore(state => ({
    activity: state.activity,
    character: state.character
  }));

  useFrame(() => {
    if (!character) return;

    const targetPosition = new Vector3(
      activity.lat,
      activity.alt + heightOffset,
      activity.long - followDistance
    );

    camera.position.lerp(targetPosition, 0.1);
    camera.lookAt(activity.lat, activity.alt, activity.long);
  });

  return null;
};

export default GameWorld;
