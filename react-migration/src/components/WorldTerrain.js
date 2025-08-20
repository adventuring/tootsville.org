/**
 * @fileoverview World terrain generation for Tootsville V
 * @module WorldTerrain
 * @author Interworldly Adventuring, LLC
 * @copyright Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useMemo, useRef } from 'react';
import { useFrame } from '@react-three/fiber';
import { Plane, MeshDistortMaterial } from '@react-three/drei';
import { Vector3, Color } from 'three';
import useGameStore from '../stores/GameStore';

/**
 * Procedural terrain generator for Tootsville worlds
 * 
 * @function WorldTerrain
 * @description Generates and renders terrain based on world type with textures and features
 * @param {Object} props - Component properties
 * @param {string} props.world - World name (CHOR, MOON, OTHM, PINK)
 * @param {Object} props.terrainSettings - Terrain generation settings
 * @returns {JSX.Element} Terrain mesh and materials
 * 
 * @example
 * <WorldTerrain 
 *   world="CHOR"
 *   terrainSettings={{ height: 100, segments: 128 }}
 * />
 * 
 * @see {@link useGameStore} for world state management
 */
const WorldTerrain = ({ 
  world = 'CHOR', 
  terrainSettings = { height: 100, segments: 128, distortion: 0.5 }
}) => {
  const meshRef = useRef();
  const { activity } = useGameStore(state => ({ activity: state.activity }));

  /**
   * Generate terrain geometry based on world type
   * @function generateTerrainGeometry
   * @private
   * @returns {Object} Terrain geometry configuration
   */
  const terrainConfig = useMemo(() => {
    const configs = {
      CHOR: {
        color: '#7ac142',
        height: 100,
        segments: 128,
        distortion: 0.5,
        roughness: 0.8,
        metalness: 0.1
      },
      MOON: {
        color: '#cccccc',
        height: 50,
        segments: 64,
        distortion: 0.3,
        roughness: 0.9,
        metalness: 0.2
      },
      OTHM: {
        color: '#8b4513',
        height: 75,
        segments: 96,
        distortion: 0.7,
        roughness: 0.7,
        metalness: 0.1
      },
      PINK: {
        color: '#ff69b4',
        height: 25,
        segments: 32,
        distortion: 0.2,
        roughness: 0.6,
        metalness: 0.3
      }
    };

    return configs[world] || configs.CHOR;
  }, [world]);

  /**
   * Animate terrain distortion
   * @function animateTerrain
   * @private
   */
  useFrame((state) => {
    if (meshRef.current) {
      meshRef.current.material.distort = Math.sin(state.clock.elapsedTime * 0.1) * 0.1 + 0.5;
    }
  });

  return (
    <group>
      {/* Main terrain plane */}
      <Plane
        ref={meshRef}
        args={[1000, 1000, terrainConfig.segments, terrainConfig.segments]}
        rotation={[-Math.PI / 2, 0, 0]}
        position={[0, -terrainConfig.height / 2, 0]}
        receiveShadow
      >
        <MeshDistortMaterial
          color={terrainConfig.color}
          roughness={terrainConfig.roughness}
          metalness={terrainConfig.metalness}
          distort={terrainConfig.distortion}
          speed={2}
        />
      </Plane>

      {/* Terrain features based on world type */}
      {world === 'CHOR' && <ChorFeatures />}
      {world === 'MOON' && <MoonFeatures />}
      {world === 'OTHM' && <OthmFeatures />}
      {world === 'PINK' && <PinkFeatures />}
    </group>
  );
};

/**
 * CHOR world specific terrain features
 * @function ChorFeatures
 * @private
 * @returns {JSX.Element} CHOR world features
 */
const ChorFeatures = () => (
  <group>
    {/* Trees, rocks, and other CHOR features */}
    <mesh position={[50, 0, 50]} castShadow>
      <cylinderGeometry args={[2, 2, 20]} />
      <meshStandardMaterial color="#228b22" />
    </mesh>
  </group>
);

/**
 * MOON world specific terrain features
 * @function MoonFeatures
 * @private
 * @returns {JSX.Element} MOON world features
 */
const MoonFeatures = () => (
  <group>
    {/* Craters and lunar features */}
    <mesh position={[30, -5, 30]} castShadow>
      <sphereGeometry args={[10, 16, 16]} />
      <meshStandardMaterial color="#666666" />
    </mesh>
  </group>
);

/**
 * OTHM world specific terrain features
 * @function OthmFeatures
 * @private
 * @returns {JSX.Element} OTHM world features
 */
const OthmFeatures = () => (
  <group>
    {/* Desert features */}
    <mesh position={[40, 0, 40]} castShadow>
      <coneGeometry args={[5, 15]} />
      <meshStandardMaterial color="#daa520" />
    </mesh>
  </group>
);

/**
 * PINK world specific terrain features
 * @function PinkFeatures
 * @private
 * @returns {JSX.Element} PINK world features
 */
const PinkFeatures = () => (
  <group>
    {/* Candy-like features */}
    <mesh position={[20, 0, 20]} castShadow>
      <sphereGeometry args={[8, 16, 16]} />
      <meshStandardMaterial color="#ff1493" />
    </mesh>
  </group>
);

export default WorldTerrain;
