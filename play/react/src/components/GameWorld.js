/**
 * GameWorld.js - Main 3D world component for Tootsville
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Manages the 3D world rendering, scene setup, and game state integration
 * using React Three Fiber and Three.js.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React, { useRef, useEffect, useState } from 'react';
import { Canvas, useFrame, useThree } from '@react-three/fiber';
import { OrbitControls, Sky, Stars, Environment, Html } from '@react-three/drei';
import * as THREE from 'three';
import { useGameStore } from '../stores/GameStore';
import { CommunicationService } from '../services/CommunicationService';

/**
 * Main 3D world component
 * @param {Object} props - Component props
 * @returns {JSX.Element} GameWorld component
 */
const GameWorld = ({ onWorldReady }) => {
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);
  const worldRef = useRef();
  const { world, characters, items, weather } = useGameStore();

  useEffect(() => {
    const initializeWorld = async () => {
      try {
        setIsLoading(true);
        
        // Initialize communication service
        const commService = new CommunicationService();
        await commService.connect('test.tootsville.org');
        
        // Notify parent component that world is ready
        if (onWorldReady) {
          onWorldReady(commService);
        }
        
        setIsLoading(false);
      } catch (err) {
        setError(err.message);
        setIsLoading(false);
      }
    };

    initializeWorld();
  }, [onWorldReady]);

  if (error) {
    return (
      <div className="error-container">
        <h2>Failed to load world</h2>
        <p>{error}</p>
        <button onClick={() => window.location.reload()}>Retry</button>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="loading-container">
        <div className="loading-spinner"></div>
        <p>Loading Tootsville...</p>
      </div>
    );
  }

  return (
    <div className="game-world">
      <Canvas
        camera={{ position: [0, 5, 10], fov: 75 }}
        shadows
        gl={{ antialias: true, alpha: false }}
      >
        <WorldScene world={world} characters={characters} items={items} weather={weather} />
      </Canvas>
    </div>
  );
};

/**
 * Main world scene component
 * @param {Object} props - Scene props
 * @returns {JSX.Element} WorldScene component
 */
const WorldScene = ({ world, characters, items, weather }) => {
  const { scene, camera } = useThree();
  const controlsRef = useRef();

  // Set up scene
  useEffect(() => {
    scene.background = new THREE.Color(0x87ceeb); // Sky blue
    scene.fog = new THREE.Fog(0x87ceeb, 10, 100);
  }, [scene]);

  // Animation loop
  useFrame((state, delta) => {
    // Update world animations
    if (world) {
      // Update weather effects
      if (weather && weather.particles) {
        weather.particles.forEach(particle => {
          particle.position.y -= delta * particle.speed;
          if (particle.position.y < -10) {
            particle.position.y = 20;
          }
        });
      }
    }
  });

  return (
    <>
      {/* Lighting */}
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
      <Stars radius={100} depth={50} count={5000} factor={4} />
      <Environment preset="sunset" />
      
      {/* Ground */}
      <Ground />
      
      {/* World objects */}
      {world && <WorldObjects world={world} />}
      
      {/* Characters */}
      {characters && characters.map(character => (
        <Character key={character.id} character={character} />
      ))}
      
      {/* Items */}
      {items && items.map(item => (
        <WorldItem key={item.id} item={item} />
      ))}
      
      {/* Weather effects */}
      {weather && <WeatherEffects weather={weather} />}
      
      {/* Controls */}
      <OrbitControls
        ref={controlsRef}
        enablePan={true}
        enableZoom={true}
        enableRotate={true}
        maxPolarAngle={Math.PI / 2}
        minDistance={2}
        maxDistance={50}
      />
    </>
  );
};

/**
 * Ground plane component
 * @returns {JSX.Element} Ground component
 */
const Ground = () => {
  return (
    <mesh rotation={[-Math.PI / 2, 0, 0]} position={[0, -0.5, 0]} receiveShadow>
      <planeGeometry args={[100, 100]} />
      <meshStandardMaterial color={0x90EE90} />
    </mesh>
  );
};

/**
 * World objects component
 * @param {Object} props - World objects props
 * @returns {JSX.Element} WorldObjects component
 */
const WorldObjects = ({ world }) => {
  if (!world.objects) return null;

  return (
    <group>
      {world.objects.map(object => (
        <WorldObject key={object.id} object={object} />
      ))}
    </group>
  );
};

/**
 * Individual world object component
 * @param {Object} props - Object props
 * @returns {JSX.Element} WorldObject component
 */
const WorldObject = ({ object }) => {
  const meshRef = useRef();

  useFrame(() => {
    if (meshRef.current && object.rotation) {
      meshRef.current.rotation.y += 0.01;
    }
  });

  return (
    <mesh
      ref={meshRef}
      position={[object.position.x, object.position.y, object.position.z]}
      castShadow
      receiveShadow
    >
      <boxGeometry args={[object.size.x, object.size.y, object.size.z]} />
      <meshStandardMaterial color={object.color || 0x808080} />
    </mesh>
  );
};

/**
 * Character component
 * @param {Object} props - Character props
 * @returns {JSX.Element} Character component
 */
const Character = ({ character }) => {
  const meshRef = useRef();

  useFrame(() => {
    if (meshRef.current && character.animation) {
      // Update character animations
      meshRef.current.position.set(
        character.position.x,
        character.position.y,
        character.position.z
      );
    }
  });

  return (
    <group position={[character.position.x, character.position.y, character.position.z]}>
      {/* Character body */}
      <mesh ref={meshRef} castShadow>
        <capsuleGeometry args={[0.5, 1, 4, 8]} />
        <meshStandardMaterial color={character.color || 0x00ff00} />
      </mesh>
      
      {/* Character name */}
      <Html position={[0, 2, 0]} center>
        <div className="character-name">{character.name}</div>
      </Html>
    </group>
  );
};

/**
 * World item component
 * @param {Object} props - Item props
 * @returns {JSX.Element} WorldItem component
 */
const WorldItem = ({ item }) => {
  const meshRef = useRef();

  useFrame(() => {
    if (meshRef.current && item.floating) {
      meshRef.current.position.y = item.position.y + Math.sin(Date.now() * 0.001) * 0.1;
    }
  });

  return (
    <mesh
      ref={meshRef}
      position={[item.position.x, item.position.y, item.position.z]}
      castShadow
    >
      <sphereGeometry args={[0.3, 16, 16]} />
      <meshStandardMaterial color={item.color || 0xff0000} />
    </mesh>
  );
};

/**
 * Weather effects component
 * @param {Object} props - Weather props
 * @returns {JSX.Element} WeatherEffects component
 */
const WeatherEffects = ({ weather }) => {
  const particlesRef = useRef();

  useEffect(() => {
    if (weather.type === 'rain' && particlesRef.current) {
      const geometry = new THREE.BufferGeometry();
      const positions = [];
      
      for (let i = 0; i < 1000; i++) {
        positions.push(
          Math.random() * 100 - 50,
          Math.random() * 50,
          Math.random() * 100 - 50
        );
      }
      
      geometry.setAttribute('position', new THREE.Float32BufferAttribute(positions, 3));
      particlesRef.current.geometry = geometry;
    }
  }, [weather.type]);

  if (weather.type === 'rain') {
    return (
      <points ref={particlesRef}>
        <bufferGeometry />
        <pointsMaterial size={0.1} color={0x87ceeb} transparent opacity={0.6} />
      </points>
    );
  }

  return null;
};

export default GameWorld;
