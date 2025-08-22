/**
 * @fileoverview Weather and environmental effects system for Tootsville V
 * @module WeatherSystem
 * @author Interworldly Adventuring, LLC
 * Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useRef, useMemo } from 'react';
import { useFrame } from '@react-three/fiber';
import { useTexture } from '@react-three/drei';
import { Vector3, Color, Fog } from 'three';
import useGameStore from '../stores/GameStore';

/**
 * Weather and environmental effects system
 * 
 * @function WeatherSystem
 * @description Manages weather conditions, atmospheric effects, and environmental lighting
 * @param {Object} props - Component properties
 * @param {string} props.world - Current world identifier
 * @returns {JSX.Element} Weather effects and atmospheric elements
 * 
 * @example
 * <WeatherSystem world="CHOR" />
 * 
 * @see {@link RainSystem} for precipitation effects
 * @see {@link FogSystem} for atmospheric fog
 * @see {@link LightingSystem} for dynamic lighting
 */
const WeatherSystem = ({ world = 'CHOR' }) => {
  const { weather, timeOfDay } = useGameStore(state => ({
    weather: state.weather,
    timeOfDay: state.timeOfDay
  }));

  /**
   * Get weather configuration for current conditions
   * @function getWeatherConfig
   * @private
   * @returns {Object} Weather configuration object
   */
  const weatherConfig = useMemo(() => {
    return getWeatherConfiguration(weather, world, timeOfDay);
  }, [weather, world, timeOfDay]);

  /**
   * Apply weather effects to the scene
   * @function applyWeatherEffects
   * @private
   */
  const applyWeatherEffects = () => {
    // Update fog based on weather
    if (weatherConfig.fog) {
      useGameStore.getState().updateFog(weatherConfig.fog);
    }

    // Update lighting based on weather and time
    if (weatherConfig.lighting) {
      useGameStore.getState().updateLighting(weatherConfig.lighting);
    }
  };

  useFrame(() => {
    applyWeatherEffects();
  });

  return (
    <group>
      {/* Rain system */}
      {weatherConfig.precipitation && (
        <RainSystem
          intensity={weatherConfig.precipitation.intensity}
          windSpeed={weatherConfig.precipitation.windSpeed}
        />
      )}

      {/* Snow system */}
      {weatherConfig.snow && (
        <SnowSystem
          intensity={weatherConfig.snow.intensity}
          windSpeed={weatherConfig.snow.windSpeed}
        />
      )}

      {/* Fog system */}
      {weatherConfig.fog && (
        <FogSystem
          density={weatherConfig.fog.density}
          color={weatherConfig.fog.color}
        />
      )}

      {/* Wind effects */}
      {weatherConfig.wind && (
        <WindSystem
          speed={weatherConfig.wind.speed}
          direction={weatherConfig.wind.direction}
        />
      )}

      {/* Atmospheric particles */}
      {weatherConfig.particles && (
        <ParticleSystem
          type={weatherConfig.particles.type}
          density={weatherConfig.particles.density}
        />
      )}
    </group>
  );
};

/**
 * Rain precipitation system
 * 
 * @function RainSystem
 * @description Renders and animates rain particles
 * @param {Object} props - Component properties
 * @param {number} props.intensity - Rain intensity (0-1)
 * @param {number} props.windSpeed - Wind speed affecting rain direction
 * @returns {JSX.Element} Rain particle system
 * 
 * @example
 * <RainSystem intensity={0.7} windSpeed={5} />
 */
const RainSystem = ({ intensity = 0.5, windSpeed = 0 }) => {
  const particlesRef = useRef();
  const particleCount = Math.floor(intensity * 1000);

  /**
   * Generate rain particle positions
   * @function generateRainParticles
   * @private
   * @returns {Array} Array of particle positions
   */
  const rainParticles = useMemo(() => {
    const particles = [];
    for (let i = 0; i < particleCount; i++) {
      particles.push({
        x: (Math.random() - 0.5) * 200,
        y: Math.random() * 100 + 50,
        z: (Math.random() - 0.5) * 200,
        speed: Math.random() * 0.5 + 0.5
      });
    }
    return particles;
  }, [particleCount]);

  /**
   * Animate rain particles
   * @function animateRain
   * @private
   */
  const animateRain = () => {
    if (!particlesRef.current) return;

    rainParticles.forEach((particle, index) => {
      particle.y -= particle.speed;
      particle.x += windSpeed * 0.01;

      // Reset particle when it hits the ground
      if (particle.y < 0) {
        particle.y = Math.random() * 100 + 50;
        particle.x = (Math.random() - 0.5) * 200;
        particle.z = (Math.random() - 0.5) * 200;
      }
    });
  };

  useFrame(() => {
    animateRain();
  });

  return (
    <group ref={particlesRef}>
      {rainParticles.map((particle, index) => (
        <mesh
          key={index}
          position={[particle.x, particle.y, particle.z]}
        >
          <cylinderGeometry args={[0.01, 0.01, 0.5]} />
          <meshBasicMaterial color="#4a90e2" transparent opacity={0.6} />
        </mesh>
      ))}
    </group>
  );
};

/**
 * Snow precipitation system
 * 
 * @function SnowSystem
 * @description Renders and animates snow particles
 * @param {Object} props - Component properties
 * @param {number} props.intensity - Snow intensity (0-1)
 * @param {number} props.windSpeed - Wind speed affecting snow direction
 * @returns {JSX.Element} Snow particle system
 * 
 * @example
 * <SnowSystem intensity={0.8} windSpeed={2} />
 */
const SnowSystem = ({ intensity = 0.5, windSpeed = 0 }) => {
  const particlesRef = useRef();
  const particleCount = Math.floor(intensity * 500);

  /**
   * Generate snow particle positions
   * @function generateSnowParticles
   * @private
   * @returns {Array} Array of particle positions
   */
  const snowParticles = useMemo(() => {
    const particles = [];
    for (let i = 0; i < particleCount; i++) {
      particles.push({
        x: (Math.random() - 0.5) * 200,
        y: Math.random() * 100 + 50,
        z: (Math.random() - 0.5) * 200,
        speed: Math.random() * 0.2 + 0.1,
        rotation: Math.random() * Math.PI * 2
      });
    }
    return particles;
  }, [particleCount]);

  /**
   * Animate snow particles
   * @function animateSnow
   * @private
   */
  const animateSnow = () => {
    if (!particlesRef.current) return;

    snowParticles.forEach((particle, index) => {
      particle.y -= particle.speed;
      particle.x += Math.sin(Date.now() * 0.001 + index) * 0.1 + windSpeed * 0.005;
      particle.rotation += 0.01;

      // Reset particle when it hits the ground
      if (particle.y < 0) {
        particle.y = Math.random() * 100 + 50;
        particle.x = (Math.random() - 0.5) * 200;
        particle.z = (Math.random() - 0.5) * 200;
      }
    });
  };

  useFrame(() => {
    animateSnow();
  });

  return (
    <group ref={particlesRef}>
      {snowParticles.map((particle, index) => (
        <mesh
          key={index}
          position={[particle.x, particle.y, particle.z]}
          rotation={[particle.rotation, particle.rotation, particle.rotation]}
        >
          <planeGeometry args={[0.1, 0.1]} />
          <meshBasicMaterial color="#ffffff" transparent opacity={0.8} />
        </mesh>
      ))}
    </group>
  );
};

/**
 * Atmospheric fog system
 * 
 * @function FogSystem
 * @description Manages atmospheric fog and visibility
 * @param {Object} props - Component properties
 * @param {number} props.density - Fog density (0-1)
 * @param {string} props.color - Fog color
 * @returns {null} This component doesn't render visible elements
 * 
 * @example
 * <FogSystem density={0.3} color="#cccccc" />
 */
const FogSystem = ({ density = 0.1, color = "#cccccc" }) => {
  const { scene } = useThree();

  React.useEffect(() => {
    if (density > 0) {
      scene.fog = new Fog(color, 10, 100 * (1 / density));
    } else {
      scene.fog = null;
    }
  }, [density, color, scene]);

  return null;
};

/**
 * Wind effect system
 * 
 * @function WindSystem
 * @description Applies wind effects to environmental elements
 * @param {Object} props - Component properties
 * @param {number} props.speed - Wind speed
 * @param {Vector3} props.direction - Wind direction vector
 * @returns {null} This component doesn't render visible elements
 * 
 * @example
 * <WindSystem speed={5} direction={[1, 0, 0]} />
 */
const WindSystem = ({ speed = 0, direction = [1, 0, 0] }) => {
  const windVector = useMemo(() => new Vector3(...direction).normalize(), [direction]);

  /**
   * Apply wind effects to scene objects
   * @function applyWindEffects
   * @private
   */
  const applyWindEffects = () => {
    // Apply wind to trees, grass, and other environmental objects
    useGameStore.getState().applyWind(windVector, speed);
  };

  useFrame(() => {
    if (speed > 0) {
      applyWindEffects();
    }
  });

  return null;
};

/**
 * Atmospheric particle system
 * 
 * @function ParticleSystem
 * @description Renders atmospheric particles like dust, pollen, or magical particles
 * @param {Object} props - Component properties
 * @param {string} props.type - Type of particles
 * @param {number} props.density - Particle density
 * @returns {JSX.Element} Particle system
 * 
 * @example
 * <ParticleSystem type="dust" density={0.3} />
 */
const ParticleSystem = ({ type = 'dust', density = 0.1 }) => {
  const particlesRef = useRef();
  const particleCount = Math.floor(density * 200);

  /**
   * Get particle configuration
   * @function getParticleConfig
   * @private
   * @param {string} particleType - Type of particles
   * @returns {Object} Particle configuration
   */
  const getParticleConfig = (particleType) => {
    const configs = {
      dust: { color: '#8b7355', size: 0.02, speed: 0.1 },
      pollen: { color: '#ffff00', size: 0.05, speed: 0.05 },
      magical: { color: '#4a90e2', size: 0.03, speed: 0.2 }
    };
    return configs[particleType] || configs.dust;
  };

  const config = getParticleConfig(type);

  /**
   * Generate atmospheric particles
   * @function generateAtmosphericParticles
   * @private
   * @returns {Array} Array of particle data
   */
  const atmosphericParticles = useMemo(() => {
    const particles = [];
    for (let i = 0; i < particleCount; i++) {
      particles.push({
        x: (Math.random() - 0.5) * 100,
        y: Math.random() * 50,
        z: (Math.random() - 0.5) * 100,
        speed: Math.random() * config.speed + 0.01,
        rotation: Math.random() * Math.PI * 2
      });
    }
    return particles;
  }, [particleCount, config]);

  /**
   * Animate atmospheric particles
   * @function animateAtmosphericParticles
   * @private
   */
  const animateAtmosphericParticles = () => {
    if (!particlesRef.current) return;

    atmosphericParticles.forEach((particle, index) => {
      particle.y += particle.speed;
      particle.rotation += 0.005;

      // Reset particle when it goes too high
      if (particle.y > 50) {
        particle.y = 0;
        particle.x = (Math.random() - 0.5) * 100;
        particle.z = (Math.random() - 0.5) * 100;
      }
    });
  };

  useFrame(() => {
    animateAtmosphericParticles();
  });

  return (
    <group ref={particlesRef}>
      {atmosphericParticles.map((particle, index) => (
        <mesh
          key={index}
          position={[particle.x, particle.y, particle.z]}
          rotation={[particle.rotation, particle.rotation, particle.rotation]}
        >
          <sphereGeometry args={[config.size]} />
          <meshBasicMaterial color={config.color} transparent opacity={0.6} />
        </mesh>
      ))}
    </group>
  );
};

/**
 * Get weather configuration for current conditions
 * @function getWeatherConfiguration
 * @private
 * @param {string} weather - Current weather type
 * @param {string} world - Current world
 * @param {string} timeOfDay - Time of day
 * @returns {Object} Weather configuration object
 */
const getWeatherConfiguration = (weather, world, timeOfDay) => {
  const baseConfig = {
    clear: {
      precipitation: null,
      fog: { density: 0.05, color: '#cccccc' },
      lighting: { intensity: 1.0, color: '#ffffff' },
      wind: { speed: 1, direction: [1, 0, 0] },
      particles: { type: 'dust', density: 0.1 }
    },
    rain: {
      precipitation: { type: 'rain', intensity: 0.7, windSpeed: 3 },
      fog: { density: 0.2, color: '#999999' },
      lighting: { intensity: 0.6, color: '#cccccc' },
      wind: { speed: 5, direction: [1, 0, 0] },
      particles: null
    },
    snow: {
      precipitation: { type: 'snow', intensity: 0.8, windSpeed: 2 },
      fog: { density: 0.3, color: '#ffffff' },
      lighting: { intensity: 0.8, color: '#ffffff' },
      wind: { speed: 3, direction: [1, 0, 0] },
      particles: null
    },
    storm: {
      precipitation: { type: 'rain', intensity: 1.0, windSpeed: 8 },
      fog: { density: 0.4, color: '#666666' },
      lighting: { intensity: 0.4, color: '#888888' },
      wind: { speed: 10, direction: [1, 0, 0] },
      particles: null
    }
  };

  return baseConfig[weather] || baseConfig.clear;
};

export default WeatherSystem;
