/**
 * AnimationManager.js - Player Character Animation State Management
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Manages player character animation states with proper transitions between walking, idle, and other states.
 * Supports character-specific movement capabilities (elephants can't jump, manatees can swim, etc.).
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { useRef, useEffect, useCallback, useState } from 'react';
import { useFrame } from '@react-three/fiber';
import { useAnimations } from '@react-three/drei';
import { Vector3 } from 'three';

/**
 * Animation state constants
 */
const ANIMATION_STATES = {
  IDLE: 'idle',
  WALKING: 'walk',
  RUNNING: 'run',
  SITTING: 'sit',
  JUMPING: 'jump',
  SWIMMING: 'swim',
  FLYING: 'fly',
  EMOTING: 'emote',
  USING_ITEM: 'use_item',
  TALKING: 'talk'
};

/**
 * Character type definitions with movement capabilities
 */
const CHARACTER_CAPABILITIES = {
  // Toots (elephants) - can't jump but can walk, run, sit, swim
  TOOT: {
    canJump: false,
    canSwim: true,
    canFly: false,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'swim']
  },
  // Manatees - can swim, can't jump or fly
  MANATEE: {
    canJump: false,
    canSwim: true,
    canFly: false,
    canWalk: false,
    canRun: false,
    canSit: false,
    defaultAnimations: ['idle', 'swim']
  },
  // Birds/Katootels - can fly, jump, walk
  BIRD: {
    canJump: true,
    canSwim: false,
    canFly: true,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'jump', 'fly']
  },
  // Default for unknown character types
  DEFAULT: {
    canJump: true,
    canSwim: false,
    canFly: false,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'jump']
  }
};

/**
 * Animation transition settings
 */
const TRANSITION_SETTINGS = {
  FADE_DURATION: 0.3,
  CROSSFADE_DURATION: 0.2,
  MIN_MOVEMENT_THRESHOLD: 0.01,
  MOVEMENT_CHECK_INTERVAL: 100 // milliseconds
};

/**
 * Get character capabilities based on character type
 * @param {Object} character - Character data
 * @returns {Object} Character capabilities
 */
const getCharacterCapabilities = (character) => {
  if (!character) return CHARACTER_CAPABILITIES.DEFAULT;
  
  // Determine character type based on avatar or character properties
  const avatarName = character.avatar || character.name || '';
  const characterType = character.type || character.species || '';
  const avatarClass = character.avatarClass || {};
  const avatarTitle = avatarClass.title || avatarClass.filename || '';
  
  // Check for specific character types first
  if (characterType.toLowerCase().includes('manatee') || 
      avatarName.toLowerCase().includes('manatee')) {
    return CHARACTER_CAPABILITIES.MANATEE;
  }
  
  if (characterType.toLowerCase().includes('bird') || 
      characterType.toLowerCase().includes('katootel') ||
      avatarName.toLowerCase().includes('bird') ||
      avatarName.toLowerCase().includes('katootel')) {
    return CHARACTER_CAPABILITIES.BIRD;
  }
  
  // Check for Toot (elephant) avatars specifically
  // Only treat as Toot if they have UltraToot or similar elephant avatars
  if (avatarName.toLowerCase().includes('ultratoot') ||
      avatarName.toLowerCase().includes('toot') ||
      avatarTitle.toLowerCase().includes('ultratoot') ||
      avatarTitle.toLowerCase().includes('toot') ||
      characterType.toLowerCase().includes('toot') ||
      characterType.toLowerCase().includes('elephant')) {
    return CHARACTER_CAPABILITIES.TOOT;
  }
  
  // For unknown character types, use DEFAULT capabilities
  // This allows other character types to have their own movement abilities
  return CHARACTER_CAPABILITIES.DEFAULT;
};

/**
 * Check if character is in water (for swimming detection)
 * @param {Object} character - Character data
 * @param {Object} world - World data
 * @returns {boolean} True if character is in water
 */
const isInWater = (character, world) => {
  if (!character || !world) return false;
  
  // Check if character is in a water world or specific water areas
  const worldName = world.name || character.world || 'CHOR';
  const altitude = character.altitude || character.alt || 0;
  
  // Check for water worlds or low altitude (underwater)
  if (worldName === 'OCEAN' || altitude < 0) {
    return true;
  }
  
  // Check for specific water areas in the world
  const position = character.position || { x: 0, y: 0, z: 0 };
  // This would need to be expanded based on actual world water areas
  // For now, we'll use a simple check based on world type
  
  return false;
};

/**
 * Animation Manager Hook
 * @param {Object} avatar - Avatar object with model and animations
 * @param {Object} character - Character data including position and state
 * @returns {Object} Animation state and controls
 */
export const useAnimationManager = (avatar, character) => {
  const meshRef = useRef();
  const { actions } = useAnimations(avatar?.animations || [], meshRef);
  
  // Animation state tracking
  const stateRef = useRef({
    currentState: ANIMATION_STATES.IDLE,
    previousState: null,
    isTransitioning: false,
    lastPosition: new Vector3(),
    lastMovementTime: 0,
    movementVelocity: new Vector3(),
    idleTimer: 0,
    transitionStartTime: 0,
    characterCapabilities: null
  });

  /**
   * Get current animation state
   */
  const getCurrentState = useCallback(() => {
    return stateRef.current.currentState;
  }, []);

  /**
   * Check if character is moving
   */
  const isMoving = useCallback((currentPosition) => {
    const lastPos = stateRef.current.lastPosition;
    const distance = currentPosition.distanceTo(lastPos);
    return distance > TRANSITION_SETTINGS.MIN_MOVEMENT_THRESHOLD;
  }, []);

  /**
   * Calculate movement velocity
   */
  const calculateVelocity = useCallback((currentPosition, deltaTime) => {
    const lastPos = stateRef.current.lastPosition;
    const velocity = currentPosition.clone().sub(lastPos).divideScalar(deltaTime);
    stateRef.current.movementVelocity.lerp(velocity, 0.1); // Smooth velocity
    return stateRef.current.movementVelocity.length();
  }, []);

  /**
   * Determine appropriate animation state based on character capabilities
   */
  const determineAnimationState = useCallback((character, velocity, world) => {
    const capabilities = stateRef.current.characterCapabilities || CHARACTER_CAPABILITIES.DEFAULT;
    
    // Check for special states first
    if (character.isSitting && capabilities.canSit) return ANIMATION_STATES.SITTING;
    if (character.isUsingItem) return ANIMATION_STATES.USING_ITEM;
    if (character.isTalking) return ANIMATION_STATES.TALKING;
    if (character.isEmoting) return ANIMATION_STATES.EMOTING;
    
    // Check for jumping (only if character can jump)
    if (character.isJumping && capabilities.canJump) return ANIMATION_STATES.JUMPING;
    
    // Check for swimming (if character can swim and is in water)
    if (capabilities.canSwim && isInWater(character, world)) {
      if (velocity > 0.1) return ANIMATION_STATES.SWIMMING;
      return ANIMATION_STATES.IDLE; // Idle in water
    }
    
    // Check for flying (if character can fly and is airborne)
    if (capabilities.canFly && character.isFlying) return ANIMATION_STATES.FLYING;
    
    // Check movement-based states (only if character can walk/run)
    if (capabilities.canWalk || capabilities.canRun) {
      if (velocity > 0.5 && capabilities.canRun) return ANIMATION_STATES.RUNNING;
      if (velocity > 0.1 && capabilities.canWalk) return ANIMATION_STATES.WALKING;
    }
    
    // Default to idle
    return ANIMATION_STATES.IDLE;
  }, []);

  /**
   * Play animation with smooth transition
   */
  const playAnimation = useCallback((animationName, crossfade = true) => {
    if (!actions[animationName]) {
      console.warn(`Animation "${animationName}" not found for character`);
      return false;
    }

    const currentState = stateRef.current.currentState;
    if (currentState === animationName) return true;

    const now = Date.now();
    stateRef.current.previousState = currentState;
    stateRef.current.currentState = animationName;
    stateRef.current.isTransitioning = true;
    stateRef.current.transitionStartTime = now;

    if (crossfade && actions[currentState]) {
      // Crossfade between animations
      actions[currentState].fadeOut(TRANSITION_SETTINGS.CROSSFADE_DURATION);
      actions[animationName].reset().fadeIn(TRANSITION_SETTINGS.CROSSFADE_DURATION).play();
    } else {
      // Stop all animations and play new one
      Object.values(actions).forEach(action => {
        if (action.isRunning()) {
          action.fadeOut(TRANSITION_SETTINGS.FADE_DURATION);
        }
      });
      
      setTimeout(() => {
        actions[animationName].reset().fadeIn(TRANSITION_SETTINGS.FADE_DURATION).play();
      }, TRANSITION_SETTINGS.FADE_DURATION * 1000);
    }

    return true;
  }, [actions]);

  /**
   * Update animation based on character state and capabilities
   */
  const updateAnimation = useCallback((character, currentPosition, deltaTime, world) => {
    if (!character || !currentPosition) return;

    // Update character capabilities if needed
    if (!stateRef.current.characterCapabilities) {
      stateRef.current.characterCapabilities = getCharacterCapabilities(character);
    }

    const now = Date.now();
    const isCurrentlyMoving = isMoving(currentPosition);
    const velocity = calculateVelocity(currentPosition, deltaTime);
    const targetState = determineAnimationState(character, velocity, world);

    // Update idle timer
    if (!isCurrentlyMoving) {
      stateRef.current.idleTimer += deltaTime;
    } else {
      stateRef.current.idleTimer = 0;
      stateRef.current.lastMovementTime = now;
    }

    // Check if we need to transition to a new animation
    const currentState = stateRef.current.currentState;
    if (targetState !== currentState && !stateRef.current.isTransitioning) {
      playAnimation(targetState);
    }

    // Handle transition completion
    if (stateRef.current.isTransitioning) {
      const transitionElapsed = now - stateRef.current.transitionStartTime;
      const transitionDuration = TRANSITION_SETTINGS.CROSSFADE_DURATION * 1000;
      
      if (transitionElapsed >= transitionDuration) {
        stateRef.current.isTransitioning = false;
      }
    }

    // Update last position for next frame
    stateRef.current.lastPosition.copy(currentPosition);
  }, [isMoving, calculateVelocity, determineAnimationState, playAnimation]);

  /**
   * Initialize animations
   */
  const initializeAnimations = useCallback(() => {
    if (!actions) return;

    // Set up default animation settings
    Object.values(actions).forEach(action => {
      action.setLoop(THREE.LoopRepeat);
      action.clampWhenFinished = true;
      action.enabled = true;
    });

    // Start with idle animation
    if (actions[ANIMATION_STATES.IDLE]) {
      actions[ANIMATION_STATES.IDLE].play();
    }
  }, [actions]);

  /**
   * Clean up animations
   */
  const cleanupAnimations = useCallback(() => {
    if (!actions) return;

    Object.values(actions).forEach(action => {
      if (action.isRunning()) {
        action.stop();
      }
    });
  }, [actions]);

  // Initialize on mount
  useEffect(() => {
    initializeAnimations();
    return cleanupAnimations;
  }, [initializeAnimations, cleanupAnimations]);

  // Animation frame update
  useFrame((state, delta) => {
    if (!character || !meshRef.current) return;

    const currentPosition = meshRef.current.position;
    const world = { name: character.world || 'CHOR' };
    updateAnimation(character, currentPosition, delta, world);
  });

  return {
    meshRef,
    getCurrentState,
    playAnimation,
    isMoving: () => isMoving(meshRef.current?.position || new Vector3()),
    getAnimationState: () => stateRef.current,
    getCharacterCapabilities: () => stateRef.current.characterCapabilities
  };
};

/**
 * Enhanced Avatar Component with Animation Management
 */
export const AnimatedAvatar = ({ avatar, character, isPlayer = false, position = [0, 0, 0] }) => {
  const { meshRef, getCurrentState, playAnimation, getCharacterCapabilities } = useAnimationManager(avatar, character);

  // Handle special animation triggers
  useEffect(() => {
    if (!character) return;

    // Handle emotion changes
    if (character.emotion && character.emotion !== 'neutral') {
      playAnimation(ANIMATION_STATES.EMOTING);
    }

    // Handle item usage
    if (character.activeItem && character.isUsingItem) {
      playAnimation(ANIMATION_STATES.USING_ITEM);
    }

    // Handle talking state
    if (character.isTalking) {
      playAnimation(ANIMATION_STATES.TALKING);
    }
  }, [character?.emotion, character?.isUsingItem, character?.isTalking, playAnimation]);

  return (
    <group ref={meshRef} position={position}>
      {avatar?.scene && <primitive object={avatar.scene} />}
      
      {/* Animation state indicator (debug) */}
      {process.env.NODE_ENV === 'development' && (
        <Html position={[0, 3, 0]} center>
          <div style={{
            background: 'rgba(0,0,0,0.7)',
            color: 'white',
            padding: '4px 8px',
            borderRadius: '4px',
            fontSize: '12px',
            fontFamily: 'monospace'
          }}>
            {getCurrentState()}
            {getCharacterCapabilities() && (
              <div style={{ fontSize: '10px', opacity: 0.8 }}>
                {getCharacterCapabilities().canJump ? '✓Jump' : '✗Jump'} 
                {getCharacterCapabilities().canSwim ? ' ✓Swim' : ' ✗Swim'}
                {getCharacterCapabilities().canFly ? ' ✓Fly' : ' ✗Fly'}
              </div>
            )}
          </div>
        </Html>
      )}
    </group>
  );
};

/**
 * Animation State Hook for external components
 */
export const useAnimationState = () => {
  const [animationState, setAnimationState] = useState({
    currentState: ANIMATION_STATES.IDLE,
    isMoving: false,
    velocity: 0,
    isTransitioning: false
  });

  return {
    ...animationState,
    ANIMATION_STATES,
    CHARACTER_CAPABILITIES
  };
};

export default useAnimationManager;
