/**
 * AnimationManager.js - Player Character Animation State Management
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Manages player character animation states with proper transitions between walking, idle, and other states.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { useRef, useEffect, useCallback } from 'react';
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
  EMOTING: 'emote',
  USING_ITEM: 'use_item',
  TALKING: 'talk'
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
    transitionStartTime: 0
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
   * Determine appropriate animation state
   */
  const determineAnimationState = useCallback((character, velocity) => {
    // Check for special states first
    if (character.isSitting) return ANIMATION_STATES.SITTING;
    if (character.isJumping) return ANIMATION_STATES.JUMPING;
    if (character.isUsingItem) return ANIMATION_STATES.USING_ITEM;
    if (character.isTalking) return ANIMATION_STATES.TALKING;
    if (character.isEmoting) return ANIMATION_STATES.EMOTING;
    
    // Check movement-based states
    if (velocity > 0.5) return ANIMATION_STATES.RUNNING;
    if (velocity > 0.1) return ANIMATION_STATES.WALKING;
    
    // Default to idle
    return ANIMATION_STATES.IDLE;
  }, []);

  /**
   * Play animation with smooth transition
   */
  const playAnimation = useCallback((animationName, crossfade = true) => {
    if (!actions[animationName]) {
      console.warn(`Animation "${animationName}" not found`);
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
   * Update animation based on character state
   */
  const updateAnimation = useCallback((character, currentPosition, deltaTime) => {
    if (!character || !currentPosition) return;

    const now = Date.now();
    const isCurrentlyMoving = isMoving(currentPosition);
    const velocity = calculateVelocity(currentPosition, deltaTime);
    const targetState = determineAnimationState(character, velocity);

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
    updateAnimation(character, currentPosition, delta);
  });

  return {
    meshRef,
    getCurrentState,
    playAnimation,
    isMoving: () => isMoving(meshRef.current?.position || new Vector3()),
    getAnimationState: () => stateRef.current
  };
};

/**
 * Enhanced Avatar Component with Animation Management
 */
export const AnimatedAvatar = ({ avatar, character, isPlayer = false, position = [0, 0, 0] }) => {
  const { meshRef, getCurrentState, playAnimation } = useAnimationManager(avatar, character);

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
    ANIMATION_STATES
  };
};

export default useAnimationManager;
