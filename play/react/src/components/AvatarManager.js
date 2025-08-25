/**
 * @fileoverview Avatar management system for Tootsville V
 * @module AvatarManager
 * @author Interworldly Adventuring, LLC
 * @copyright Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useRef, useEffect, useMemo } from 'react';
import { useFrame } from '@react-three/fiber';
import { useGLTF, useAnimations } from '@react-three/drei';
import { Vector3, Quaternion, Matrix4 } from 'three';
import useGameStore from '../stores/GameStore';
import { AnimatedAvatar, useAnimationManager } from './AnimationManager';

/**
 * Manages all avatars in the game world including player and NPC characters
 * 
 * @function AvatarManager
 * @description Handles rendering, positioning, and animation of all character avatars
 * @returns {JSX.Element} Collection of avatar components
 * 
 * @example
 * <AvatarManager />
 * 
 * @see {@link Avatar} for individual avatar rendering
 * @see {@link useGameStore} for avatar state management
 */
const AvatarManager = () => {
  const { avatars, character, activity } = useGameStore(state => ({
    avatars: state.avatars,
    character: state.character,
    activity: state.activity
  }));

  /**
   * Filter avatars to exclude the current player
   * @function filterOtherAvatars
   * @private
   * @returns {Array} Array of other players' avatars
   */
  const otherAvatars = useMemo(() => {
    if (!character) return avatars;
    return avatars.filter(avatar => avatar.id !== character.id);
  }, [avatars, character]);

  return (
    <group>
      {/* Render other players' avatars */}
      {otherAvatars.map(avatar => (
        <Avatar
          key={avatar.id}
          avatar={avatar}
          isPlayer={false}
        />
      ))}
      
      {/* Render current player avatar with enhanced animation */}
      {character && (
        <AnimatedAvatar
          key={character.id}
          avatar={character}
          character={character}
          isPlayer={true}
          position={[activity.lat, activity.alt, activity.long]}
        />
      )}
    </group>
  );
};

/**
 * Individual avatar component with animation and interaction support
 * 
 * @function Avatar
 * @description Renders a single character avatar with animations and physics
 * @param {Object} props - Component properties
 * @param {Object} props.avatar - Avatar data object
 * @param {boolean} props.isPlayer - Whether this is the current player
 * @param {Array} props.position - Position vector [x, y, z]
 * @returns {JSX.Element} The avatar mesh and animations
 * 
 * @example
 * <Avatar 
 *   avatar={avatarData}
 *   isPlayer={true}
 *   position={[0, 0, 0]}
 * />
 */
const Avatar = ({ avatar, isPlayer = false, position = [0, 0, 0] }) => {
  const meshRef = useRef();
  const { scene, animations } = useGLTF(avatar.modelUrl);
  const { actions } = useAnimations(animations, meshRef);

  /**
   * Animation state management
   * @function updateAnimation
   * @private
   * @param {string} animationName - Name of animation to play
   */
  const updateAnimation = (animationName) => {
    if (actions[animationName]) {
      Object.values(actions).forEach(action => action.stop());
      actions[animationName].play();
    }
  };

  /**
   * Handle avatar movement and animation
   * @function handleMovement
   * @private
   */
  const handleMovement = () => {
    if (!meshRef.current) return;

    // Update position
    meshRef.current.position.set(...position);

    // Update animation based on movement state
    if (avatar.isMoving) {
      updateAnimation('walk');
    } else if (avatar.isRunning) {
      updateAnimation('run');
    } else {
      updateAnimation('idle');
    }

    // Handle special animations
    if (avatar.isJumping) {
      updateAnimation('jump');
    } else if (avatar.isSitting) {
      updateAnimation('sit');
    }
  };

  useFrame(() => {
    handleMovement();
  });

  return (
    <group ref={meshRef}>
      <primitive object={scene} />
      
      {/* Interaction zone for non-player avatars */}
      {!isPlayer && (
        <mesh
          position={[0, 1, 0]}
          visible={false}
          onClick={() => {
            useGameStore.getState().selectAvatar(avatar.id);
          }}
        >
          <cylinderGeometry args={[1, 1, 2]} />
          <meshBasicMaterial transparent opacity={0} />
        </mesh>
      )}
    </group>
  );
};

/**
 * Avatar interaction system for player-to-player communication
 * 
 * @function AvatarInteraction
 * @description Handles avatar selection, chat, and social interactions
 * @param {Object} props - Component properties
 * @param {Object} props.targetAvatar - The avatar being interacted with
 * @returns {JSX.Element} Interaction UI elements
 * 
 * @example
 * <AvatarInteraction targetAvatar={selectedAvatar} />
 */
const AvatarInteraction = ({ targetAvatar }) => {
  const { sendMessage, openTrade } = useGameStore();

  if (!targetAvatar) return null;

  return (
    <div className="avatar-interaction-panel">
      <h3>{targetAvatar.name}</h3>
      <button onClick={() => sendMessage(targetAvatar.id, 'Hello!')}>
        Say Hello
      </button>
      <button onClick={() => openTrade(targetAvatar.id)}>
        Trade
      </button>
      <button onClick={() => useGameStore.getState().addFriend(targetAvatar.id)}>
        Add Friend
      </button>
    </div>
  );
};

export default AvatarManager;
