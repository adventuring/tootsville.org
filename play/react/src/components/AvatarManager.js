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
import Avatar from './Avatar';

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
      
      {/* Render current player avatar */}
      {character && (
        <Avatar
          key={character.id}
          avatar={character}
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
