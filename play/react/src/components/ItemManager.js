/**
 * @fileoverview Item management system for Tootsville V
 * @module ItemManager
 * @author Interworldly Adventuring, LLC
 * @copyright Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import React, { useRef, useMemo, useEffect } from 'react';
import { useFrame } from '@react-three/fiber';
import { useGLTF, Text, Html } from '@react-three/drei';
import { Vector3, Quaternion } from 'three';
import useGameStore from '../stores/GameStore';

/**
 * Manages all items and objects in the game world
 * 
 * @function ItemManager
 * @description Handles rendering, interaction, and state management for all game items
 * @param {Object} props - Component properties
 * @param {Array} props.items - Array of item data objects
 * @param {Object} props.interactionSettings - Item interaction configuration
 * @returns {JSX.Element} Collection of item components
 * 
 * @example
 * <ItemManager 
 *   items={itemData}
 *   interactionSettings={{ range: 5, highlight: true }}
 * />
 * 
 * @see {@link useGameStore} for item state management
 */
const ItemManager = ({ 
  items = [], 
  interactionSettings = { range: 5, highlight: true, showLabels: true }
}) => {
  const { activity, character } = useGameStore(state => ({
    activity: state.activity,
    character: state.character
  }));

  /**
   * Filter items by proximity to player
   * @function filterNearbyItems
   * @private
   * @returns {Array} Array of items within interaction range
   */
  const nearbyItems = useMemo(() => {
    if (!character || !interactionSettings.range) return items;

    const playerPos = new Vector3(activity.lat, activity.alt, activity.long);
    
    return items.filter(item => {
      const itemPos = new Vector3(item.x, item.y, item.z);
      const distance = playerPos.distanceTo(itemPos);
      return distance <= interactionSettings.range;
    });
  }, [items, character, activity, interactionSettings.range]);

  /**
   * Handle item interaction
   * @function handleItemInteraction
   * @private
   * @param {Object} item - Item data object
   */
  const handleItemInteraction = (item) => {
    if (!character) return;

    const playerPos = new Vector3(activity.lat, activity.alt, activity.long);
    const itemPos = new Vector3(item.x, item.y, item.z);
    const distance = playerPos.distanceTo(itemPos);

    if (distance <= interactionSettings.range) {
      useGameStore.getState().actions.interactWithItem(item);
    }
  };

  return (
    <group>
      {nearbyItems.map(item => (
        <Item
          key={item.id}
          item={item}
          isNearby={true}
          onInteraction={handleItemInteraction}
          showLabel={interactionSettings.showLabels}
          highlight={interactionSettings.highlight}
        />
      ))}
      
      {/* Render distant items with lower detail */}
      {items.filter(item => !nearbyItems.includes(item)).map(item => (
        <Item
          key={item.id}
          item={item}
          isNearby={false}
          onInteraction={handleItemInteraction}
          showLabel={false}
          highlight={false}
        />
      ))}
    </group>
  );
};

/**
 * Individual item component with interaction support
 * 
 * @function Item
 * @description Renders a single item with appropriate detail level and interaction
 * @param {Object} props - Component properties
 * @param {Object} props.item - Item data object
 * @param {boolean} props.isNearby - Whether item is within interaction range
 * @param {Function} props.onInteraction - Interaction handler function
 * @param {boolean} props.showLabel - Whether to show item label
 * @param {boolean} props.highlight - Whether to highlight the item
 * @returns {JSX.Element} The item mesh and interactions
 * 
 * @example
 * <Item 
 *   item={itemData}
 *   isNearby={true}
 *   onInteraction={handleInteraction}
 *   showLabel={true}
 *   highlight={true}
 * />
 */
const Item = ({ 
  item, 
  isNearby = false, 
  onInteraction, 
  showLabel = false, 
  highlight = false 
}) => {
  const meshRef = useRef();
  const { scene } = useGLTF(item.modelUrl || '/models/default-item.glb');

  /**
   * Animate item based on type and state
   * @function animateItem
   * @private
   */
  useFrame((state) => {
    if (meshRef.current) {
      // Floating animation for magical items
      if (item.type === 'magical' || item.type === 'treasure') {
        meshRef.current.position.y = item.y + Math.sin(state.clock.elapsedTime * 2) * 0.5;
        meshRef.current.rotation.y += 0.01;
      }
      
      // Highlight animation
      if (highlight && isNearby) {
        meshRef.current.material.emissiveIntensity = Math.sin(state.clock.elapsedTime * 3) * 0.3 + 0.7;
      }
    }
  });

  /**
   * Handle click interaction
   * @function handleClick
   * @private
   */
  const handleClick = () => {
    if (onInteraction) {
      onInteraction(item);
    }
  };

  return (
    <group position={[item.x, item.y, item.z]}>
      {/* Item mesh */}
      <primitive 
        ref={meshRef}
        object={scene} 
        onClick={handleClick}
        scale={isNearby ? 1 : 0.8}
      />
      
      {/* Item label */}
      {showLabel && isNearby && (
        <Html position={[0, 2, 0]} center>
          <div className="item-label">
            <span>{item.name}</span>
            {item.description && (
              <div className="item-description">{item.description}</div>
            )}
          </div>
        </Html>
      )}
      
      {/* Interaction indicator */}
      {highlight && isNearby && (
        <mesh position={[0, 0, 0]}>
          <ringGeometry args={[1.5, 1.8, 32]} />
          <meshBasicMaterial 
            color="#ffff00" 
            transparent 
            opacity={0.5}
          />
        </mesh>
      )}
    </group>
  );
};

/**
 * Item factory for creating different item types
 * 
 * @function createItem
 * @description Creates item data object with appropriate properties
 * @param {string} type - Item type
 * @param {Object} properties - Item properties
 * @returns {Object} Item data object
 * 
 * @example
 * const sword = createItem('weapon', { 
 *   name: 'Iron Sword', 
 *   damage: 10 
 * });
 */
export const createItem = (type, properties = {}) => {
  const baseItem = {
    id: `item_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
    type: type,
    name: properties.name || 'Unknown Item',
    description: properties.description || '',
    modelUrl: properties.modelUrl || '/models/default-item.glb',
    x: properties.x || 0,
    y: properties.y || 0,
    z: properties.z || 0,
    collectible: properties.collectible !== false,
    stackable: properties.stackable || false,
    maxStack: properties.maxStack || 1,
    rarity: properties.rarity || 'common',
    value: properties.value || 0
  };

  // Add type-specific properties
  switch (type) {
    case 'weapon':
      return {
        ...baseItem,
        damage: properties.damage || 1,
        durability: properties.durability || 100,
        weaponType: properties.weaponType || 'sword'
      };
    
    case 'armor':
      return {
        ...baseItem,
        defense: properties.defense || 1,
        durability: properties.durability || 100,
        armorType: properties.armorType || 'chest'
      };
    
    case 'consumable':
      return {
        ...baseItem,
        effect: properties.effect || 'heal',
        effectValue: properties.effectValue || 10,
        duration: properties.duration || 0
      };
    
    case 'treasure':
      return {
        ...baseItem,
        value: properties.value || 100,
        rarity: properties.rarity || 'rare'
      };
    
    case 'magical':
      return {
        ...baseItem,
        spell: properties.spell || 'none',
        manaCost: properties.manaCost || 0,
        cooldown: properties.cooldown || 0
      };
    
    default:
      return baseItem;
  }
};

export default ItemManager;
