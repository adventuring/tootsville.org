/**
 * PropsAirBlaster.js - Props Air Blaster equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that shoots air blasts to knock things around and knock down sitting characters.
 * Non-violent equipment that uses air pressure for fun interactions.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
// Import Game Event Notifications system
if (typeof require !== 'undefined') {
  try {
    require('../GameEventNotifications.js');
  } catch (e) {
    // Fallback for browser environment
    if (!Tootsville.Game) Tootsville.Game = {};
    if (!Tootsville.Game.EventNotifications) {
      // Load script dynamically
      const script = document.createElement('script');
      script.src = 'Game/GameEventNotifications.js';
      document.head.appendChild(script);
    }
  }
}


Tootsville.Game.Equipment.PropsAirBlaster = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Props Air Blaster",
    description: "Shoots air blasts to knock things around and knock down characters",
    type: "tool",
    rarity: "uncommon",
    icon: "💨",
    maxUses: 10,
    cooldown: 2000, // 2 seconds
    range: 15, // units
    blastRadius: 3 // units
  },

  /**
   * Initialize the air blaster
   * @param {Object} player - The player using the blaster
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("PropsAirBlaster: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    
    return true;
  },

  /**
   * Use the air blaster
   * @param {Object} target - Target object or direction
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'props_air_blaster',
        'unknown',
        targetPlayer || player,
        { id: 'system', name: 'Props Air Blaster' },
        `The air blaster needs to recharge! Please wait a moment.`
      );
    };
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'props_air_blaster',
        'unknown',
        targetPlayer || player,
        { id: 'system', name: 'Props Air Blaster' },
        `The air blaster is out of pressure!`
      );
    };
      return false;
    }
    
    // Get direction from target or player's facing direction
    const direction = this.getBlastDirection(target);
    
    // Create air blast effect
    const success = this.createAirBlast(direction);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(direction);
      
      // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'props_air_blaster',
        'unknown',
        targetPlayer || player,
        { id: 'system', name: 'Props Air Blaster' },
        `Whoosh! You fired an air blast!`
      );
    };
    }
    
    return success;
  },

  /**
   * Get blast direction from target or player's facing direction
   * @param {Object} target - Target object or direction
   * @returns {Object} Direction vector
   */
  getBlastDirection: function(target) {
    if (target && target.position) {
      // Calculate direction to target
      const playerPos = this.player.avatar.position;
      const targetPos = target.position;
      
      return {
        x: targetPos.x - playerPos.x,
        y: targetPos.y - playerPos.y,
        z: targetPos.z - playerPos.z
      };
    } else {
      // Use player's facing direction
      const avatar = this.player.avatar;
      const rotation = avatar.rotation || { y: 0 };
      
      return {
        x: Math.sin(rotation.y),
        y: 0,
        z: Math.cos(rotation.y)
      };
    }
  },

  /**
   * Create air blast effect
   * @param {Object} direction - Direction vector
   * @returns {boolean} Success status
   */
  createAirBlast: function(direction) {
    try {
      const playerPos = this.player.avatar.position;
      const blastStart = { ...playerPos };
      
      // Create visual air blast effect
      this.createVisualEffect(blastStart, direction);
      
      // Check for objects and players in blast path
      this.checkBlastCollisions(blastStart, direction);
      
      return true;
      
    } catch (error) {
      console.error("PropsAirBlaster: Error creating air blast:", error);
      return false;
    }
  },

  /**
   * Create visual air blast effect
   * @param {Object} startPos - Starting position
   * @param {Object} direction - Direction vector
   */
  createVisualEffect: function(startPos, direction) {
    // Create air blast particle effect
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      Tootsville.Scene.createParticleEffect({
        type: 'air_blast',
        position: startPos,
        direction: direction,
        duration: 1000,
        particleCount: 20,
        color: '#ffffff',
        size: 0.5
      });
    }
  },

  /**
   * Check for collisions with objects and players
   * @param {Object} startPos - Starting position
   * @param {Object} direction - Direction vector
   */
  checkBlastCollisions: function(startPos, direction) {
    const blastRadius = this.metadata.blastRadius;
    const range = this.metadata.range;
    
    // Check for nearby players
    if (Tootsville.Game && Tootsville.Game.getNearbyPlayers) {
      const nearbyPlayers = Tootsville.Game.getNearbyPlayers(startPos, range);
      
      nearbyPlayers.forEach(player => {
        if (player === this.player) return; // Skip self
        
        const distance = this.getDistance(startPos, player.avatar.position);
        if (distance <= blastRadius) {
          this.affectPlayer(player);
        }
      });
    }
    
    // Check for nearby objects/props
    if (Tootsville.Scene && Tootsville.Scene.getNearbyObjects) {
      const nearbyObjects = Tootsville.Scene.getNearbyObjects(startPos, range);
      
      nearbyObjects.forEach(obj => {
        const distance = this.getDistance(startPos, obj.position);
        if (distance <= blastRadius) {
          this.affectObject(obj);
        }
      });
    }
  },

  /**
   * Affect a player with the air blast
   * @param {Object} player - The player to affect
   */
  affectPlayer: function(player) {
    // Knock down player (put them in sitting position)
    this.knockDownPlayer(player);
  },

  /**
   * Knock down a player (put them in sitting position)
   * @param {Object} player - The player to knock down
   */
  knockDownPlayer: function(player) {
    // Set player to sitting state
    if (player.state) {
      player.state.sitting = true;
    } else {
      player.state = { sitting: true };
    }
    
    // Apply knockback effect
    const knockbackForce = 2;
    const playerPos = player.avatar.position;
    const blastDirection = this.getBlastDirection(player);
    
    const newPos = {
      x: playerPos.x + (blastDirection.x * knockbackForce),
      y: playerPos.y,
      z: playerPos.z + (blastDirection.z * knockbackForce)
    };
    
    // Move player
    player.avatar.position.set(newPos.x, newPos.y, newPos.z);
    
    // Broadcast effect through proper game messaging system
    this.broadcastPlayerEffect(player, 'knockdown');

    // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'props_air_blaster',
        'knockdown',
        player,
        { id: 'system', name: 'Props Air Blaster' }, // System as source
        `${player.name} was knocked down by the air blast!`
      );
    }
  },

  /**
   * Push a standing player
   * @param {Object} player - The player to push
   */
  pushPlayer: function(player) {
    const pushForce = 1;
    const playerPos = player.avatar.position;
    const blastDirection = this.getBlastDirection(player);
    
    const newPos = {
      x: playerPos.x + (blastDirection.x * pushForce),
      y: playerPos.y,
      z: playerPos.z + (blastDirection.z * pushForce)
    };
    
    // Move player
    player.avatar.position.set(newPos.x, newPos.y, newPos.z);
    
    // Broadcast effect
    this.broadcastPlayerEffect(player, 'push');
  },

  /**
   * Affect an object with the air blast
   * @param {Object} obj - The object to affect
   */
  affectObject: function(obj) {
    // Check if object can be moved
    if (obj.movable !== false) {
      const pushForce = 1.5;
      const objPos = obj.position;
      const blastDirection = this.getBlastDirection(obj);
      
      const newPos = {
        x: objPos.x + (blastDirection.x * pushForce),
        y: objPos.y,
        z: objPos.z + (blastDirection.z * pushForce)
      };
      
      // Move object
      obj.position.set(newPos.x, newPos.y, newPos.z);
      
      // Broadcast object effect
      this.broadcastObjectEffect(obj);
    }
  },

  /**
   * Calculate distance between two points
   * @param {Object} pos1 - First position
   * @param {Object} pos2 - Second position
   * @returns {number} Distance
   */
  getDistance: function(pos1, pos2) {
    const dx = pos2.x - pos1.x;
    const dy = pos2.y - pos1.y;
    const dz = pos2.z - pos1.z;
    
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
  },

  /**
   * Update the equipment display
   */
  updateEquipmentDisplay: function() {
    const equipmentIcon = document.querySelector('.equipment-icon[data-item="props-air-blaster"]');
    if (equipmentIcon) {
      const usesDisplay = equipmentIcon.querySelector('.uses-display');
      if (usesDisplay) {
        usesDisplay.textContent = `${this.usesRemaining}/${this.metadata.maxUses}`;
      }
      
      if (this.usesRemaining <= 0) {
        equipmentIcon.classList.add('disabled');
      }
    }
  },

  /**
   * Broadcast the effect to other players
   * @param {Object} direction - Blast direction
   */
  broadcastEffect: function(direction) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect',
        equipment: 'props_air_blaster',
        playerId: this.player.id,
        direction: direction,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast player effect
   * @param {Object} player - Affected player
   * @param {string} effectType - Type of effect
   */
  broadcastPlayerEffect: function(player, effectType) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'player_effect',
        equipment: 'props_air_blaster',
        targetPlayerId: player.id,
        effectType: effectType,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast object effect
   * @param {Object} obj - Affected object
   */
  broadcastObjectEffect: function(obj) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'object_effect',
        equipment: 'props_air_blaster',
        objectId: obj.id,
        newPosition: obj.position,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Get equipment info for UI display
   * @returns {Object} Equipment information
   */
  getInfo: function() {
    return {
      ...this.metadata,
      usesRemaining: this.usesRemaining,
      canUse: this.usesRemaining > 0 && (Date.now() - this.lastUsed) >= this.metadata.cooldown
    };
  },

  /**
   * Clean up the equipment
   */
  cleanup: function() {
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
  }
};
