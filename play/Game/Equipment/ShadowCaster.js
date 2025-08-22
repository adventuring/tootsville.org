/**
 * ShadowCaster.js - Shadow Caster equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that throws black gummy "Shaddow" energy/tar for fun effects.
 * Non-violent equipment that creates shadowy effects and temporary obstacles.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.Game.Equipment.ShadowCaster = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Shadow Caster",
    description: "Throws black gummy 'Shaddow' energy/tar for fun effects",
    type: "tool",
    rarity: "rare",
    icon: "fixme-black-splat-mark",
    maxUses: 5,
    cooldown: 3000, // 3 seconds
    range: 20, // units
    shadowDuration: 10000, // 10 seconds
    shadowRadius: 2 // units
  },

  /**
   * Initialize the shadow caster
   * @param {Object} player - The player using the caster
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("ShadowCaster: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.activeShadows = new Map(); // Track active shadow effects
    
    return true;
  },

  /**
   * Use the shadow caster
   * @param {Object} target - Target position or object
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      Tootsville.Gossip.Parrot.say(
        "Shadow Caster",
        "The shadow caster needs to recharge! Please wait a moment."
      );
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      Tootsville.Gossip.Parrot.say(
        "Shadow Caster",
        "The shadow caster is out of shadow energy!"
      );
      return false;
    }
    
    // Get target position
    const targetPos = this.getTargetPosition(target);
    
    // Create shadow effect
    const success = this.createShadow(targetPos);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPos);
      
      Tootsville.Gossip.Parrot.say(
        "Shadow Caster",
        "Sploosh! You cast a shadow!"
      );
    }
    
    return success;
  },

  /**
   * Get target position from target or player's facing direction
   * @param {Object} target - Target object or position
   * @returns {Object} Target position
   */
  getTargetPosition: function(target) {
    if (target && target.position) {
      return { ...target.position };
    } else if (target && target.x !== undefined) {
      return { ...target };
    } else {
      // Use player's facing direction at range
      const avatar = this.player.avatar;
      const playerPos = avatar.position;
      const rotation = avatar.rotation || { y: 0 };
      
      return {
        x: playerPos.x + (Math.sin(rotation.y) * this.metadata.range),
        y: playerPos.y,
        z: playerPos.z + (Math.cos(rotation.y) * this.metadata.range)
      };
    }
  },

  /**
   * Create shadow effect at target position
   * @param {Object} targetPos - Target position
   * @returns {boolean} Success status
   */
  createShadow: function(targetPos) {
    try {
      const shadowId = `shadow_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
      
      // Create visual shadow effect
      this.createVisualShadow(targetPos, shadowId);
      
      // Create shadow obstacle
      this.createShadowObstacle(targetPos, shadowId);
      
      // Track active shadow
      this.activeShadows.set(shadowId, {
        position: targetPos,
        createdAt: Date.now(),
        duration: this.metadata.shadowDuration
      });
      
      // Schedule shadow removal
      setTimeout(() => {
        this.removeShadow(shadowId);
      }, this.metadata.shadowDuration);
      
      return true;
      
    } catch (error) {
      console.error("ShadowCaster: Error creating shadow:", error);
      return false;
    }
  },

  /**
   * Create visual shadow effect
   * @param {Object} position - Shadow position
   * @param {string} shadowId - Unique shadow identifier
   */
  createVisualShadow: function(position, shadowId) {
    // Create shadow particle effect
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      Tootsville.Scene.createParticleEffect({
        type: 'shadow_tar',
        position: position,
        duration: this.metadata.shadowDuration,
        particleCount: 50,
        color: '#000000',
        size: 1.0,
        opacity: 0.8,
        spread: this.metadata.shadowRadius,
        id: shadowId
      });
    }
    
    // Create shadow mesh/geometry
    if (Tootsville.Scene && Tootsville.Scene.createShadowMesh) {
      Tootsville.Scene.createShadowMesh({
        position: position,
        radius: this.metadata.shadowRadius,
        color: '#000000',
        opacity: 0.6,
        id: shadowId
      });
    }
  },

  /**
   * Create shadow obstacle that affects movement
   * @param {Object} position - Shadow position
   * @param {string} shadowId - Unique shadow identifier
   */
  createShadowObstacle: function(position, shadowId) {
    // Create collision area
    if (Tootsville.Scene && Tootsville.Scene.createCollisionArea) {
      Tootsville.Scene.createCollisionArea({
        type: 'shadow_tar',
        position: position,
        radius: this.metadata.shadowRadius,
        effect: 'slow_movement',
        slowFactor: 0.5, // 50% slower movement
        id: shadowId
      });
    }
  },

  /**
   * Remove shadow effect
   * @param {string} shadowId - Shadow identifier to remove
   */
  removeShadow: function(shadowId) {
    // Remove from tracking
    this.activeShadows.delete(shadowId);
    
    // Remove visual effects
    if (Tootsville.Scene && Tootsville.Scene.removeParticleEffect) {
      Tootsville.Scene.removeParticleEffect(shadowId);
    }
    
    if (Tootsville.Scene && Tootsville.Scene.removeShadowMesh) {
      Tootsville.Scene.removeShadowMesh(shadowId);
    }
    
    // Remove collision area
    if (Tootsville.Scene && Tootsville.Scene.removeCollisionArea) {
      Tootsville.Scene.removeCollisionArea(shadowId);
    }
    
    // Broadcast removal
    this.broadcastShadowRemoval(shadowId);
  },

  /**
   * Check if player is in shadow area
   * @param {Object} playerPos - Player position
   * @returns {boolean} True if player is in shadow
   */
  isInShadow: function(playerPos) {
    for (const [shadowId, shadow] of this.activeShadows) {
      const distance = this.getDistance(playerPos, shadow.position);
      if (distance <= this.metadata.shadowRadius) {
        return true;
      }
    }
    return false;
  },

  /**
   * Apply shadow effects to player movement
   * @param {Object} player - The player to affect
   * @param {Object} movement - Movement vector
   * @returns {Object} Modified movement vector
   */
  applyShadowEffect: function(player, movement) {
    if (this.isInShadow(player.avatar.position)) {
      // Slow down movement in shadow
      return {
        x: movement.x * 0.5,
        y: movement.y * 0.5,
        z: movement.z * 0.5
      };
    }
    return movement;
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
    const equipmentIcon = document.querySelector('.equipment-icon[data-item="shadow-caster"]');
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
   * @param {Object} targetPos - Target position
   */
  broadcastEffect: function(targetPos) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect',
        equipment: 'shadow_caster',
        playerId: this.player.id,
        targetPosition: targetPos,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast shadow removal
   * @param {string} shadowId - Shadow identifier
   */
  broadcastShadowRemoval: function(shadowId) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'shadow_removal',
        equipment: 'shadow_caster',
        shadowId: shadowId,
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
      activeShadows: this.activeShadows.size,
      canUse: this.usesRemaining > 0 && (Date.now() - this.lastUsed) >= this.metadata.cooldown
    };
  },

  /**
   * Clean up the equipment
   */
  cleanup: function() {
    // Remove all active shadows
    for (const [shadowId] of this.activeShadows) {
      this.removeShadow(shadowId);
    }
    
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
    this.activeShadows.clear();
  }
};
