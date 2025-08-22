/**
 * KaTootelEggShooter.js - KaTootel Egg Shooter equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that shoots KaTootel eggs for fun effects.
 * Non-violent equipment that creates egg-based interactions.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.Game.Equipment.KaTootelEggShooter = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "KaTootel Egg Shooter",
    description: "Shoots KaTootel eggs for fun effects",
    type: "tool",
    rarity: "uncommon",
    icon: "🥚",
    maxUses: 8,
    cooldown: 1500, // 1.5 seconds
    range: 25, // units
    eggSpeed: 15, // units per second
    eggRadius: 1.5 // units
  },

  /**
   * Initialize the KaTootel egg shooter
   * @param {Object} player - The player using the shooter
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("KaTootelEggShooter: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.activeEggs = new Map(); // Track active egg projectiles
    
    return true;
  },

  /**
   * Use the KaTootel egg shooter
   * @param {Object} target - Target position or object
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      Tootsville.Gossip.Parrot.say(
        "KaTootel Egg Shooter",
        "The egg shooter needs to reload! Please wait a moment."
      );
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      Tootsville.Gossip.Parrot.say(
        "KaTootel Egg Shooter",
        "You're out of KaTootel eggs!"
      );
      return false;
    }
    
    // Get target position
    const targetPos = this.getTargetPosition(target);
    
    // Shoot egg
    const success = this.shootEgg(targetPos);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPos);
      
      Tootsville.Gossip.Parrot.say(
        "KaTootel Egg Shooter",
        "Splat! You shot a KaTootel egg!"
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
   * Shoot a KaTootel egg
   * @param {Object} targetPos - Target position
   * @returns {boolean} Success status
   */
  shootEgg: function(targetPos) {
    try {
      const eggId = `egg_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
      const playerPos = this.player.avatar.position;
      
      // Calculate direction
      const direction = {
        x: targetPos.x - playerPos.x,
        y: targetPos.y - playerPos.y,
        z: targetPos.z - playerPos.z
      };
      
      // Normalize direction
      const length = Math.sqrt(direction.x * direction.x + direction.y * direction.y + direction.z * direction.z);
      direction.x /= length;
      direction.y /= length;
      direction.z /= length;
      
      // Create egg projectile
      this.createEggProjectile(playerPos, direction, eggId);
      
      // Track active egg
      this.activeEggs.set(eggId, {
        position: { ...playerPos },
        direction: direction,
        startTime: Date.now(),
        distance: 0
      });
      
      return true;
      
    } catch (error) {
      console.error("KaTootelEggShooter: Error shooting egg:", error);
      return false;
    }
  },

  /**
   * Create egg projectile
   * @param {Object} startPos - Starting position
   * @param {Object} direction - Direction vector
   * @param {string} eggId - Unique egg identifier
   */
  createEggProjectile: function(startPos, direction, eggId) {
    // Create egg mesh/geometry
    if (Tootsville.Scene && Tootsville.Scene.createEggMesh) {
      Tootsville.Scene.createEggMesh({
        position: startPos,
        direction: direction,
        speed: this.metadata.eggSpeed,
        radius: this.metadata.eggRadius,
        color: '#f4f4f4', // Egg white
        id: eggId
      });
    }
    
    // Create egg trail effect
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      Tootsville.Scene.createParticleEffect({
        type: 'egg_trail',
        position: startPos,
        direction: direction,
        duration: 1000,
        particleCount: 10,
        color: '#f4f4f4',
        size: 0.2,
        id: eggId
      });
    }
  },

  /**
   * Update egg projectiles (called by game loop)
   * @param {number} deltaTime - Time since last update in milliseconds
   */
  updateEggs: function(deltaTime) {
    const eggsToRemove = [];
    
    for (const [eggId, egg] of this.activeEggs) {
      // Update egg position
      const distance = this.metadata.eggSpeed * (deltaTime / 1000);
      egg.position.x += egg.direction.x * distance;
      egg.position.y += egg.direction.y * distance;
      egg.position.z += egg.direction.z * distance;
      egg.distance += distance;
      
      // Check if egg has reached maximum range
      if (egg.distance >= this.metadata.range) {
        eggsToRemove.push(eggId);
        this.eggHit(egg.position, eggId, 'range');
        continue;
      }
      
      // Check for collisions
      this.checkEggCollisions(egg.position, eggId);
      
      // Update egg mesh position
      if (Tootsville.Scene && Tootsville.Scene.updateEggMesh) {
        Tootsville.Scene.updateEggMesh(eggId, egg.position);
      }
    }
    
    // Remove eggs that have hit something or reached range
    eggsToRemove.forEach(eggId => {
      this.removeEgg(eggId);
    });
  },

  /**
   * Check for egg collisions
   * @param {Object} eggPos - Egg position
   * @param {string} eggId - Egg identifier
   */
  checkEggCollisions: function(eggPos, eggId) {
    // Check for nearby players
    if (Tootsville.Game && Tootsville.Game.getNearbyPlayers) {
      const nearbyPlayers = Tootsville.Game.getNearbyPlayers(eggPos, this.metadata.eggRadius);
      
      nearbyPlayers.forEach(player => {
        if (player === this.player) return; // Skip self
        
        const distance = this.getDistance(eggPos, player.avatar.position);
        if (distance <= this.metadata.eggRadius) {
          this.eggHit(eggPos, eggId, 'player', player);
          return;
        }
      });
    }
    
    // Check for nearby objects
    if (Tootsville.Scene && Tootsville.Scene.getNearbyObjects) {
      const nearbyObjects = Tootsville.Scene.getNearbyObjects(eggPos, this.metadata.eggRadius);
      
      nearbyObjects.forEach(obj => {
        const distance = this.getDistance(eggPos, obj.position);
        if (distance <= this.metadata.eggRadius) {
          this.eggHit(eggPos, eggId, 'object', obj);
          return;
        }
      });
    }
  },

  /**
   * Handle egg hitting something
   * @param {Object} hitPos - Hit position
   * @param {string} eggId - Egg identifier
   * @param {string} hitType - Type of hit ('player', 'object', 'range')
   * @param {Object} hitTarget - Hit target (player or object)
   */
  eggHit: function(hitPos, eggId, hitType, hitTarget = null) {
    // Create egg splat effect
    this.createEggSplat(hitPos);
    
    // Handle different hit types
    switch (hitType) {
      case 'player':
        this.handlePlayerHit(hitTarget);
        break;
      case 'object':
        this.handleObjectHit(hitTarget);
        break;
      case 'range':
        // Egg just reached maximum range
        break;
    }
    
    // Remove the egg
    this.removeEgg(eggId);
    
    // Broadcast hit
    this.broadcastEggHit(hitPos, hitType, hitTarget);
  },

  /**
   * Handle egg hitting a player
   * @param {Object} player - Hit player
   */
  handlePlayerHit: function(player) {
    // Create fun effect on player
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      const position = player.avatar.position;
      
      Tootsville.Scene.createParticleEffect({
        type: 'egg_splat_player',
        position: position,
        duration: 3000,
        particleCount: 25,
        color: '#f4f4f4',
        size: 0.4,
        spread: 3.0
      });
    }
    
    Tootsville.Gossip.Parrot.say(
      "KaTootel Egg Shooter",
      `${player.name} got egged!`
    );
  },

  /**
   * Handle egg hitting an object
   * @param {Object} obj - Hit object
   */
  handleObjectHit: function(obj) {
    // Create egg splat effect on object
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      Tootsville.Scene.createParticleEffect({
        type: 'egg_splat_object',
        position: obj.position,
        duration: 2000,
        particleCount: 15,
        color: '#f4f4f4',
        size: 0.3,
        spread: 2.0
      });
    }
  },

  /**
   * Create egg splat effect
   * @param {Object} position - Splat position
   */
  createEggSplat: function(position) {
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      Tootsville.Scene.createParticleEffect({
        type: 'egg_splat',
        position: position,
        duration: 2000,
        particleCount: 20,
        color: '#f4f4f4',
        size: 0.5,
        spread: 2.5
      });
    }
  },

  /**
   * Remove egg projectile
   * @param {string} eggId - Egg identifier
   */
  removeEgg: function(eggId) {
    // Remove from tracking
    this.activeEggs.delete(eggId);
    
    // Remove egg mesh
    if (Tootsville.Scene && Tootsville.Scene.removeEggMesh) {
      Tootsville.Scene.removeEggMesh(eggId);
    }
    
    // Remove particle effects
    if (Tootsville.Scene && Tootsville.Scene.removeParticleEffect) {
      Tootsville.Scene.removeParticleEffect(eggId);
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
    const equipmentIcon = document.querySelector('.equipment-icon[data-item="katootel-egg-shooter"]');
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
        equipment: 'katootel_egg_shooter',
        playerId: this.player.id,
        targetPosition: targetPos,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast egg hit
   * @param {Object} hitPos - Hit position
   * @param {string} hitType - Hit type
   * @param {Object} hitTarget - Hit target
   */
  broadcastEggHit: function(hitPos, hitType, hitTarget) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'egg_hit',
        equipment: 'katootel_egg_shooter',
        playerId: this.player.id,
        hitPosition: hitPos,
        hitType: hitType,
        hitTargetId: hitTarget ? hitTarget.id : null,
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
      activeEggs: this.activeEggs.size,
      canUse: this.usesRemaining > 0 && (Date.now() - this.lastUsed) >= this.metadata.cooldown
    };
  },

  /**
   * Clean up the equipment
   */
  cleanup: function() {
    // Remove all active eggs
    for (const [eggId] of this.activeEggs) {
      this.removeEgg(eggId);
    }
    
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
    this.activeEggs.clear();
  }
};
