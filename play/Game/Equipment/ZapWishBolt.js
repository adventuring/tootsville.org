/**
 * ZapWishBolt.js - Zap Wish Bolt equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that shoots magical wish bolts at targets with various effects.
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


Tootsville.Game.Equipment.ZapWishBolt = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Zap Wish Bolt",
    description: "A magical device that shoots bolts of wish energy",
    type: "magical",
    rarity: "epic",
    icon: "⚡",
    maxUses: 4,
    cooldown: 3000, // 3 seconds
    boltSpeed: 15, // Units per second
    maxRange: 50, // Maximum range in units
    boltDamage: 5 // Damage dealt to targets
  },

  /**
   * Initialize the zap wish bolt device
   * @param {Object} player - The player using the equipment
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("ZapWishBolt: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.activeBolts = [];
    
    return true;
  },

  /**
   * Use the zap wish bolt device
   * @param {Object} target - Target object or position
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'zap_wish_bolt',
          'stun',
          targetPlayer || player,
          { id: 'system', name: 'Zap Wish Bolt' },
          `The wish bolt device is recharging! Please wait a moment.`
        );
      };
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'zap_wish_bolt',
          'stun',
          targetPlayer || player,
          { id: 'system', name: 'Zap Wish Bolt' },
          `The wish bolt device is depleted!`
        );
      };
      return false;
    }
    
    // Determine target position
    let targetPosition = null;
    
    if (target && target.avatar && target.avatar.position) {
      targetPosition = target.avatar.position.clone();
    } else if (target && target.x !== undefined && target.y !== undefined && target.z !== undefined) {
      targetPosition = new THREE.Vector3(target.x, target.y, target.z);
    } else {
      // Use player's forward direction
      targetPosition = this.getForwardTarget();
    }
    
    if (!targetPosition) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'zap_wish_bolt',
          'error',
          `No valid target found for the wish bolt.`
        );
      };
      return false;
    }
    
    // Fire wish bolt
    const success = this.fireWishBolt(targetPosition);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPosition);
      
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'zap_wish_bolt',
          'stun',
          targetPlayer || player,
          { id: 'system', name: 'Zap Wish Bolt' },
          `You fired a Zap Wish Bolt!`
        );
      };
    }
    
    return success;
  },

  /**
   * Get forward target position based on player direction
   * @returns {THREE.Vector3} Target position
   */
  getForwardTarget: function() {
    if (!this.player.avatar || !this.player.avatar.position) {
      return null;
    }
    
    const playerPos = this.player.avatar.position.clone();
    const forward = new THREE.Vector3(0, 0, -1);
    
    // Apply player rotation
    if (this.player.avatar.rotation) {
      forward.applyEuler(this.player.avatar.rotation);
    }
    
    // Scale by range
    forward.multiplyScalar(this.metadata.maxRange);
    
    return playerPos.add(forward);
  },

  /**
   * Fire a wish bolt at the target position
   * @param {THREE.Vector3} targetPosition - The target position
   * @returns {boolean} Success status
   */
  fireWishBolt: function(targetPosition) {
    try {
      if (!this.player.avatar || !this.player.avatar.position) {
        return false;
      }
      
      const startPosition = this.player.avatar.position.clone();
      const direction = targetPosition.clone().sub(startPosition).normalize();
      
      // Create wish bolt mesh
      const boltGeometry = new THREE.CylinderGeometry(0.05, 0.05, 0.5, 8);
      const boltMaterial = new THREE.MeshBasicMaterial({
        color: 0x00ffff,
        transparent: true,
        opacity: 0.9
      });
      
      const bolt = new THREE.Mesh(boltGeometry, boltMaterial);
      bolt.position.copy(startPosition);
      bolt.lookAt(targetPosition);
      
      // Add to scene
      if (Tootsville.Tank && Tootsville.Tank.scene) {
        Tootsville.Tank.scene.add(bolt);
      }
      
      // Create bolt data
      const boltData = {
        mesh: bolt,
        startPosition: startPosition.clone(),
        targetPosition: targetPosition.clone(),
        direction: direction,
        distance: startPosition.distanceTo(targetPosition),
        distanceTraveled: 0,
        speed: this.metadata.boltSpeed,
        damage: this.metadata.boltDamage,
        startTime: Date.now()
      };
      
      this.activeBolts.push(boltData);
      
      // Animate the bolt
      this.animateWishBolt(boltData);
      
      return true;
    } catch (error) {
      console.error("ZapWishBolt: Error firing wish bolt:", error);
      return false;
    }
  },

  /**
   * Animate a wish bolt
   * @param {Object} boltData - The bolt data
   */
  animateWishBolt: function(boltData) {
    const animate = () => {
      if (!boltData.mesh || !this.activeBolts.includes(boltData)) {
        return; // Bolt was removed
      }
      
      // Move bolt forward
      const deltaDistance = boltData.speed * 0.016; // Assuming 60fps
      boltData.distanceTraveled += deltaDistance;
      
      const progress = boltData.distanceTraveled / boltData.distance;
      
      if (progress >= 1) {
        // Bolt reached target
        this.impactWishBolt(boltData);
        return;
      }
      
      // Update position
      const newPosition = boltData.startPosition.clone().lerp(
        boltData.targetPosition, 
        progress
      );
      boltData.mesh.position.copy(newPosition);
      
      // Add trail effect
      this.createBoltTrail(boltData.mesh.position);
      
      // Rotate bolt
      boltData.mesh.rotation.z += 0.3;
      
      requestAnimationFrame(animate);
    };
    
    animate();
  },

  /**
   * Create bolt trail effect
   * @param {THREE.Vector3} position - Current bolt position
   */
  createBoltTrail: function(position) {
    // Create small sparkle trail
    const trailGeometry = new THREE.SphereGeometry(0.02, 4, 4);
    const trailMaterial = new THREE.MeshBasicMaterial({
      color: 0x00ffff,
      transparent: true,
      opacity: 0.6
    });
    
    const trail = new THREE.Mesh(trailGeometry, trailMaterial);
    trail.position.copy(position);
    
    // Add random offset
    trail.position.x += (Math.random() - 0.5) * 0.1;
    trail.position.y += (Math.random() - 0.5) * 0.1;
    trail.position.z += (Math.random() - 0.5) * 0.1;
    
    if (Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.add(trail);
    }
    
    // Remove trail after short time
    setTimeout(() => {
      if (Tootsville.Tank && Tootsville.Tank.scene) {
        Tootsville.Tank.scene.remove(trail);
      }
      trail.geometry.dispose();
      trail.material.dispose();
    }, 500);
  },

  /**
   * Handle wish bolt impact
   * @param {Object} boltData - The bolt data
   */
  impactWishBolt: function(boltData) {
    // Create impact effect
    this.createImpactEffect(boltData.targetPosition);
    
    // Check for player hits
    this.checkPlayerHits(boltData);
    
    // Remove bolt
    this.removeWishBolt(boltData);
  },

  /**
   * Create impact effect at target position
   * @param {THREE.Vector3} position - Impact position
   */
  createImpactEffect: function(position) {
    // Create explosion effect
    const explosionGeometry = new THREE.SphereGeometry(2, 16, 16);
    const explosionMaterial = new THREE.MeshBasicMaterial({
      color: 0x00ffff,
      transparent: true,
      opacity: 0.8
    });
    
    const explosion = new THREE.Mesh(explosionGeometry, explosionMaterial);
    explosion.position.copy(position);
    
    if (Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.add(explosion);
    }
    
    // Animate explosion
    let scale = 0.1;
    const animateExplosion = () => {
      scale += 0.1;
      explosion.scale.set(scale, scale, scale);
      explosion.material.opacity = 0.8 - (scale * 0.1);
      
      if (scale < 2) {
        requestAnimationFrame(animateExplosion);
      } else {
        // Remove explosion
        if (Tootsville.Tank && Tootsville.Tank.scene) {
          Tootsville.Tank.scene.remove(explosion);
        }
        explosion.geometry.dispose();
        explosion.material.dispose();
      }
    };
    
    animateExplosion();
  },

  /**
   * Check for player hits
   * @param {Object} boltData - The bolt data
   */
  checkPlayerHits: function(boltData) {
    if (!Tootsville.Gossip || !Tootsville.Gossip.Network) {
      return;
    }
    
    const hitRadius = 2; // Hit detection radius
    
    // Check all nearby players
    const nearbyPlayers = Tootsville.Gossip.Network.getNearbyPlayers(
      boltData.targetPosition, 
      hitRadius
    );
    
    nearbyPlayers.forEach(player => {
      if (player !== this.player) {
        // Apply damage or effect to player
        this.applyBoltEffect(player, boltData.damage);
      }
    });
  },

  /**
   * Apply bolt effect to player
   * @param {Object} player - The player to affect
   * @param {number} damage - Damage amount
   */
  applyBoltEffect: function(player, damage) {
    // This would integrate with the player health/damage system
    // For now, just show a message
    Tootsville.Gossip.Parrot.say(
      "Zap Wish Bolt",
      `${player.name} was hit by a wish bolt!`
    );
    
    // Send damage event to server
    if (Tootsville.Gossip && Tootsville.Gossip.Network) {
      Tootsville.Gossip.Network.send('player_damage', {
        targetId: player.id || player.uuid,
        damage: damage,
        source: 'zapWishBolt'
      });
    }
  },

  /**
   * Remove a wish bolt
   * @param {Object} boltData - The bolt data
   */
  removeWishBolt: function(boltData) {
    const index = this.activeBolts.indexOf(boltData);
    if (index > -1) {
      this.activeBolts.splice(index, 1);
    }
    
    if (boltData.mesh && Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.remove(boltData.mesh);
      boltData.mesh.geometry.dispose();
      boltData.mesh.material.dispose();
    }
  },

  /**
   * Update the equipment display
   */
  updateEquipmentDisplay: function() {
    if (!this.player || !this.player.equipment) {
      return;
    }
    
    // Update uses remaining display
    const equipmentSlot = this.player.equipment.find(slot => 
      slot.item && slot.item.name === this.metadata.name
    );
    
    if (equipmentSlot) {
      equipmentSlot.usesRemaining = this.usesRemaining;
      equipmentSlot.maxUses = this.metadata.maxUses;
      
      // Trigger UI update
      if (Tootsville.UI && Tootsville.UI.HUD) {
        Tootsville.UI.HUD.refreshEquipment();
      }
    }
  },

  /**
   * Broadcast the wish bolt effect to other players
   * @param {THREE.Vector3} targetPosition - The target position
   */
  broadcastEffect: function(targetPosition) {
    if (!Tootsville.Gossip || !Tootsville.Gossip.Network) {
      return;
    }
    
    const effectData = {
      type: 'zapWishBolt',
      playerId: this.player.id || this.player.uuid,
      startPosition: this.player.avatar ? this.player.avatar.position : null,
      targetPosition: targetPosition,
      boltSpeed: this.metadata.boltSpeed,
      boltDamage: this.metadata.boltDamage,
      timestamp: Date.now()
    };
    
    Tootsville.Gossip.Network.broadcast('equipment_effect', effectData);
  },

  /**
   * Handle wish bolt effect from other players
   * @param {Object} effectData - The effect data received
   */
  handleRemoteEffect: function(effectData) {
    if (effectData.type !== 'zapWishBolt') {
      return;
    }
    
    // Create remote wish bolt
    if (effectData.startPosition && effectData.targetPosition) {
      const startPos = new THREE.Vector3(
        effectData.startPosition.x,
        effectData.startPosition.y,
        effectData.startPosition.z
      );
      const targetPos = new THREE.Vector3(
        effectData.targetPosition.x,
        effectData.targetPosition.y,
        effectData.targetPosition.z
      );
      
      this.fireWishBolt(targetPos);
    }
  },

  /**
   * Get equipment status information
   * @returns {Object} Status information
   */
  getStatus: function() {
    const now = Date.now();
    const cooldownRemaining = Math.max(0, this.metadata.cooldown - (now - this.lastUsed));
    
    return {
      name: this.metadata.name,
      usesRemaining: this.usesRemaining,
      maxUses: this.metadata.maxUses,
      cooldownRemaining: cooldownRemaining,
      activeBolts: this.activeBolts.length,
      canUse: this.usesRemaining > 0 && cooldownRemaining === 0
    };
  },

  /**
   * Clean up resources when equipment is removed
   */
  cleanup: function() {
    // Remove all active bolts
    this.activeBolts.forEach(boltData => {
      this.removeWishBolt(boltData);
    });
    
    this.activeBolts = [];
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
  }
};

// Register with equipment system
if (Tootsville.Game && Tootsville.Game.Equipment) {
  Tootsville.Game.Equipment.register('ZapWishBolt', Tootsville.Game.Equipment.ZapWishBolt);
}
