/**
 * ShrinkPotion.js - Shrink Potion equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that makes the player character smaller when used.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.Game.Equipment.ShrinkPotion = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Shrink Potion",
    description: "A magical potion that makes you smaller",
    type: "potion",
    rarity: "uncommon",
    icon: "🧪",
    maxUses: 3,
    cooldown: 6000, // 6 seconds
    shrinkFactor: 0.7, // Scale factor (70% of original size)
    duration: 12000 // 12 seconds
  },

  /**
   * Initialize the shrink potion
   * @param {Object} player - The player using the potion
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("ShrinkPotion: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.originalScale = null;
    this.shrinkStartTime = 0;
    this.isShrunk = false;
    
    return true;
  },

  /**
   * Use the shrink potion
   * @param {Object} target - Target object (optional, can be used on self)
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      Tootsville.Gossip.Parrot.say(
        "Shrink Potion",
        "The potion is still brewing! Please wait a moment."
      );
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      Tootsville.Gossip.Parrot.say(
        "Shrink Potion",
        "The potion bottle is empty!"
      );
      return false;
    }
    
    // Check if already shrunk
    if (this.isShrunk) {
      Tootsville.Gossip.Parrot.say(
        "Shrink Potion",
        "You are already under the effects of a shrink potion!"
      );
      return false;
    }
    
    // Determine target (default to self if no target specified)
    const targetPlayer = target || this.player;
    
    if (!targetPlayer || !targetPlayer.avatar) {
      Tootsville.Gossip.Parrot.say(
        "Shrink Potion",
        "No valid target found for the potion."
      );
      return false;
    }
    
    // Apply shrink effect
    const success = this.applyShrinkEffect(targetPlayer);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPlayer);
      
      Tootsville.Gossip.Parrot.say(
        "Shrink Potion",
        `You used a Shrink Potion! ${targetPlayer === this.player ? 'You' : targetPlayer.name} became smaller!`
      );
    }
    
    return success;
  },

  /**
   * Apply the shrink effect to a player
   * @param {Object} targetPlayer - The player to apply the effect to
   * @returns {boolean} Success status
   */
  applyShrinkEffect: function(targetPlayer) {
    try {
      if (!targetPlayer.avatar || !targetPlayer.avatar.scaling) {
        return false;
      }
      
      // Store original scale
      this.originalScale = {
        x: targetPlayer.avatar.scaling.x,
        y: targetPlayer.avatar.scaling.y,
        z: targetPlayer.avatar.scaling.z
      };
      
      // Apply shrink effect
      const newScale = this.metadata.shrinkFactor;
      targetPlayer.avatar.scaling.set(newScale, newScale, newScale);
      
      // Mark as shrunk
      this.isShrunk = true;
      this.shrinkStartTime = Date.now();
      
      // Create shrink visual effect
      this.createShrinkVisual(targetPlayer);
      
      // Schedule return to normal size
      setTimeout(() => {
        this.removeShrinkEffect(targetPlayer);
        Tootsville.Gossip.Parrot.say(
          "Shrink Potion",
          "You have returned to your normal size."
        );
      }, this.metadata.duration);
      
      return true;
    } catch (error) {
      console.error("ShrinkPotion: Error applying shrink effect:", error);
      return false;
    }
  },

  /**
   * Create visual shrink effect
   * @param {Object} targetPlayer - The player to create the effect for
   */
  createShrinkVisual: function(targetPlayer) {
    if (!targetPlayer.avatar || !targetPlayer.avatar.position) {
      return;
    }
    
    const position = targetPlayer.avatar.position;
    
    // Create shrink sparkle effect
    const sparkleGeometry = new THREE.SphereGeometry(0.1, 8, 6);
    const sparkleMaterial = new THREE.MeshBasicMaterial({
      color: 0x00ffff,
      transparent: true,
      opacity: 0.8
    });
    
    this.shrinkSparkles = [];
    
    // Create multiple sparkles around the player
    for (let i = 0; i < 8; i++) {
      const sparkle = new THREE.Mesh(sparkleGeometry, sparkleMaterial);
      const angle = (i / 8) * Math.PI * 2;
      const radius = 2;
      
      sparkle.position.set(
        position.x + Math.cos(angle) * radius,
        position.y + 1 + Math.sin(i) * 0.5,
        position.z + Math.sin(angle) * radius
      );
      
      this.shrinkSparkles.push(sparkle);
      
      if (Tootsville.Tank && Tootsville.Tank.scene) {
        Tootsville.Tank.scene.add(sparkle);
      }
    }
    
    // Animate sparkles
    this.animateShrinkSparkles();
  },

  /**
   * Animate the shrink sparkles
   */
  animateShrinkSparkles: function() {
    if (!this.shrinkSparkles || this.shrinkSparkles.length === 0) {
      return;
    }
    
    const animate = () => {
      if (!this.shrinkSparkles) {
        return; // Sparkles were removed
      }
      
      const time = Date.now() * 0.001;
      
      this.shrinkSparkles.forEach((sparkle, index) => {
        // Rotate and pulse sparkles
        sparkle.rotation.x += 0.1;
        sparkle.rotation.y += 0.1;
        sparkle.material.opacity = 0.4 + Math.sin(time * 4 + index) * 0.3;
        
        // Float up and down
        sparkle.position.y += Math.sin(time * 2 + index) * 0.01;
      });
      
      requestAnimationFrame(animate);
    };
    
    animate();
  },

  /**
   * Remove the shrink effect
   * @param {Object} targetPlayer - The player to remove the effect from
   */
  removeShrinkEffect: function(targetPlayer) {
    if (!targetPlayer.avatar || !this.originalScale) {
      return;
    }
    
    // Restore original scale
    targetPlayer.avatar.scaling.set(
      this.originalScale.x,
      this.originalScale.y,
      this.originalScale.z
    );
    
    // Remove sparkles
    this.removeShrinkSparkles();
    
    // Reset state
    this.isShrunk = false;
    this.originalScale = null;
    this.shrinkStartTime = 0;
  },

  /**
   * Remove shrink sparkles
   */
  removeShrinkSparkles: function() {
    if (this.shrinkSparkles) {
      this.shrinkSparkles.forEach(sparkle => {
        if (Tootsville.Tank && Tootsville.Tank.scene) {
          Tootsville.Tank.scene.remove(sparkle);
        }
        sparkle.geometry.dispose();
        sparkle.material.dispose();
      });
      this.shrinkSparkles = null;
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
   * Broadcast the shrink effect to other players
   * @param {Object} targetPlayer - The player who used the shrink potion
   */
  broadcastEffect: function(targetPlayer) {
    if (!Tootsville.Gossip || !Tootsville.Gossip.Network) {
      return;
    }
    
    const effectData = {
      type: 'shrinkPotion',
      playerId: targetPlayer.id || targetPlayer.uuid,
      position: targetPlayer.avatar ? targetPlayer.avatar.position : null,
      duration: this.metadata.duration,
      shrinkFactor: this.metadata.shrinkFactor,
      timestamp: Date.now()
    };
    
    Tootsville.Gossip.Network.broadcast('equipment_effect', effectData);
  },

  /**
   * Handle shrink effect from other players
   * @param {Object} effectData - The effect data received
   */
  handleRemoteEffect: function(effectData) {
    if (effectData.type !== 'shrinkPotion') {
      return;
    }
    
    // Find the target player
    const targetPlayer = Tootsville.Gossip.Network.findPlayer(effectData.playerId);
    
    if (targetPlayer && targetPlayer.avatar) {
      // Apply shrink effect to remote player
      this.applyShrinkEffect(targetPlayer);
      
      // Remove after duration
      setTimeout(() => {
        this.removeShrinkEffect(targetPlayer);
      }, effectData.duration);
    }
  },

  /**
   * Get equipment status information
   * @returns {Object} Status information
   */
  getStatus: function() {
    const now = Date.now();
    const cooldownRemaining = Math.max(0, this.metadata.cooldown - (now - this.lastUsed));
    const shrinkTimeRemaining = this.isShrunk ? 
      Math.max(0, this.metadata.duration - (now - this.shrinkStartTime)) : 0;
    
    return {
      name: this.metadata.name,
      usesRemaining: this.usesRemaining,
      maxUses: this.metadata.maxUses,
      cooldownRemaining: cooldownRemaining,
      isShrunk: this.isShrunk,
      shrinkTimeRemaining: shrinkTimeRemaining,
      canUse: this.usesRemaining > 0 && cooldownRemaining === 0 && !this.isShrunk
    };
  },

  /**
   * Clean up resources when equipment is removed
   */
  cleanup: function() {
    if (this.isShrunk && this.player) {
      this.removeShrinkEffect(this.player);
    }
    this.removeShrinkSparkles();
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
    this.isShrunk = false;
  }
};

// Register with equipment system
if (Tootsville.Game && Tootsville.Game.Equipment) {
  Tootsville.Game.Equipment.register('ShrinkPotion', Tootsville.Game.Equipment.ShrinkPotion);
}
