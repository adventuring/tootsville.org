/**
 * ShrinkToHalfSize.js - Shrink To Half Size equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that makes the player character exactly half their original size.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.Game.Equipment.ShrinkToHalfSize = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Shrink To Half Size",
    description: "A powerful shrinking device that makes you exactly half size",
    type: "magical",
    rarity: "rare",
    icon: "🔬",
    maxUses: 2,
    cooldown: 10000, // 10 seconds
    shrinkFactor: 0.5, // Exactly half size (50% of original)
    duration: 20000 // 20 seconds
  },

  /**
   * Initialize the shrink to half size device
   * @param {Object} player - The player using the equipment
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("ShrinkToHalfSize: Invalid player or missing avatar");
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
   * Use the shrink to half size device
   * @param {Object} target - Target object (optional, can be used on self)
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      Tootsville.Gossip.Parrot.say(
        "Shrink To Half Size",
        "The shrinking device is recharging! Please wait a moment."
      );
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      Tootsville.Gossip.Parrot.say(
        "Shrink To Half Size",
        "The shrinking device is depleted!"
      );
      return false;
    }
    
    // Check if already shrunk
    if (this.isShrunk) {
      Tootsville.Gossip.Parrot.say(
        "Shrink To Half Size",
        "You are already under the effects of the shrinking device!"
      );
      return false;
    }
    
    // Determine target (default to self if no target specified)
    const targetPlayer = target || this.player;
    
    if (!targetPlayer || !targetPlayer.avatar) {
      Tootsville.Gossip.Parrot.say(
        "Shrink To Half Size",
        "No valid target found for the shrinking device."
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
        "Shrink To Half Size",
        `You used the Shrink To Half Size device! ${targetPlayer === this.player ? 'You' : targetPlayer.name} became exactly half size!`
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
      
      // Apply shrink effect (exactly half size)
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
          "Shrink To Half Size",
          "You have returned to your normal size."
        );
      }, this.metadata.duration);
      
      return true;
    } catch (error) {
      console.error("ShrinkToHalfSize: Error applying shrink effect:", error);
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
    
    // Create shrinking field effect
    const fieldGeometry = new THREE.CylinderGeometry(3, 3, 0.2, 16);
    const fieldMaterial = new THREE.MeshBasicMaterial({
      color: 0xff00ff,
      transparent: true,
      opacity: 0.4,
      side: THREE.DoubleSide
    });
    
    this.shrinkField = new THREE.Mesh(fieldGeometry, fieldMaterial);
    this.shrinkField.position.set(position.x, 0.1, position.z);
    this.shrinkField.rotation.x = -Math.PI / 2;
    
    if (Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.add(this.shrinkField);
    }
    
    // Create shrinking particles
    this.shrinkParticles = [];
    const particleGeometry = new THREE.SphereGeometry(0.05, 6, 4);
    const particleMaterial = new THREE.MeshBasicMaterial({
      color: 0xff00ff,
      transparent: true,
      opacity: 0.8
    });
    
    // Create particles in a shrinking spiral pattern
    for (let i = 0; i < 12; i++) {
      const particle = new THREE.Mesh(particleGeometry, particleMaterial);
      const angle = (i / 12) * Math.PI * 2;
      const radius = 3 - (i * 0.2); // Decreasing radius for spiral effect
      
      particle.position.set(
        position.x + Math.cos(angle) * radius,
        position.y + 0.5 + (i * 0.1),
        position.z + Math.sin(angle) * radius
      );
      
      this.shrinkParticles.push(particle);
      
      if (Tootsville.Tank && Tootsville.Tank.scene) {
        Tootsville.Tank.scene.add(particle);
      }
    }
    
    // Animate the shrinking effect
    this.animateShrinkEffect();
  },

  /**
   * Animate the shrinking effect
   */
  animateShrinkEffect: function() {
    if (!this.shrinkField || !this.shrinkParticles) {
      return;
    }
    
    const animate = () => {
      if (!this.shrinkField) {
        return; // Effect was removed
      }
      
      const time = Date.now() * 0.001;
      
      // Animate shrinking field
      this.shrinkField.material.opacity = 0.2 + Math.sin(time * 2) * 0.2;
      this.shrinkField.rotation.z += 0.02;
      
      // Animate particles in shrinking spiral
      this.shrinkParticles.forEach((particle, index) => {
        const particleTime = time + index * 0.5;
        const radius = 3 - (index * 0.2) - Math.sin(particleTime) * 0.5;
        const angle = (index / 12) * Math.PI * 2 + particleTime;
        
        particle.position.x = this.player.avatar.position.x + Math.cos(angle) * radius;
        particle.position.z = this.player.avatar.position.z + Math.sin(angle) * radius;
        particle.position.y = this.player.avatar.position.y + 0.5 + Math.sin(particleTime * 2) * 0.3;
        
        particle.material.opacity = 0.6 + Math.sin(particleTime * 3) * 0.3;
        particle.rotation.x += 0.1;
        particle.rotation.y += 0.1;
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
    
    // Remove visual effects
    this.removeShrinkVisual();
    
    // Reset state
    this.isShrunk = false;
    this.originalScale = null;
    this.shrinkStartTime = 0;
  },

  /**
   * Remove shrink visual effects
   */
  removeShrinkVisual: function() {
    // Remove shrinking field
    if (this.shrinkField && Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.remove(this.shrinkField);
      this.shrinkField.geometry.dispose();
      this.shrinkField.material.dispose();
      this.shrinkField = null;
    }
    
    // Remove particles
    if (this.shrinkParticles) {
      this.shrinkParticles.forEach(particle => {
        if (Tootsville.Tank && Tootsville.Tank.scene) {
          Tootsville.Tank.scene.remove(particle);
        }
        particle.geometry.dispose();
        particle.material.dispose();
      });
      this.shrinkParticles = null;
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
   * @param {Object} targetPlayer - The player who used the shrinking device
   */
  broadcastEffect: function(targetPlayer) {
    if (!Tootsville.Gossip || !Tootsville.Gossip.Network) {
      return;
    }
    
    const effectData = {
      type: 'shrinkToHalfSize',
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
    if (effectData.type !== 'shrinkToHalfSize') {
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
    this.removeShrinkVisual();
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
    this.isShrunk = false;
  }
};

// Register with equipment system
if (Tootsville.Game && Tootsville.Game.Equipment) {
  Tootsville.Game.Equipment.register('ShrinkToHalfSize', Tootsville.Game.Equipment.ShrinkToHalfSize);
}
