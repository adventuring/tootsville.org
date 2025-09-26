/**
 * LightField.js - Light Field equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that creates a field of light around the player when used.
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


Tootsville.Game.Equipment.LightField = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Light Field",
    description: "Creates a magical field of light around you",
    type: "magical",
    rarity: "rare",
    icon: "✨",
    maxUses: 5,
    cooldown: 8000, // 8 seconds
    duration: 15000, // 15 seconds
    radius: 10 // Light field radius in units
  },

  /**
   * Initialize the light field
   * @param {Object} player - The player using the equipment
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("LightField: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.activeField = null;
    this.fieldStartTime = 0;
    
    return true;
  },

  /**
   * Use the light field
   * @param {Object} target - Target object (optional, can be used on self)
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'light_field',
          'effect',
          targetPlayer || player,
          { id: 'system', name: 'Light Field' },
          `The light field is recharging! Please wait a moment.`
        );
      };
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'light_field',
          'effect',
          targetPlayer || player,
          { id: 'system', name: 'Light Field' },
          `The light field generator is depleted!`
        );
      };
      return false;
    }
    
    // Determine target (default to self if no target specified)
    const targetPlayer = target || this.player;
    
    if (!targetPlayer || !targetPlayer.avatar) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'light_field',
          'error',
          `No valid target found for the light field.`
        );
      };
      return false;
    }
    
    // Apply light field effect
    const success = this.applyLightFieldEffect(targetPlayer);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPlayer);
      
      Tootsville.Gossip.Parrot.say(
        "Light Field",
        `You activated a Light Field! ${targetPlayer === this.player ? 'You' : targetPlayer.name} are now surrounded by magical light!`
      );
    }
    
    return success;
  },

  /**
   * Apply the light field effect to a player
   * @param {Object} targetPlayer - The player to apply the effect to
   * @returns {boolean} Success status
   */
  applyLightFieldEffect: function(targetPlayer) {
    try {
      // Remove existing light field if active
      this.removeLightField();
      
      // Create light field visual effect
      this.createLightFieldVisual(targetPlayer);
      
      // Set field start time
      this.fieldStartTime = Date.now();
      
      // Schedule field removal
      setTimeout(() => {
        this.removeLightField();
        // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'light_field',
          'effect',
          targetPlayer || player,
          { id: 'system', name: 'Light Field' },
          `The light field has faded away.`
        );
      };
      }, this.metadata.duration);
      
      return true;
    } catch (error) {
      console.error("LightField: Error applying light field effect:", error);
      return false;
    }
  },

  /**
   * Create the visual light field effect
   * @param {Object} targetPlayer - The player to create the effect around
   */
  createLightFieldVisual: function(targetPlayer) {
    if (!targetPlayer.avatar || !targetPlayer.avatar.position) {
      return;
    }
    
    const position = targetPlayer.avatar.position;
    
    // Create light field mesh
    const lightFieldGeometry = new THREE.CylinderGeometry(
      this.metadata.radius, 
      this.metadata.radius, 
      0.1, 
      32
    );
    
    const lightFieldMaterial = new THREE.MeshBasicMaterial({
      color: 0xffffaa,
      transparent: true,
      opacity: 0.3,
      side: THREE.DoubleSide
    });
    
    this.activeField = new THREE.Mesh(lightFieldGeometry, lightFieldMaterial);
    this.activeField.position.set(position.x, 0.05, position.z);
    this.activeField.rotation.x = -Math.PI / 2;
    
    // Add to scene
    if (Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.add(this.activeField);
    }
    
    // Create point light
    this.fieldLight = new THREE.PointLight(0xffffaa, 0.8, this.metadata.radius * 2);
    this.fieldLight.position.set(position.x, 2, position.z);
    
    if (Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.add(this.fieldLight);
    }
    
    // Animate the light field
    this.animateLightField();
  },

  /**
   * Animate the light field effect
   */
  animateLightField: function() {
    if (!this.activeField || !this.fieldLight) {
      return;
    }
    
    const animate = () => {
      if (!this.activeField) {
        return; // Field was removed
      }
      
      const time = Date.now() * 0.001;
      
      // Pulse the light field
      this.activeField.material.opacity = 0.2 + Math.sin(time * 3) * 0.1;
      this.fieldLight.intensity = 0.6 + Math.sin(time * 2) * 0.2;
      
      // Rotate the field slowly
      this.activeField.rotation.z += 0.01;
      
      requestAnimationFrame(animate);
    };
    
    animate();
  },

  /**
   * Remove the light field effect
   */
  removeLightField: function() {
    if (this.activeField && Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.remove(this.activeField);
      this.activeField.geometry.dispose();
      this.activeField.material.dispose();
      this.activeField = null;
    }
    
    if (this.fieldLight && Tootsville.Tank && Tootsville.Tank.scene) {
      Tootsville.Tank.scene.remove(this.fieldLight);
      this.fieldLight.dispose();
      this.fieldLight = null;
    }
    
    this.fieldStartTime = 0;
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
   * Broadcast the light field effect to other players
   * @param {Object} targetPlayer - The player who used the light field
   */
  broadcastEffect: function(targetPlayer) {
    if (!Tootsville.Gossip || !Tootsville.Gossip.Network) {
      return;
    }
    
    const effectData = {
      type: 'lightField',
      playerId: targetPlayer.id || targetPlayer.uuid,
      position: targetPlayer.avatar ? targetPlayer.avatar.position : null,
      duration: this.metadata.duration,
      radius: this.metadata.radius,
      timestamp: Date.now()
    };
    
    Tootsville.Gossip.Network.broadcast('equipment_effect', effectData);
  },

  /**
   * Handle light field effect from other players
   * @param {Object} effectData - The effect data received
   */
  handleRemoteEffect: function(effectData) {
    if (effectData.type !== 'lightField') {
      return;
    }
    
    // Find the target player
    const targetPlayer = Tootsville.Gossip.Network.findPlayer(effectData.playerId);
    
    if (targetPlayer && targetPlayer.avatar) {
      // Create visual effect for remote player
      this.createLightFieldVisual(targetPlayer);
      
      // Remove after duration
      setTimeout(() => {
        this.removeLightField();
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
    const fieldTimeRemaining = this.activeField ? 
      Math.max(0, this.metadata.duration - (now - this.fieldStartTime)) : 0;
    
    return {
      name: this.metadata.name,
      usesRemaining: this.usesRemaining,
      maxUses: this.metadata.maxUses,
      cooldownRemaining: cooldownRemaining,
      isActive: !!this.activeField,
      fieldTimeRemaining: fieldTimeRemaining,
      canUse: this.usesRemaining > 0 && cooldownRemaining === 0
    };
  },

  /**
   * Clean up resources when equipment is removed
   */
  cleanup: function() {
    this.removeLightField();
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
  }
};

// Register with equipment system
if (Tootsville.Game && Tootsville.Game.Equipment) {
  Tootsville.Game.Equipment.register('LightField', Tootsville.Game.Equipment.LightField);
}
