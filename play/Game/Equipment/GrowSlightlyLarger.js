/**
 * GrowSlightlyLarger.js - Grow Slightly Larger equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that makes the player slightly larger for a temporary period.
 * Non-violent equipment for fun size-changing effects.
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


Tootsville.Game.Equipment.GrowSlightlyLarger = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Grow Slightly Larger",
    description: "Makes you slightly larger for a short time",
    type: "potion",
    rarity: "common",
    icon: "📏",
    maxUses: 3,
    cooldown: 5000, // 5 seconds
    duration: 30000, // 30 seconds
    growthFactor: 1.2 // 20% larger
  },

  /**
   * Initialize the grow slightly larger effect
   * @param {Object} player - The player using the equipment
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("GrowSlightlyLarger: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    this.activeEffect = null;
    this.originalScale = null;
    
    return true;
  },

  /**
   * Use the grow slightly larger effect
   * @param {Object} target - Target (unused for this equipment)
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'grow_slightly_larger',
        'grow',
        targetPlayer || player,
        { id: 'system', name: 'Grow Slightly Larger' },
        `You need to wait before growing again!`
      );
    };
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'grow_slightly_larger',
          'empty',
          "You're out of growth potions!"
        );
      }
      return false;
    }
    
    // Check if effect is already active
    if (this.activeEffect) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'grow_slightly_larger',
          'active',
          "You're already larger! Wait for the effect to wear off."
        );
      }
      return false;
    }
    
    // Apply growth effect
    const success = this.applyGrowthEffect();
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect();
      
      // Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        'grow_slightly_larger',
        'grow',
        targetPlayer || player,
        { id: 'system', name: 'Grow Slightly Larger' },
        `You feel yourself growing slightly larger!`
      );
    };
    }
    
    return success;
  },

  /**
   * Apply the growth effect to the player
   * @returns {boolean} Success status
   */
  applyGrowthEffect: function() {
    try {
      const avatar = this.player.avatar;
      
      // Store original scale
      this.originalScale = {
        x: avatar.scale.x,
        y: avatar.scale.y,
        z: avatar.scale.z
      };
      
      // Apply growth factor
      const newScale = this.originalScale.x * this.metadata.growthFactor;
      avatar.scale.set(newScale, newScale, newScale);
      
      // Create visual effect
      this.createVisualEffect();
      
      // Set up effect tracking
      this.activeEffect = {
        startTime: Date.now(),
        duration: this.metadata.duration
      };
      
      // Schedule effect removal
      setTimeout(() => {
        this.removeGrowthEffect();
      }, this.metadata.duration);
      
      return true;
      
    } catch (error) {
      console.error("GrowSlightlyLarger: Error applying growth effect:", error);
      return false;
    }
  },

  /**
   * Remove the growth effect
   */
  removeGrowthEffect: function() {
    if (!this.activeEffect || !this.originalScale) {
      return;
    }
    
    try {
      const avatar = this.player.avatar;
      
      // Restore original scale
      avatar.scale.set(
        this.originalScale.x,
        this.originalScale.y,
        this.originalScale.z
      );
      
      // Remove visual effect
      this.removeVisualEffect();
      
      // Clear effect tracking
      this.activeEffect = null;
      this.originalScale = null;
      
      // Broadcast effect removal
      this.broadcastEffectRemoval();
      
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'grow_slightly_larger',
          'grow',
          this.player,
          { id: 'system', name: 'Grow Slightly Larger' },
          `${this.player.name} returned to normal size.`
        );
      }
      
    } catch (error) {
      console.error("GrowSlightlyLarger: Error removing growth effect:", error);
    }
  },

  /**
   * Create visual effect for growth
   */
  createVisualEffect: function() {
    if (Tootsville.Scene && Tootsville.Scene.createParticleEffect) {
      const position = this.player.avatar.position;
      
      Tootsville.Scene.createParticleEffect({
        type: 'growth_sparkle',
        position: position,
        duration: 2000,
        particleCount: 30,
        color: '#ffd700', // Gold sparkles
        size: 0.3,
        spread: 2.0
      });
    }
  },

  /**
   * Remove visual effect
   */
  removeVisualEffect: function() {
    if (Tootsville.Scene && Tootsville.Scene.removeParticleEffect) {
      Tootsville.Scene.removeParticleEffect('growth_sparkle');
    }
  },

  /**
   * Update the equipment display
   */
  updateEquipmentDisplay: function() {
    const equipmentIcon = document.querySelector('.equipment-icon[data-item="grow-slightly-larger"]');
    if (equipmentIcon) {
      const usesDisplay = equipmentIcon.querySelector('.uses-display');
      if (usesDisplay) {
        usesDisplay.textContent = `${this.usesRemaining}/${this.metadata.maxUses}`;
      }
      
      if (this.usesRemaining <= 0) {
        equipmentIcon.classList.add('disabled');
      }
      
      // Show active effect indicator
      if (this.activeEffect) {
        equipmentIcon.classList.add('active');
        const timeLeft = Math.max(0, this.metadata.duration - (Date.now() - this.activeEffect.startTime));
        const timeDisplay = equipmentIcon.querySelector('.time-display');
        if (timeDisplay) {
          timeDisplay.textContent = `${Math.ceil(timeLeft / 1000)}s`;
        }
      } else {
        equipmentIcon.classList.remove('active');
      }
    }
  },

  /**
   * Broadcast the effect to other players
   */
  broadcastEffect: function() {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect',
        equipment: 'grow_slightly_larger',
        playerId: this.player.id,
        growthFactor: this.metadata.growthFactor,
        duration: this.metadata.duration,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast effect removal
   */
  broadcastEffectRemoval: function() {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect_removal',
        equipment: 'grow_slightly_larger',
        playerId: this.player.id,
        timestamp: Date.now()
      });
    }
  },

  /**
   * Get equipment info for UI display
   * @returns {Object} Equipment information
   */
  getInfo: function() {
    const isActive = !!this.activeEffect;
    const timeLeft = isActive ? 
      Math.max(0, this.metadata.duration - (Date.now() - this.activeEffect.startTime)) : 0;
    
    return {
      ...this.metadata,
      usesRemaining: this.usesRemaining,
      isActive: isActive,
      timeLeft: timeLeft,
      canUse: this.usesRemaining > 0 && 
               !isActive && 
               (Date.now() - this.lastUsed) >= this.metadata.cooldown
    };
  },

  /**
   * Clean up the equipment
   */
  cleanup: function() {
    // Remove active effect if any
    if (this.activeEffect) {
      this.removeGrowthEffect();
    }
    
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
    this.activeEffect = null;
    this.originalScale = null;
  }
};
