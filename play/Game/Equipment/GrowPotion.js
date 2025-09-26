/**
 * GrowPotion.js - Grow Potion equipment item
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Equipment that makes the player character larger when used.
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


Tootsville.Game.Equipment.GrowPotion = {
  
  /**
   * Equipment metadata
   */
  metadata: {
    name: "Grow Potion",
    description: "A magical potion that makes you larger",
    type: "potion",
    rarity: "common",
    icon: "🧪",
    maxUses: 3,
    cooldown: 5000 // 5 seconds
  },

  /**
   * Initialize the grow potion
   * @param {Object} player - The player using the potion
   * @returns {boolean} Success status
   */
  initialize: function(player) {
    if (!player || !player.avatar) {
      console.error("GrowPotion: Invalid player or missing avatar");
      return false;
    }
    
    this.player = player;
    this.lastUsed = 0;
    this.usesRemaining = this.metadata.maxUses;
    
    return true;
  },

  /**
   * Use the grow potion
   * @param {Object} target - Target object (optional, can be used on self)
   * @returns {boolean} Success status
   */
  use: function(target = null) {
    const now = Date.now();
    
    // Check cooldown
    if (now - this.lastUsed < this.metadata.cooldown) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'grow_potion',
          'cooldown',
          'The potion is still brewing! Please wait a moment.'
        );
      }
      return false;
    }
    
    // Check uses remaining
    if (this.usesRemaining <= 0) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'grow_potion',
          'empty',
          'The potion bottle is empty!'
        );
      }
      return false;
    }
    
    // Determine target (default to self if no target specified)
    const targetPlayer = target || this.player;
    
    if (!targetPlayer || !targetPlayer.avatar) {
      // Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          'grow_potion',
          'error',
          'No valid target found for the potion.'
        );
      }
      return false;
    }
    
    // Apply growth effect
    const success = this.applyGrowthEffect(targetPlayer);
    
    if (success) {
      this.lastUsed = now;
      this.usesRemaining--;
      
      // Update equipment display
      this.updateEquipmentDisplay();
      
      // Send effect to other players
      this.broadcastEffect(targetPlayer);
      
      // Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          'grow_potion',
          'grow',
          targetPlayer,
          { id: 'system', name: 'Grow Potion' },
          `${targetPlayer.name} grew larger!`
        );
      }
    }
    
    return success;
  },

  /**
   * Apply the growth effect to a player
   * @param {Object} player - The player to affect
   * @returns {boolean} Success status
   */
  applyGrowthEffect: function(player) {
    try {
      const avatar = player.avatar;
      const currentScale = avatar.scale || { x: 1, y: 1, z: 1 };
      
      // Increase size by 20%
      const growthFactor = 1.2;
      const newScale = {
        x: currentScale.x * growthFactor,
        y: currentScale.y * growthFactor,
        z: currentScale.z * growthFactor
      };
      
      // Apply new scale
      avatar.scale.set(newScale.x, newScale.y, newScale.z);
      
      // Update player state
      player.effects = player.effects || {};
      player.effects.grown = {
        timestamp: Date.now(),
        duration: 30000, // 30 seconds
        originalScale: currentScale,
        newScale: newScale
      };
      
      // Schedule effect removal
      setTimeout(() => {
        this.removeGrowthEffect(player);
      }, 30000);
      
      return true;
      
    } catch (error) {
      console.error("GrowPotion: Error applying growth effect:", error);
      return false;
    }
  },

  /**
   * Remove the growth effect from a player
   * @param {Object} player - The player to affect
   */
  removeGrowthEffect: function(player) {
    try {
      if (player.effects && player.effects.grown) {
        const avatar = player.avatar;
        const originalScale = player.effects.grown.originalScale;
        
        // Restore original scale
        avatar.scale.set(originalScale.x, originalScale.y, originalScale.z);
        
        // Remove effect
        delete player.effects.grown;
        
        // Broadcast effect removal
        this.broadcastEffectRemoval(player);
        
        // Send proper equipment effect notification
        if (Tootsville.Game && Tootsville.Game.EventNotifications) {
          Tootsville.Game.EventNotifications.equipmentEffect(
            'grow_potion',
            'grow',
            player,
            { id: 'system', name: 'Grow Potion' },
            `${player.name} returned to normal size.`
          );
        }
      }
    } catch (error) {
      console.error("GrowPotion: Error removing growth effect:", error);
    }
  },

  /**
   * Update the equipment display
   */
  updateEquipmentDisplay: function() {
    // Update UI to show remaining uses
    const equipmentIcon = document.querySelector('.equipment-icon[data-item="grow-potion"]');
    if (equipmentIcon) {
      const usesDisplay = equipmentIcon.querySelector('.uses-display');
      if (usesDisplay) {
        usesDisplay.textContent = `${this.usesRemaining}/${this.metadata.maxUses}`;
      }
      
      // Disable if no uses remaining
      if (this.usesRemaining <= 0) {
        equipmentIcon.classList.add('disabled');
      }
    }
  },

  /**
   * Broadcast the effect to other players
   * @param {Object} targetPlayer - The player affected
   */
  broadcastEffect: function(targetPlayer) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect',
        equipment: 'grow_potion',
        targetPlayerId: targetPlayer.id,
        effect: 'growth',
        timestamp: Date.now()
      });
    }
  },

  /**
   * Broadcast effect removal to other players
   * @param {Object} targetPlayer - The player affected
   */
  broadcastEffectRemoval: function(targetPlayer) {
    if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
      Tootsville.Gossip.sendMessage({
        type: 'equipment_effect_removal',
        equipment: 'grow_potion',
        targetPlayerId: targetPlayer.id,
        effect: 'growth',
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
    // Remove any active effects
    if (this.player && this.player.effects && this.player.effects.grown) {
      this.removeGrowthEffect(this.player);
    }
    
    this.player = null;
    this.lastUsed = 0;
    this.usesRemaining = 0;
  }
};
