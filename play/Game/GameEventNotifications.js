/* -*- js2 -*-*/

/**@license
 *
 * play/Game/GameEventNotifications.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2021   The
 * Interworldly Adventuring, LLC of Portland, OR, USA.
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */

/**
 * Game Event Notification System
 *
 * Replaces inappropriate usage of Gossip Parrots for normal game events.
 * Gossip Parrots should only be used for serious networking problems.
 *
 * This system provides proper channels for:
 * - Equipment effects
 * - Player interactions
 * - Game state changes
 * - NPC communications
 * - World events
 */

if (!('Game' in Tootsville)) { Tootsville.Game = {}; }
if (!('EventNotifications' in Tootsville.Game)) { Tootsville.Game.EventNotifications = {}; }

/**
 * Send a game event notification using Infinity Mode protocol format
 * @param {Object} eventConfig - Event configuration
 * @param {string} eventConfig.eventType - Type of game event
 * @param {Object} eventConfig.eventData - Event data
 * @param {string} eventConfig.category - Event category (equipment|player|system|npc|world)
 * @param {string} eventConfig.priority - Event priority (normal|high|low)
 * @param {Object} eventConfig.target - Target information
 * @param {Object} eventConfig.metadata - Additional metadata
 */
Tootsville.Game.EventNotifications.send = function(eventConfig) {
  const {
    eventType,
    eventData,
    category = 'system',
    priority = 'normal',
    target = { type: 'all' },
    metadata = {}
  } = eventConfig;

  // Create RFC-compliant message format
  const message = {
    type: 'game-event',
    eventId: 'event_' + Date.now() + '_' + Math.random().toString(36).substr(2, 9),
    timestamp: new Date().toISOString(),
    priority: priority,
    category: category,
    target: target,
    event: {
      type: this.mapEventTypeToProtocol(eventType),
      subtype: eventType,
      data: eventData
    },
    metadata: {
      source: 'client',
      persistent: false,
      ttl: 30000,
      ...metadata
    }
  };

  // Send through Infinity Mode WebSocket channels (preferred)
  if (Tootsville.InfinityMode && Tootsville.InfinityMode.sendGameMessage) {
    Tootsville.InfinityMode.sendGameMessage(message);
  }
  // Fallback to REST API
  else if (Tootsville.InfinityMode && Tootsville.InfinityMode.sendRESTMessage) {
    Tootsville.InfinityMode.sendRESTMessage('game-events', message);
  }
  // Legacy fallback (should be removed in future versions)
  else if (Tootsville.Gossip && Tootsville.Gossip.sendMessage) {
    // Only use for serious networking problems, not game events
    console.warn('Using legacy Gossip system for game events - this should be updated to Infinity Mode');
    Tootsville.Gossip.sendMessage({
      type: 'network_warning',
      message: 'Game event system using legacy protocol',
      severity: 'low'
    });
  }

  // Add to local notification queue for UI display
  this.addToNotificationQueue(message);

  // Trigger local event handlers
  this.triggerEventHandlers(eventType, message);
};

/**
 * Map event types to protocol-compliant format
 * @param {string} eventType - Original event type
 * @returns {string} Protocol event type
 */
Tootsville.Game.EventNotifications.mapEventTypeToProtocol = function(eventType) {
  const typeMapping = {
    // Equipment events
    'equipment_effect': 'effect',
    'equipment_status': 'status',

    // Player events
    'player_interaction': 'interaction',
    'player_move': 'movement',

    // NPC events
    'npc_interaction': 'interaction',

    // World events
    'world_event': 'notification',
    'scene_update': 'notification',

    // System events
    'system_notification': 'notification',
    'game_state_change': 'notification',

    // Default
    'default': 'notification'
  };

  return typeMapping[eventType] || typeMapping.default;
};

/**
 * Add notification to local queue for UI display
 * @param {Object} notification - Notification object (RFC-compliant format)
 */
Tootsville.Game.EventNotifications.addToNotificationQueue = function(notification) {
  if (!this.notificationQueue) {
    this.notificationQueue = [];
  }

  this.notificationQueue.push(notification);

  // Keep queue size manageable
  if (this.notificationQueue.length > 50) {
    this.notificationQueue.shift();
  }

  // Update UI if available
  this.updateUI(notification);
};

/**
 * Update UI with notification
 * @param {Object} notification - Notification object (RFC-compliant format)
 */
Tootsville.Game.EventNotifications.updateUI = function(notification) {
  // Extract message from RFC-compliant format
  const message = this.formatNotificationMessage(notification);

  // Try Talk Box first
  if (Tootsville.UI && Tootsville.UI.TalkBox && Tootsville.UI.TalkBox.showNotification) {
    Tootsville.UI.TalkBox.showNotification(message, notification.priority);
    return;
  }

  // Try HUD notifications
  if (Tootsville.UI && Tootsville.UI.HUD && Tootsville.UI.HUD.showNotification) {
    Tootsville.UI.HUD.showNotification({
      message: message,
      type: notification.category,
      priority: notification.priority,
      data: notification
    });
    return;
  }

  // Try game event overlay
  if (Tootsville.UI && Tootsville.UI.GameEventOverlay) {
    Tootsville.UI.GameEventOverlay.show(notification);
    return;
  }

  // Fallback: console log (for debugging)
  console.log('Game Event:', message, notification);
};

/**
 * Format notification message for display from RFC-compliant format
 * @param {Object} notification - Notification object
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatNotificationMessage = function(notification) {
  const { category, event } = notification;

  switch (category) {
    case 'equipment':
      return this.formatEquipmentMessage(event);

    case 'player':
      return this.formatPlayerMessage(event);

    case 'system':
      return this.formatSystemMessage(event);

    case 'npc':
      return this.formatNPCMessage(event);

    case 'world':
      return this.formatWorldMessage(event);

    default:
      return event.data?.message || `Game event: ${event.subtype}`;
  }
};

/**
 * Format equipment-related messages
 * @param {Object} event - Event data
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatEquipmentMessage = function(event) {
  const { type, subtype, data } = event;

  switch (type) {
    case 'effect':
      return data.message || this.generateEquipmentMessage(data.equipment, data.effectType, data.target, data.source);

    case 'status':
      return data.message || `${data.equipment}: ${data.statusType}`;

    default:
      return data.message || `Equipment event: ${subtype}`;
  }
};

/**
 * Format player-related messages
 * @param {Object} event - Event data
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatPlayerMessage = function(event) {
  const { type, data } = event;

  switch (type) {
    case 'interaction':
      return data.message || `${data.playerName} ${data.interactionType} ${data.targetName}`;

    case 'movement':
      return data.message || `${data.playerName} moved`;

    default:
      return data.message || `Player event: ${event.subtype}`;
  }
};

/**
 * Format system messages
 * @param {Object} event - Event data
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatSystemMessage = function(event) {
  const { data } = event;
  return data.message || data.title || `System: ${event.subtype}`;
};

/**
 * Format NPC messages
 * @param {Object} event - Event data
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatNPCMessage = function(event) {
  const { data } = event;
  return data.message || `${data.npcName}: ${data.interactionType}`;
};

/**
 * Format world messages
 * @param {Object} event - Event data
 * @returns {string} Formatted message
 */
Tootsville.Game.EventNotifications.formatWorldMessage = function(event) {
  const { data } = event;
  return data.message || `World: ${data.eventType}`;
};

/**
 * Register event handler
 * @param {string} eventType - Event type to handle
 * @param {Function} handler - Handler function
 */
Tootsville.Game.EventNotifications.on = function(eventType, handler) {
  if (!this.eventHandlers) {
    this.eventHandlers = {};
  }

  if (!this.eventHandlers[eventType]) {
    this.eventHandlers[eventType] = [];
  }

  this.eventHandlers[eventType].push(handler);
};

/**
 * Trigger event handlers for a specific event type
 * @param {string} eventType - Event type
 * @param {Object} notification - Notification object
 */
Tootsville.Game.EventNotifications.triggerEventHandlers = function(eventType, notification) {
  if (this.eventHandlers && this.eventHandlers[eventType]) {
    this.eventHandlers[eventType].forEach(handler => {
      try {
        handler(notification);
      } catch (error) {
        console.error('Error in event handler:', error);
      }
    });
  }
};

/**
 * Get recent notifications
 * @param {number} limit - Maximum number of notifications to return
 * @returns {Array} Array of recent notifications
 */
Tootsville.Game.EventNotifications.getRecentNotifications = function(limit = 10) {
  if (!this.notificationQueue) {
    return [];
  }

  return this.notificationQueue.slice(-limit);
};

/**
 * Clear notification queue
 */
Tootsville.Game.EventNotifications.clearNotifications = function() {
  this.notificationQueue = [];
};

/**
 * Equipment Effect Notification (RFC-compliant)
 * @param {string} equipmentName - Name of equipment
 * @param {string} effectType - Type of effect
 * @param {Object} target - Target object (player, NPC, scenery, etc.)
 * @param {Object} source - Source object (player using equipment)
 * @param {string} customMessage - Custom message (optional)
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.equipmentEffect = function(equipmentName, effectType, target, source, customMessage, priority = 'normal') {
  const message = customMessage || this.generateEquipmentMessage(equipmentName, effectType, target, source);

  this.send({
    eventType: 'equipment_effect',
    category: 'equipment',
    priority: priority,
    target: {
      type: target.type === 'player' ? 'user' : 'all',
      userId: target.type === 'player' ? target.id : undefined
    },
    eventData: {
      equipment: equipmentName,
      effectType: effectType,
      targetId: target.id,
      targetName: target.name,
      targetType: target.type || 'unknown',
      sourceId: source.id,
      sourceName: source.name,
      message: message
    },
    metadata: {
      equipmentType: equipmentName,
      effectType: effectType,
      sourceType: source.type || 'player'
    }
  });
};

/**
 * Generate equipment effect message
 * @param {string} equipmentName - Equipment name
 * @param {string} effectType - Effect type
 * @param {Object} target - Target
 * @param {Object} source - Source
 * @returns {string} Generated message
 */
Tootsville.Game.EventNotifications.generateEquipmentMessage = function(equipmentName, effectType, target, source) {
  const equipmentDisplayName = this.formatEquipmentName(equipmentName);
  const effectDisplayName = this.formatEffectName(effectType);

  if (source.id === target.id) {
    return `${source.name} used ${equipmentDisplayName} and ${effectDisplayName}`;
  } else {
    return `${source.name} used ${equipmentDisplayName} on ${target.name} causing ${effectDisplayName}`;
  }
};

/**
 * Format equipment name for display
 * @param {string} equipmentName - Internal equipment name
 * @returns {string} Display name
 */
Tootsville.Game.EventNotifications.formatEquipmentName = function(equipmentName) {
  const names = {
    'props_air_blaster': 'Props Air Blaster',
    'zap_wish_bolt': 'Zap Wish Bolt',
    'shrink_potion': 'Shrink Potion',
    'grow_potion': 'Grow Potion',
    'light_field': 'Light Field',
    'shadow_caster': 'Shadow Caster',
    'ka_tootel_egg_shooter': 'Ka-Tootel Egg Shooter'
  };

  return names[equipmentName] || equipmentName.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
};

/**
 * Format effect name for display
 * @param {string} effectType - Internal effect type
 * @returns {string} Display name
 */
Tootsville.Game.EventNotifications.formatEffectName = function(effectType) {
  const names = {
    'knockdown': 'was knocked down',
    'push': 'was pushed back',
    'shrink': 'shrank',
    'grow': 'grew larger',
    'stun': 'was stunned',
    'slow': 'slowed down',
    'speed': 'sped up',
    'heal': 'was healed',
    'damage': 'took damage'
  };

  return names[effectType] || effectType.replace(/_/g, ' ');
};

/**
 * Player Interaction Notification (RFC-compliant)
 * @param {Object} player - Player initiating interaction
 * @param {Object} target - Target of interaction
 * @param {string} interactionType - Type of interaction
 * @param {string} customMessage - Custom message (optional)
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.playerInteraction = function(player, target, interactionType, customMessage, priority = 'normal') {
  const message = customMessage || `${player.name} ${interactionType} ${target.name}`;

  this.send({
    eventType: 'player_interaction',
    category: 'player',
    priority: priority,
    target: {
      type: target.type === 'player' ? 'user' : 'all',
      userId: target.type === 'player' ? target.id : undefined
    },
    eventData: {
      playerId: player.id,
      playerName: player.name,
      targetId: target.id,
      targetName: target.name,
      targetType: target.type || 'unknown',
      interactionType: interactionType,
      message: message
    },
    metadata: {
      interactionType: interactionType,
      playerLevel: player.level,
      targetLevel: target.level
    }
  });
};

/**
 * World Event Notification (RFC-compliant)
 * @param {string} eventType - Type of world event
 * @param {Object} eventData - Additional event data
 * @param {string} customMessage - Custom message (optional)
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.worldEvent = function(eventType, eventData, customMessage, priority = 'normal') {
  const message = customMessage || `World event: ${eventType}`;

  this.send({
    eventType: 'world_event',
    category: 'world',
    priority: priority,
    target: { type: 'all' },
    eventData: {
      eventType: eventType,
      eventData: eventData,
      message: message
    },
    metadata: {
      worldEventType: eventType,
      affectedArea: eventData?.area || 'global',
      eventScale: eventData?.scale || 'local'
    }
  });
};

/**
 * NPC Interaction Notification (RFC-compliant)
 * @param {Object} npc - NPC involved
 * @param {Object} target - Target of interaction (player or other)
 * @param {string} interactionType - Type of interaction
 * @param {string} customMessage - Custom message (optional)
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.npcInteraction = function(npc, target, interactionType, customMessage, priority = 'normal') {
  const message = customMessage || `${npc.name}: ${interactionType}`;

  this.send({
    eventType: 'npc_interaction',
    category: 'npc',
    priority: priority,
    target: {
      type: target.type === 'player' ? 'user' : 'all',
      userId: target.type === 'player' ? target.id : undefined
    },
    eventData: {
      npcId: npc.id,
      npcName: npc.name,
      targetId: target.id,
      targetName: target.name,
      interactionType: interactionType,
      message: message
    },
    metadata: {
      npcType: npc.type || 'generic',
      interactionType: interactionType,
      npcLocation: npc.location
    }
  });
};

/**
 * System Notification (RFC-compliant)
 * @param {string} category - System category (cooldown, inventory, equipment, etc.)
 * @param {string} title - Notification title
 * @param {string} message - Notification message
 * @param {string} level - Notification level (info, warning, error)
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.systemNotification = function(category, title, message, level = 'info', priority = 'normal') {
  // Map level to priority if not specified
  if (priority === 'normal') {
    switch (level) {
      case 'error': priority = 'high'; break;
      case 'warning': priority = 'high'; break;
      case 'info': priority = 'normal'; break;
    }
  }

  this.send({
    eventType: 'system_notification',
    category: 'system',
    priority: priority,
    target: { type: 'user', userId: 'current' }, // Current user only
    eventData: {
      category: category,
      title: title,
      message: message,
      level: level
    },
    metadata: {
      systemCategory: category,
      notificationLevel: level,
      persistent: level === 'error' || level === 'warning'
    }
  });
};

/**
 * Equipment Status Notification (RFC-compliant)
 * @param {string} equipmentName - Equipment name
 * @param {string} statusType - Status type (cooldown, empty, ready, etc.)
 * @param {string} message - Status message
 * @param {string} priority - Event priority (normal|high|low)
 */
Tootsville.Game.EventNotifications.equipmentStatus = function(equipmentName, statusType, message, priority = 'normal') {
  // Higher priority for critical equipment status
  if (priority === 'normal' && (statusType === 'broken' || statusType === 'error')) {
    priority = 'high';
  }

  this.send({
    eventType: 'equipment_status',
    category: 'equipment',
    priority: priority,
    target: { type: 'user', userId: 'current' }, // Current user only
    eventData: {
      equipment: equipmentName,
      statusType: statusType,
      message: message
    },
    metadata: {
      equipmentType: equipmentName,
      statusType: statusType,
      requiresAttention: statusType === 'broken' || statusType === 'error'
    }
  });
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = Tootsville.Game.EventNotifications;
}
