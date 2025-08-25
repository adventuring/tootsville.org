/**
 * PacketHandler.js - Robust Communications Packet Handling
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Implements "strict in what they produce (send), very relaxed about what they accept (input)"
 * principle for all communications packets in the Tootsville game.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { EventEmitter } from 'events';

/**
 * Packet validation and handling system
 * 
 * This system ensures that:
 * - Outgoing packets are strictly validated and formatted
 * - Incoming packets are handled gracefully even if malformed
 * - Unknown fields are ignored rather than causing errors
 * - Backward compatibility is maintained
 */
class PacketHandler extends EventEmitter {
  constructor() {
    super();
    
    // Packet type definitions with validation schemas
    this.packetSchemas = {
      // Command packets (outgoing)
      command: {
        required: ['c'],
        optional: ['d', 'r', 'a', 'u', 's', 'v'],
        strict: true // Strict validation for outgoing commands
      },
      
      // Gatekeeper packets (incoming)
      gatekeeper: {
        required: ['from', 'status'],
        optional: ['error', 'err', 'command', 'msg', 'err2'],
        strict: false // Relaxed validation for incoming packets
      },
      
      // Login packets
      logOK: {
        required: ['_cmd'],
        optional: ['status', 'neighbor', 'greet', 'auth', 'motd'],
        strict: false
      },
      
      // Avatar packets
      avatars: {
        required: ['from', 'status'],
        optional: ['avatars', 'inRoom', 'joined', 'left'],
        strict: false
      },
      
      // Movement packets
      wtl: {
        required: ['n', 'u'],
        optional: ['course', 'facing', 'status'],
        strict: false
      },
      
      // Chat packets
      publicMessage: {
        required: ['from', 'status'],
        optional: ['message', 'author', 'world', 'lat', 'long', 'alt'],
        strict: false
      },
      
      // Inventory packets
      inventory: {
        required: ['from', 'status'],
        optional: ['items', 'equipped', 'capacity'],
        strict: false
      },
      
      // Error packets
      error: {
        required: ['from', 'status'],
        optional: ['error', 'err', 'command', 'details'],
        strict: false
      }
    };
    
    // Packet handlers for different types
    this.packetHandlers = new Map();
    
    // Error tracking
    this.errorCount = 0;
    this.lastError = null;
  }

  /**
   * Validate and format outgoing packet (strict)
   * @param {string} type - Packet type
   * @param {Object} data - Packet data
   * @returns {Object} Validated and formatted packet
   */
  validateOutgoingPacket(type, data) {
    const schema = this.packetSchemas[type];
    if (!schema) {
      console.warn(`Unknown packet type: ${type}`);
      // For strict validation, throw error for unknown types
      throw new Error(`Packet validation failed: Unknown packet type: ${type}`);
    }

    const validated = {};
    const errors = [];

    // Check required fields
    for (const field of schema.required) {
      if (!(field in data)) {
        errors.push(`Missing required field: ${field}`);
      } else {
        validated[field] = data[field];
      }
    }

    // Add optional fields if present
    for (const field of schema.optional) {
      if (field in data) {
        validated[field] = data[field];
      }
    }

    // For strict validation, reject packets with missing required fields
    if (schema.strict && errors.length > 0) {
      const errorMsg = `Packet validation failed: ${errors.join(', ')}`;
      console.error(errorMsg, { type, data, validated });
      throw new Error(errorMsg);
    }

    // Log validation warnings for non-strict packets
    if (errors.length > 0) {
      console.warn(`Packet validation warnings: ${errors.join(', ')}`, { type, data });
    }

    return validated;
  }

  /**
   * Handle incoming packet (relaxed)
   * @param {Object} packet - Raw packet data
   * @returns {Object} Processed packet with any missing fields handled gracefully
   */
  handleIncomingPacket(packet) {
    if (!packet || typeof packet !== 'object') {
      console.warn('Received invalid packet (not an object):', packet);
      return { from: 'unknown', status: false, error: 'Invalid packet format' };
    }

    try {
      // Determine packet type
      const packetType = this.determinePacketType(packet);
      const schema = this.packetSchemas[packetType];

      if (!schema) {
        console.warn(`Unknown packet type: ${packetType}`, packet);
        return this.handleUnknownPacket(packet);
      }

      // Process packet with relaxed validation
      const processed = this.processIncomingPacket(packet, schema);
      
      // Emit event for packet handling
      this.emit('packetReceived', { type: packetType, data: processed, original: packet });
      
      return processed;

    } catch (error) {
      this.handlePacketError(error, packet);
      return this.createErrorPacket(packet, error);
    }
  }

  /**
   * Determine packet type based on content
   * @param {Object} packet - Packet data
   * @returns {string} Packet type
   */
  determinePacketType(packet) {
    // Check for logOK packets (special case)
    if (packet._cmd === 'logOK') {
      return 'logOK';
    }

    // Check for command packets
    if (packet.c) {
      return 'command';
    }

    // Check for gatekeeper packets
    if (packet.from) {
      // Try to determine specific type based on 'from' field
      const fromType = packet.from;
      
      // Map common 'from' values to packet types
      const fromTypeMap = {
        'avatars': 'avatars',
        'publicMessage': 'publicMessage',
        'inventory': 'inventory',
        'error': 'error',
        'c': 'error' // Command errors
      };

      return fromTypeMap[fromType] || 'gatekeeper';
    }

    // Default to gatekeeper for unknown types
    return 'gatekeeper';
  }

  /**
   * Process incoming packet with relaxed validation
   * @param {Object} packet - Raw packet
   * @param {Object} schema - Packet schema
   * @returns {Object} Processed packet
   */
  processIncomingPacket(packet, schema) {
    const processed = {};

    // Always include required fields, with defaults if missing
    for (const field of schema.required) {
      if (field in packet) {
        processed[field] = packet[field];
      } else {
        // Provide sensible defaults for missing required fields
        processed[field] = this.getDefaultValue(field);
        console.warn(`Missing required field '${field}', using default value`);
      }
    }

    // Include optional fields if present
    for (const field of schema.optional) {
      if (field in packet) {
        processed[field] = packet[field];
      }
    }

    // Include any additional fields (for backward compatibility)
    for (const [key, value] of Object.entries(packet)) {
      if (!(key in processed)) {
        processed[key] = value;
      }
    }

    return processed;
  }

  /**
   * Get default value for a field
   * @param {string} field - Field name
   * @returns {any} Default value
   */
  getDefaultValue(field) {
    const defaults = {
      'from': 'unknown',
      'status': false,
      'error': 'Missing required field',
      '_cmd': 'unknown',
      'c': 'unknown',
      'n': '',
      'u': '',
      'avatars': {},
      'inRoom': '@Tootsville',
      'message': '',
      'author': '',
      'items': [],
      'equipped': {},
      'capacity': 0
    };

    return defaults[field] !== undefined ? defaults[field] : null;
  }

  /**
   * Handle unknown packet types
   * @param {Object} packet - Unknown packet
   * @returns {Object} Processed packet
   */
  handleUnknownPacket(packet) {
    console.warn('Handling unknown packet type:', packet);
    
    // Try to extract useful information
    const processed = {
      from: packet.from || 'unknown',
      status: packet.status || false,
      error: 'Unknown packet type',
      originalData: packet
    };

    // Include any recognizable fields
    if (packet.c) processed.command = packet.c;
    if (packet.d) processed.data = packet.d;
    if (packet.error) processed.error = packet.error;

    return processed;
  }

  /**
   * Handle packet processing errors
   * @param {Error} error - Error object
   * @param {Object} packet - Original packet
   */
  handlePacketError(error, packet) {
    this.errorCount++;
    this.lastError = {
      error: error.message,
      packet: packet,
      timestamp: Date.now()
    };

    console.error('Packet processing error:', error.message, {
      packet: packet,
      errorCount: this.errorCount,
      lastError: this.lastError
    });

    // Emit error event
    this.emit('packetError', {
      error: error.message,
      packet: packet,
      errorCount: this.errorCount
    });
  }

  /**
   * Create error packet for failed processing
   * @param {Object} originalPacket - Original packet that failed
   * @param {Error} error - Error that occurred
   * @returns {Object} Error packet
   */
  createErrorPacket(originalPacket, error) {
    return {
      from: 'error',
      status: false,
      error: error.message,
      originalPacket: originalPacket,
      timestamp: Date.now()
    };
  }

  /**
   * Register packet handler for specific type
   * @param {string} type - Packet type
   * @param {Function} handler - Handler function
   */
  registerHandler(type, handler) {
    if (typeof handler !== 'function') {
      throw new Error(`Handler for packet type '${type}' must be a function`);
    }

    this.packetHandlers.set(type, handler);
    console.log(`Registered handler for packet type: ${type}`);
  }

  /**
   * Get packet handler for type
   * @param {string} type - Packet type
   * @returns {Function|null} Handler function or null
   */
  getHandler(type) {
    return this.packetHandlers.get(type) || null;
  }

  /**
   * Create outgoing packet with strict validation
   * @param {string} type - Packet type
   * @param {Object} data - Packet data
   * @returns {Object} Validated packet
   */
  createOutgoingPacket(type, data) {
    const validated = this.validateOutgoingPacket(type, data);
    
    // Add metadata
    validated._timestamp = Date.now();
    validated._version = '1.0';
    
    // Emit event for outgoing packet
    this.emit('packetSent', { type, data: validated });
    
    return validated;
  }

  /**
   * Get error statistics
   * @returns {Object} Error statistics
   */
  getErrorStats() {
    return {
      errorCount: this.errorCount,
      lastError: this.lastError,
      hasErrors: this.errorCount > 0
    };
  }

  /**
   * Reset error tracking
   */
  resetErrorTracking() {
    this.errorCount = 0;
    this.lastError = null;
    console.log('Packet error tracking reset');
  }

  /**
   * Validate packet schema
   * @param {Object} schema - Schema to validate
   * @returns {boolean} True if valid
   */
  validateSchema(schema) {
    if (!schema || typeof schema !== 'object') {
      return false;
    }

    if (!Array.isArray(schema.required) || !Array.isArray(schema.optional)) {
      return false;
    }

    return true;
  }

  /**
   * Add new packet schema
   * @param {string} type - Packet type
   * @param {Object} schema - Schema definition
   */
  addSchema(type, schema) {
    if (!this.validateSchema(schema)) {
      throw new Error(`Invalid schema for packet type: ${type}`);
    }

    this.packetSchemas[type] = schema;
    console.log(`Added schema for packet type: ${type}`);
  }
}

export default PacketHandler;

