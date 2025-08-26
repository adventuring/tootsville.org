/**
 * PacketHandler.ts - Packet validation and processing service
 * 
 * Vue 3 / TypeScript version with strict outgoing validation and relaxed incoming validation
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { EventEmitter } from 'events'

// Types
export interface Packet {
  type: string
  data: any
  timestamp?: number
  id?: string
}

export interface PacketSchema {
  type: string
  required: string[]
  optional?: string[]
  defaults?: Record<string, any>
}

export interface ValidationResult {
  valid: boolean
  errors: string[]
  warnings: string[]
}

export interface PacketHandler {
  on(event: 'packetReceived', listener: (packet: Packet) => void): this
  on(event: 'packetSent', listener: (packet: Packet) => void): this
  on(event: 'packetError', listener: (error: any) => void): this
  emit(event: 'packetReceived', packet: Packet): boolean
  emit(event: 'packetSent', packet: Packet): boolean
  emit(event: 'packetError', error: any): boolean
}

// Packet schemas for validation
const PACKET_SCHEMAS: Record<string, PacketSchema> = {
  logOK: {
    type: 'logOK',
    required: ['status'],
    defaults: { status: false }
  },
  avatars: {
    type: 'avatars',
    required: ['avatars'],
    optional: ['timestamp']
  },
  publicMessage: {
    type: 'publicMessage',
    required: ['message', 'sender'],
    optional: ['timestamp', 'location']
  },
  inventory: {
    type: 'inventory',
    required: ['items'],
    optional: ['capacity', 'weight']
  },
  error: {
    type: 'error',
    required: ['code', 'message'],
    optional: ['details']
  },
  wtl: {
    type: 'wtl',
    required: ['action'],
    optional: ['target', 'parameters']
  }
}

/**
 * Packet validation and processing service
 * 
 * @description Implements "strict in what they produce (send), very relaxed about what they accept (input)"
 * principle for all communications packets. Provides packet validation, processing, and custom handler
 * registration for game communication protocols.
 * 
 * @inputs
 * - Packet type strings (e.g., 'logOK', 'avatars', 'publicMessage')
 * - Packet data objects with required and optional fields
 * - Custom handler functions for specific packet types
 * 
 * @outputs
 * - Validated Packet objects with timestamps and IDs
 * - Validation results with errors and warnings
 * - Event emissions for packet processing
 * 
 * @sideEffects
 * - Emits 'packetReceived', 'packetSent', 'packetError' events
 * - Maintains error and warning statistics
 * - Registers and manages custom packet handlers
 * 
 * @units
 * - Packet types: string identifiers
 * - Timestamps: milliseconds since epoch
 * - Error counts: integer values
 * - Warning counts: integer values
 * 
 * @example
 * ```typescript
 * const handler = new PacketHandler()
 * 
 * // Create a packet
 * const packet = handler.createPacket('logOK', { status: true })
 * 
 * // Register custom handler
 * handler.registerHandler('customType', (packet) => {
 *   console.log('Processing custom packet:', packet)
 * })
 * 
 * // Process packet
 * const result = handler.processPacket(packet)
 * ```
 */
export class PacketHandler extends EventEmitter {
  private schemas: Record<string, PacketSchema>
  private packetHandlers: Map<string, (packet: Packet) => void>
  private errorCount: number = 0
  private warningCount: number = 0

  constructor() {
    super()
    this.schemas = { ...PACKET_SCHEMAS }
    this.packetHandlers = new Map()
  }

  /**
   * Creates a packet with strict validation for outgoing communications
   * 
   * @description Creates a new packet with the specified type and data, applying strict validation
   * for outgoing packets. Throws an error if validation fails, ensuring only valid packets are sent.
   * 
   * @inputs
   * - type: string - The packet type identifier
   * - data: any - The packet data object
   * 
   * @outputs
   * - Packet object with timestamp and ID
   * 
   * @sideEffects
   * - Emits 'packetSent' event
   * - Throws error on validation failure
   * 
   * @units
   * - type: string identifier
   * - timestamp: milliseconds since epoch
   * - id: unique string identifier
   * 
   * @returns {Packet} Validated packet object
   * @throws {Error} When packet validation fails
   */
  createPacket(type: string, data: any): Packet {
    const packet: Packet = {
      type,
      data,
      timestamp: Date.now(),
      id: this.generatePacketId()
    }

    // Validate outgoing packet (strict)
    const validation = this.validateOutgoingPacket(packet)
    if (!validation.valid) {
      throw new Error(`Packet validation failed: ${validation.errors.join(', ')}`)
    }

    // Apply defaults
    this.applyDefaults(packet)

    this.emit('packetSent', packet)
    return packet
  }

  /**
   * Processes incoming packets with relaxed validation
   * 
   * @description Processes incoming packet data with relaxed validation, allowing for more flexible
   * packet formats. Handles both string and object inputs, applies defaults, and calls custom handlers.
   * 
   * @inputs
   * - data: any - Packet data as string or object
   * 
   * @outputs
   * - Packet object or null if validation fails
   * 
   * @sideEffects
   * - Emits 'packetReceived' or 'packetError' events
   * - Increments error/warning counters
   * - Calls registered custom handlers
   * 
   * @units
   * - data: string or object
   * - errorCount: integer
   * - warningCount: integer
   * 
   * @returns {Packet | null} Processed packet or null if invalid
   */
  processPacket(data: any): Packet | null {
    try {
      let packet: Packet

      if (typeof data === 'string') {
        packet = JSON.parse(data)
      } else {
        packet = data
      }

      // Relaxed validation for incoming packets
      const validation = this.validateIncomingPacket(packet)
      
      if (validation.errors.length > 0) {
        this.errorCount++
        this.emit('packetError', {
          packet,
          errors: validation.errors,
          type: 'validation_error'
        })
        return null
      }

      if (validation.warnings.length > 0) {
        this.warningCount++
        console.warn('Packet validation warnings:', validation.warnings)
      }

      // Apply defaults for missing fields
      this.applyDefaults(packet)

      // Call custom handler if registered
      const handler = this.packetHandlers.get(packet.type)
      if (handler) {
        handler(packet)
      }

      this.emit('packetReceived', packet)
      return packet
    } catch (error) {
      this.errorCount++
      this.emit('packetError', {
        error,
        type: 'processing_error'
      })
      return null
    }
  }

  /**
   * Validates outgoing packets with strict requirements
   * 
   * @description Performs strict validation on outgoing packets, ensuring all required fields are present
   * and no unknown fields are included. Used for packets being sent to ensure data integrity.
   * 
   * @inputs
   * - packet: Packet - The packet to validate
   * 
   * @outputs
   * - ValidationResult with validation status, errors, and warnings
   * 
   * @sideEffects
   * - None
   * 
   * @units
   * - packet: Packet object
   * - errors: array of strings
   * - warnings: array of strings
   * 
   * @returns {ValidationResult} Validation result with errors and warnings
   */
  validateOutgoingPacket(packet: Packet): ValidationResult {
    const errors: string[] = []
    const warnings: string[] = []

    // Check if packet type is known
    if (!this.schemas[packet.type]) {
      errors.push(`Unknown packet type: ${packet.type}`)
      return { valid: false, errors, warnings }
    }

    const schema = this.schemas[packet.type]

    // Validate required fields
    for (const field of schema.required) {
      if (!(field in packet.data)) {
        errors.push(`Missing required field: ${field}`)
      }
    }

    // Check for unknown fields
    const knownFields = [...schema.required, ...(schema.optional || [])]
    for (const field in packet.data) {
      if (!knownFields.includes(field)) {
        warnings.push(`Unknown field: ${field}`)
      }
    }

    return {
      valid: errors.length === 0,
      errors,
      warnings
    }
  }

  /**
   * Validates incoming packets with relaxed requirements
   * 
   * @description Performs relaxed validation on incoming packets, allowing for missing optional fields
   * and unknown fields. Used for packets being received to ensure maximum compatibility.
   * 
   * @inputs
   * - packet: Packet - The packet to validate
   * 
   * @outputs
   * - ValidationResult with validation status, errors, and warnings
   * 
   * @sideEffects
   * - None
   * 
   * @units
   * - packet: Packet object
   * - errors: array of strings
   * - warnings: array of strings
   * 
   * @returns {ValidationResult} Validation result with errors and warnings
   */
  validateIncomingPacket(packet: Packet): ValidationResult {
    const errors: string[] = []
    const warnings: string[] = []

    // Basic structure validation
    if (!packet || typeof packet !== 'object') {
      errors.push('Invalid packet structure')
      return { valid: false, errors, warnings }
    }

    if (!packet.type || typeof packet.type !== 'string') {
      errors.push('Missing or invalid packet type')
      return { valid: false, errors, warnings }
    }

    if (!packet.data || typeof packet.data !== 'object') {
      errors.push('Missing or invalid packet data')
      return { valid: false, errors, warnings }
    }

    // Check if packet type is known
    if (!this.schemas[packet.type]) {
      warnings.push(`Unknown packet type: ${packet.type}`)
      return { valid: true, errors, warnings }
    }

    const schema = this.schemas[packet.type]

    // Validate required fields (but don't fail on missing ones)
    for (const field of schema.required) {
      if (!(field in packet.data)) {
        warnings.push(`Missing required field: ${field}`)
      }
    }

    // Check for unknown fields
    const knownFields = [...schema.required, ...(schema.optional || [])]
    for (const field in packet.data) {
      if (!knownFields.includes(field)) {
        warnings.push(`Unknown field: ${field}`)
      }
    }

    return {
      valid: true,
      errors,
      warnings
    }
  }

  /**
   * Apply default values to packet
   */
  private applyDefaults(packet: Packet): void {
    const schema = this.schemas[packet.type]
    if (!schema || !schema.defaults) return

    for (const [field, defaultValue] of Object.entries(schema.defaults)) {
      if (!(field in packet.data)) {
        packet.data[field] = defaultValue
      }
    }
  }

  /**
   * Get default value for a field
   */
  getDefaultValue(type: string, field: string): any {
    const schema = this.schemas[type]
    if (!schema || !schema.defaults) return null
    
    return schema.defaults[field] !== undefined ? schema.defaults[field] : null
  }

  /**
   * Registers a custom handler for a specific packet type
   * 
   * @description Registers a custom function to handle packets of a specific type. The handler will
   * be called whenever a packet of that type is processed.
   * 
   * @inputs
   * - type: string - The packet type to handle
   * - handler: function - The handler function to call
   * 
   * @outputs
   * - None
   * 
   * @sideEffects
   * - Registers handler in internal Map
   * 
   * @units
   * - type: string identifier
   * - handler: function reference
   * 
   * @returns {void}
   */
  registerHandler(type: string, handler: (packet: Packet) => void): void {
    this.packetHandlers.set(type, handler)
  }

  /**
   * Unregister a handler for a specific packet type
   */
  unregisterHandler(type: string): void {
    this.packetHandlers.delete(type)
  }

  /**
   * Get handler for a specific packet type
   */
  getHandler(type: string): ((packet: Packet) => void) | undefined {
    return this.packetHandlers.get(type)
  }

  /**
   * Adds a new packet schema for validation
   * 
   * @description Adds a new packet schema to the internal schema registry, enabling validation for
   * custom packet types.
   * 
   * @inputs
   * - schema: PacketSchema - The schema definition to add
   * 
   * @outputs
   * - None
   * 
   * @sideEffects
   * - Adds schema to internal schemas object
   * 
   * @units
   * - schema: PacketSchema object
   * 
   * @returns {void}
   */
  addSchema(schema: PacketSchema): void {
    this.schemas[schema.type] = schema
  }

  /**
   * Remove a packet schema
   */
  removeSchema(type: string): void {
    delete this.schemas[type]
  }

  /**
   * Get all registered schemas
   */
  getSchemas(): Record<string, PacketSchema> {
    return { ...this.schemas }
  }

  /**
   * Returns current error and warning statistics
   * 
   * @description Returns the current count of validation errors and warnings that have occurred
   * during packet processing.
   * 
   * @inputs
   * - None
   * 
   * @outputs
   * - Object with error and warning counts
   * 
   * @sideEffects
   * - None
   * 
   * @units
   * - errors: integer count
   * - warnings: integer count
   * 
   * @returns {{ errors: number; warnings: number }} Current error and warning counts
   */
  getErrorStats(): { errors: number; warnings: number } {
    return {
      errors: this.errorCount,
      warnings: this.warningCount
    }
  }

  /**
   * Reset error and warning counts
   */
  resetErrorStats(): void {
    this.errorCount = 0
    this.warningCount = 0
  }

  /**
   * Generate a unique packet ID
   */
  private generatePacketId(): string {
    return `packet_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
  }

  /**
   * Validate a packet schema
   */
  validateSchema(schema: PacketSchema): ValidationResult {
    const errors: string[] = []
    const warnings: string[] = []

    if (!schema.type || typeof schema.type !== 'string') {
      errors.push('Schema must have a valid type')
    }

    if (!schema.required || !Array.isArray(schema.required)) {
      errors.push('Schema must have a required array')
    }

    if (schema.optional && !Array.isArray(schema.optional)) {
      errors.push('Schema optional field must be an array')
    }

    if (schema.defaults && typeof schema.defaults !== 'object') {
      errors.push('Schema defaults must be an object')
    }

    return {
      valid: errors.length === 0,
      errors,
      warnings
    }
  }
}

// Export singleton instance
export const packetHandler = new PacketHandler()
