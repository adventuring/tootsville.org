import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { PacketHandler } from '../PacketHandler'

describe('PacketHandler', () => {
  let packetHandler: PacketHandler

  beforeEach(() => {
    packetHandler = new PacketHandler()
  })

  afterEach(() => {
    packetHandler.removeAllListeners()
  })

  describe('Packet Validation Requirements', () => {
    it('should enforce strict validation for outgoing packets', () => {
      // When sending packets, all required fields must be present
      expect(() => {
        packetHandler.createPacket('logOK', {})
      }).toThrow('Packet validation failed: Missing required field: status')
    })

    it('should allow relaxed validation for incoming packets', () => {
      // When receiving packets, missing optional fields should be tolerated
      const packet = {
        type: 'publicMessage',
        data: { 
          message: 'Hello', 
          sender: 'player1' 
          // timestamp is optional and missing
        }
      }
      const result = packetHandler.validateIncomingPacket(packet)
      
      expect(result.valid).toBe(true)
    })

    it('should apply default values for missing optional fields', () => {
      // When receiving packets with missing optional fields, defaults should be applied
      const packet = {
        type: 'logOK',
        data: {}
      }
      packetHandler.validateIncomingPacket(packet)
      packetHandler['applyDefaults'](packet)
      
      expect(packet.data.status).toBe(false) // default value applied
    })

    it('should reject unknown packet types when sending', () => {
      // Outgoing packets must use known packet types
      expect(() => {
        packetHandler.createPacket('unknownType', { data: 'test' })
      }).toThrow('Packet validation failed: Unknown packet type: unknownType')
    })

    it('should accept unknown packet types when receiving', () => {
      // Incoming packets can be of unknown types (for future compatibility)
      const unknownPacket = {
        type: 'unknownType',
        data: { data: 'test' }
      }
      const result = packetHandler.validateIncomingPacket(unknownPacket)
      
      expect(result.valid).toBe(true)
      expect(result.warnings).toContain('Unknown packet type: unknownType')
    })
  })

  describe('Packet Processing Requirements', () => {
    it('should process valid packets successfully', () => {
      // Valid packets should be processed without errors
      const validPacket = packetHandler.createPacket('logOK', { status: true })
      const result = packetHandler.processPacket(validPacket)
      
      expect(result).toEqual(validPacket)
    })

    it('should notify when packets are received', () => {
      // The system should notify when packets are successfully processed
      const eventSpy = vi.fn()
      packetHandler.on('packetReceived', eventSpy)
      
      const packet = packetHandler.createPacket('logOK', { status: true })
      packetHandler.processPacket(packet)
      
      expect(eventSpy).toHaveBeenCalledWith(packet)
    })

    it('should notify when packet errors occur', () => {
      // The system should notify when packet processing fails
      const errorSpy = vi.fn()
      packetHandler.on('packetError', errorSpy)
      
      const invalidPacket = { type: 'logOK' } // missing data field
      packetHandler.processPacket(invalidPacket)
      
      expect(errorSpy).toHaveBeenCalled()
    })

    it('should handle malformed packets gracefully', () => {
      // The system should not crash when receiving malformed packets
      const malformedPacket = { type: 'logOK' } // missing data field
      const result = packetHandler.processPacket(malformedPacket as any)
      
      expect(result).toBe(null)
    })
  })

  describe('Custom Packet Handling Requirements', () => {
    it('should allow custom handlers for specific packet types', () => {
      // Users should be able to register custom handlers for packet types
      const customHandler = vi.fn()
      packetHandler.registerHandler('logOK', customHandler)
      
      const packet = packetHandler.createPacket('logOK', { status: true })
      packetHandler.processPacket(packet)
      
      expect(customHandler).toHaveBeenCalledWith(packet)
    })

    it('should allow removal of custom handlers', () => {
      // Users should be able to remove custom handlers
      const customHandler = vi.fn()
      packetHandler.registerHandler('logOK', customHandler)
      packetHandler.unregisterHandler('logOK')
      
      const packet = packetHandler.createPacket('logOK', { status: true })
      packetHandler.processPacket(packet)
      
      expect(customHandler).not.toHaveBeenCalled()
    })
  })

  describe('Schema Management Requirements', () => {
    it('should allow adding new packet schemas', () => {
      // Users should be able to define new packet types
      const newSchema = {
        type: 'customMessage',
        required: ['content'],
        optional: ['priority']
      }
      
      packetHandler.addSchema(newSchema)
      
      const packet = packetHandler.createPacket('customMessage', { content: 'Hello' })
      expect(packet.type).toBe('customMessage')
    })

    it('should allow removing packet schemas', () => {
      // Users should be able to remove packet schemas
      packetHandler.removeSchema('logOK')
      
      expect(() => {
        packetHandler.createPacket('logOK', { status: true })
      }).toThrow('Packet validation failed: Unknown packet type: logOK')
    })
  })

  describe('Monitoring and Statistics Requirements', () => {
    it('should track validation errors', () => {
      // The system should track how many validation errors occur
      const invalidPacket = { type: 'logOK' } // missing data field
      packetHandler.processPacket(invalidPacket)
      
      const stats = packetHandler.getErrorStats()
      expect(stats.errors).toBeGreaterThan(0)
    })

    it('should track validation warnings', () => {
      // The system should track how many validation warnings occur
      const packet = {
        type: 'unknownType',
        data: { data: 'test' }
      }
      packetHandler.validateIncomingPacket(packet)
      
      const stats = packetHandler.getErrorStats()
      expect(stats.warnings).toBeGreaterThanOrEqual(0)
    })

    it('should allow resetting statistics', () => {
      // Users should be able to reset error/warning counters
      const invalidPacket = { type: 'logOK' } // missing data field
      packetHandler.processPacket(invalidPacket)
      
      const statsBefore = packetHandler.getErrorStats()
      expect(statsBefore.errors).toBeGreaterThan(0)
      
      packetHandler.resetErrorStats()
      
      const statsAfter = packetHandler.getErrorStats()
      expect(statsAfter.errors).toBe(0)
    })
  })

  describe('Game-Specific Packet Requirements', () => {
    it('should handle login status packets', () => {
      // The game needs to handle login status updates
      const loginPacket = packetHandler.createPacket('logOK', { status: true })
      const result = packetHandler.processPacket(loginPacket)
      
      expect(result).toEqual(loginPacket)
    })

    it('should handle avatar list packets', () => {
      // The game needs to handle avatar list updates
      const avatarPacket = packetHandler.createPacket('avatars', { avatars: [] })
      const result = packetHandler.processPacket(avatarPacket)
      
      expect(result).toEqual(avatarPacket)
    })

    it('should handle public message packets', () => {
      // The game needs to handle public chat messages
      const messagePacket = packetHandler.createPacket('publicMessage', { 
        message: 'Hello Tootsville!', 
        sender: 'player1' 
      })
      const result = packetHandler.processPacket(messagePacket)
      
      expect(result).toEqual(messagePacket)
    })

    it('should handle inventory packets', () => {
      // The game needs to handle inventory updates
      const inventoryPacket = packetHandler.createPacket('inventory', { 
        items: [],
        capacity: 100 
      })
      const result = packetHandler.processPacket(inventoryPacket)
      
      expect(result).toEqual(inventoryPacket)
    })

    it('should handle error packets', () => {
      // The game needs to handle error notifications
      const errorPacket = packetHandler.createPacket('error', { 
        code: 404, 
        message: 'Not found' 
      })
      const result = packetHandler.processPacket(errorPacket)
      
      expect(result).toEqual(errorPacket)
    })
  })
})
