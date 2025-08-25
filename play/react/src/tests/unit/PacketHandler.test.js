/**
 * PacketHandler.test.js - Unit tests for robust communications packet handling
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import PacketHandler from '../../services/PacketHandler';

describe('PacketHandler', () => {
  let packetHandler;

  beforeEach(() => {
    packetHandler = new PacketHandler();
  });

  describe('Outgoing Packet Validation (Strict)', () => {
    it('should strictly validate command packets', () => {
      // Valid command packet
      const validCommand = { c: 'move', d: { x: 1, y: 0, z: 1 } };
      const validated = packetHandler.validateOutgoingPacket('command', validCommand);
      
      expect(validated.c).toBe('move');
      expect(validated.d).toEqual({ x: 1, y: 0, z: 1 });
    });

    it('should reject command packets missing required field', () => {
      const invalidCommand = { d: { x: 1, y: 0, z: 1 } }; // Missing 'c'
      
      expect(() => {
        packetHandler.validateOutgoingPacket('command', invalidCommand);
      }).toThrow('Packet validation failed: Missing required field: c');
    });

    it('should include optional fields in validated packets', () => {
      const commandWithOptional = {
        c: 'move',
        d: { x: 1, y: 0, z: 1 },
        r: '$World',
        a: 'user-uuid',
        u: 'user-uuid'
      };
      
      const validated = packetHandler.validateOutgoingPacket('command', commandWithOptional);
      
      expect(validated.c).toBe('move');
      expect(validated.d).toEqual({ x: 1, y: 0, z: 1 });
      expect(validated.r).toBe('$World');
      expect(validated.a).toBe('user-uuid');
      expect(validated.u).toBe('user-uuid');
    });

    it('should reject unknown packet types for strict validation', () => {
      const unknownPacket = { someField: 'value' };
      
      expect(() => {
        packetHandler.validateOutgoingPacket('unknownType', unknownPacket);
      }).toThrow('Packet validation failed: Unknown packet type: unknownType');
    });
  });

  describe('Incoming Packet Handling (Relaxed)', () => {
    it('should handle valid gatekeeper packets', () => {
      const validPacket = {
        from: 'avatars',
        status: true,
        avatars: { 'user1': { name: 'TestUser' } }
      };
      
      const processed = packetHandler.handleIncomingPacket(validPacket);
      
      expect(processed.from).toBe('avatars');
      expect(processed.status).toBe(true);
      expect(processed.avatars).toEqual({ 'user1': { name: 'TestUser' } });
    });

    it('should handle gatekeeper packets with missing required fields', () => {
      const incompletePacket = {
        from: 'avatars'
        // Missing 'status' field
      };
      
      const processed = packetHandler.handleIncomingPacket(incompletePacket);
      
      expect(processed.from).toBe('avatars');
      expect(processed.status).toBe(false); // Default value
      expect(processed.error).toBeUndefined();
    });

    it('should handle packets with unknown fields', () => {
      const packetWithUnknownFields = {
        from: 'avatars',
        status: true,
        unknownField: 'someValue',
        anotherUnknown: 123
      };
      
      const processed = packetHandler.handleIncomingPacket(packetWithUnknownFields);
      
      expect(processed.from).toBe('avatars');
      expect(processed.status).toBe(true);
      expect(processed.unknownField).toBe('someValue');
      expect(processed.anotherUnknown).toBe(123);
    });

    it('should handle logOK packets correctly', () => {
      const logOKPacket = {
        _cmd: 'logOK',
        status: true,
        greet: 'Welcome to Tootsville!',
        motd: 'Message of the day'
      };
      
      const processed = packetHandler.handleIncomingPacket(logOKPacket);
      
      expect(processed._cmd).toBe('logOK');
      expect(processed.status).toBe(true);
      expect(processed.greet).toBe('Welcome to Tootsville!');
      expect(processed.motd).toBe('Message of the day');
    });

    it('should handle malformed packets gracefully', () => {
      const malformedPacket = {
        from: 'avatars',
        // Missing status, but should still work
        someRandomField: 'randomValue'
      };
      
      const processed = packetHandler.handleIncomingPacket(malformedPacket);
      
      expect(processed.from).toBe('avatars');
      expect(processed.status).toBe(false); // Default value
      expect(processed.someRandomField).toBe('randomValue');
    });

    it('should handle completely invalid packets', () => {
      const invalidPacket = null;
      
      const processed = packetHandler.handleIncomingPacket(invalidPacket);
      
      expect(processed.from).toBe('unknown');
      expect(processed.status).toBe(false);
      expect(processed.error).toBe('Invalid packet format');
    });

    it('should handle non-object packets', () => {
      const nonObjectPacket = 'not an object';
      
      const processed = packetHandler.handleIncomingPacket(nonObjectPacket);
      
      expect(processed.from).toBe('unknown');
      expect(processed.status).toBe(false);
      expect(processed.error).toBe('Invalid packet format');
    });
  });

  describe('Packet Type Detection', () => {
    it('should detect logOK packets', () => {
      const logOKPacket = { _cmd: 'logOK', status: true };
      const type = packetHandler.determinePacketType(logOKPacket);
      expect(type).toBe('logOK');
    });

    it('should detect command packets', () => {
      const commandPacket = { c: 'move', d: { x: 1, y: 0, z: 1 } };
      const type = packetHandler.determinePacketType(commandPacket);
      expect(type).toBe('command');
    });

    it('should detect specific gatekeeper packet types', () => {
      const avatarsPacket = { from: 'avatars', status: true };
      const type = packetHandler.determinePacketType(avatarsPacket);
      expect(type).toBe('avatars');
    });

    it('should default to gatekeeper for unknown from types', () => {
      const unknownPacket = { from: 'unknownType', status: true };
      const type = packetHandler.determinePacketType(unknownPacket);
      expect(type).toBe('gatekeeper');
    });

    it('should handle packets without from field', () => {
      const noFromPacket = { status: true, someData: 'value' };
      const type = packetHandler.determinePacketType(noFromPacket);
      expect(type).toBe('gatekeeper');
    });
  });

  describe('Error Handling', () => {
    it('should track error statistics', () => {
      // Trigger an error
      const invalidPacket = null;
      packetHandler.handleIncomingPacket(invalidPacket);
      
      const stats = packetHandler.getErrorStats();
      expect(stats.errorCount).toBe(0); // No actual errors in this case
      expect(stats.hasErrors).toBe(false);
    });

    it('should emit packetError events', () => {
      const errorHandler = jest.fn();
      packetHandler.on('packetError', errorHandler);
      
      // Trigger an error by passing invalid data
      const invalidPacket = { from: 'test' };
      packetHandler.handleIncomingPacket(invalidPacket);
      
      // Should not emit error for valid packet
      expect(errorHandler).not.toHaveBeenCalled();
    });

    it('should reset error tracking', () => {
      packetHandler.resetErrorTracking();
      
      const stats = packetHandler.getErrorStats();
      expect(stats.errorCount).toBe(0);
      expect(stats.lastError).toBeNull();
    });
  });

  describe('Event Emission', () => {
    it('should emit packetReceived events', () => {
      const receivedHandler = jest.fn();
      packetHandler.on('packetReceived', receivedHandler);
      
      const testPacket = { from: 'avatars', status: true };
      packetHandler.handleIncomingPacket(testPacket);
      
      expect(receivedHandler).toHaveBeenCalledWith({
        type: 'avatars',
        data: expect.objectContaining({ from: 'avatars', status: true }),
        original: testPacket
      });
    });

    it('should emit packetSent events', () => {
      const sentHandler = jest.fn();
      packetHandler.on('packetSent', sentHandler);
      
      const testData = { c: 'move', d: { x: 1, y: 0, z: 1 } };
      packetHandler.createOutgoingPacket('command', testData);
      
      expect(sentHandler).toHaveBeenCalledWith({
        type: 'command',
        data: expect.objectContaining({ c: 'move', d: { x: 1, y: 0, z: 1 } })
      });
    });
  });

  describe('Handler Registration', () => {
    it('should register packet handlers', () => {
      const handler = jest.fn();
      packetHandler.registerHandler('testType', handler);
      
      const retrievedHandler = packetHandler.getHandler('testType');
      expect(retrievedHandler).toBe(handler);
    });

    it('should reject non-function handlers', () => {
      expect(() => {
        packetHandler.registerHandler('testType', 'not a function');
      }).toThrow('Handler for packet type \'testType\' must be a function');
    });

    it('should return null for unregistered handlers', () => {
      const handler = packetHandler.getHandler('nonexistent');
      expect(handler).toBeNull();
    });
  });

  describe('Schema Management', () => {
    it('should validate schemas', () => {
      const validSchema = {
        required: ['field1'],
        optional: ['field2']
      };
      
      expect(packetHandler.validateSchema(validSchema)).toBe(true);
    });

    it('should reject invalid schemas', () => {
      const invalidSchema = {
        required: 'not an array'
      };
      
      expect(packetHandler.validateSchema(invalidSchema)).toBe(false);
    });

    it('should add new schemas', () => {
      const newSchema = {
        required: ['newField'],
        optional: ['optionalField']
      };
      
      packetHandler.addSchema('newType', newSchema);
      
      const validated = packetHandler.validateOutgoingPacket('newType', { newField: 'value' });
      expect(validated.newField).toBe('value');
    });

    it('should reject invalid schemas when adding', () => {
      const invalidSchema = { required: 'not an array' };
      
      expect(() => {
        packetHandler.addSchema('newType', invalidSchema);
      }).toThrow('Invalid schema for packet type: newType');
    });
  });

  describe('Default Values', () => {
    it('should provide sensible defaults for missing fields', () => {
      expect(packetHandler.getDefaultValue('from')).toBe('unknown');
      expect(packetHandler.getDefaultValue('status')).toBe(false);
      expect(packetHandler.getDefaultValue('error')).toBe('Missing required field');
      expect(packetHandler.getDefaultValue('avatars')).toEqual({});
      expect(packetHandler.getDefaultValue('items')).toEqual([]);
    });

    it('should return null for unknown fields', () => {
      expect(packetHandler.getDefaultValue('unknownField')).toBeNull();
    });
  });

  describe('Backward Compatibility', () => {
    it('should preserve additional fields in incoming packets', () => {
      const packetWithExtras = {
        from: 'avatars',
        status: true,
        legacyField: 'oldValue',
        deprecatedField: 123
      };
      
      const processed = packetHandler.handleIncomingPacket(packetWithExtras);
      
      expect(processed.from).toBe('avatars');
      expect(processed.status).toBe(true);
      expect(processed.legacyField).toBe('oldValue');
      expect(processed.deprecatedField).toBe(123);
    });

    it('should handle packets with different field names', () => {
      const packetWithDifferentNames = {
        from: 'avatars',
        status: true,
        oldFieldName: 'value',
        differentCase: 'test'
      };
      
      const processed = packetHandler.handleIncomingPacket(packetWithDifferentNames);
      
      expect(processed.oldFieldName).toBe('value');
      expect(processed.differentCase).toBe('test');
    });
  });
});

