/**
 * CommunicationService.test.js - Unit tests for CommunicationService
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the CommunicationService including
 * WebSocket connection management, message handling, and error scenarios.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import { CommunicationService } from '../../services/CommunicationService';

// Mock WebSocket
class MockWebSocket {
  constructor(url) {
    this.url = url;
    this.readyState = WebSocket.CONNECTING;
    this.onopen = null;
    this.onclose = null;
    this.onerror = null;
    this.onmessage = null;
    this.send = jest.fn();
    this.close = jest.fn();
    
    // Simulate connection
    setTimeout(() => {
      this.readyState = WebSocket.OPEN;
      if (this.onopen) this.onopen();
    }, 10);
  }
}

global.WebSocket = MockWebSocket;

describe('CommunicationService', () => {
  let service;
  let mockWebSocket;

  beforeEach(() => {
    service = new CommunicationService();
    jest.clearAllMocks();
  });

  afterEach(() => {
    if (service.isConnected()) {
      service.disconnect();
    }
  });

  describe('Connection Management', () => {
    test('should connect to WebSocket successfully', async () => {
      const connectPromise = service.connect('test.tootsville.org');
      
      await expect(connectPromise).resolves.toBe(true);
      expect(service.isConnected()).toBe(true);
    });

    test('should handle connection errors', async () => {
      // Mock WebSocket to throw error
      const originalWebSocket = global.WebSocket;
      global.WebSocket = class ErrorWebSocket {
        constructor() {
          this.readyState = WebSocket.CONNECTING;
          setTimeout(() => {
            this.readyState = WebSocket.CLOSED;
            if (this.onerror) this.onerror(new Error('Connection failed'));
          }, 10);
        }
      };

      const connectPromise = service.connect('invalid.tootsville.org');
      
      await expect(connectPromise).rejects.toThrow('Connection failed');
      expect(service.isConnected()).toBe(false);
      
      global.WebSocket = originalWebSocket;
    });

    test('should disconnect properly', async () => {
      await service.connect('test.tootsville.org');
      expect(service.isConnected()).toBe(true);
      
      service.disconnect();
      expect(service.isConnected()).toBe(false);
    });

    test('should prevent multiple connections', async () => {
      await service.connect('test.tootsville.org');
      expect(service.isConnected()).toBe(true);
      
      const secondConnect = service.connect('test2.tootsville.org');
      await expect(secondConnect).rejects.toThrow('Already connected');
    });

    test('should handle reconnection', async () => {
      await service.connect('test.tootsville.org');
      service.disconnect();
      
      const reconnectPromise = service.connect('test.tootsville.org');
      await expect(reconnectPromise).resolves.toBe(true);
      expect(service.isConnected()).toBe(true);
    });
  });

  describe('Message Sending', () => {
    beforeEach(async () => {
      await service.connect('test.tootsville.org');
    });

    test('should send messages in correct format', () => {
      const testMessage = { type: 'test', data: 'hello' };
      service.send(testMessage);
      
      expect(service.socket.send).toHaveBeenCalledWith(
        JSON.stringify({ command: 'test', data: 'hello' })
      );
    });

    test('should handle send errors', () => {
      service.socket.send.mockImplementation(() => {
        throw new Error('Send failed');
      });
      
      expect(() => {
        service.send({ type: 'test', data: 'hello' });
      }).toThrow('Send failed');
    });

    test('should queue messages when disconnected', async () => {
      service.disconnect();
      
      service.send({ type: 'queued', data: 'message' });
      expect(service.messageQueue).toHaveLength(1);
      
      await service.connect('test.tootsville.org');
      expect(service.socket.send).toHaveBeenCalledWith(
        JSON.stringify({ command: 'queued', data: 'message' })
      );
    });

    test('should send queued messages on reconnection', async () => {
      service.disconnect();
      service.send({ type: 'first', data: 'message1' });
      service.send({ type: 'second', data: 'message2' });
      
      await service.connect('test.tootsville.org');
      
      expect(service.socket.send).toHaveBeenCalledTimes(2);
      expect(service.socket.send).toHaveBeenNthCalledWith(1,
        JSON.stringify({ command: 'first', data: 'message1' })
      );
      expect(service.socket.send).toHaveBeenNthCalledWith(2,
        JSON.stringify({ command: 'second', data: 'message2' })
      );
    });
  });

  describe('Message Handling', () => {
    beforeEach(async () => {
      await service.connect('test.tootsville.org');
    });

    test('should handle incoming messages', () => {
      const mockHandler = jest.fn();
      service.onMessage(mockHandler);
      
      const testMessage = { type: 'response', payload: 'test data' };
      service.socket.onmessage({ data: JSON.stringify(testMessage) });
      
      expect(mockHandler).toHaveBeenCalledWith(testMessage);
    });

    test('should handle malformed JSON messages', () => {
      const mockHandler = jest.fn();
      service.onMessage(mockHandler);
      
      service.socket.onmessage({ data: 'invalid json' });
      
      expect(mockHandler).not.toHaveBeenCalled();
    });

    test('should handle multiple message handlers', () => {
      const handler1 = jest.fn();
      const handler2 = jest.fn();
      
      service.onMessage(handler1);
      service.onMessage(handler2);
      
      const testMessage = { type: 'test', payload: 'data' };
      service.socket.onmessage({ data: JSON.stringify(testMessage) });
      
      expect(handler1).toHaveBeenCalledWith(testMessage);
      expect(handler2).toHaveBeenCalledWith(testMessage);
    });

    test('should remove message handlers', () => {
      const handler = jest.fn();
      const removeHandler = service.onMessage(handler);
      
      const testMessage = { type: 'test', payload: 'data' };
      service.socket.onmessage({ data: JSON.stringify(testMessage) });
      expect(handler).toHaveBeenCalledTimes(1);
      
      removeHandler();
      service.socket.onmessage({ data: JSON.stringify(testMessage) });
      expect(handler).toHaveBeenCalledTimes(1); // Should not be called again
    });
  });

  describe('Error Handling', () => {
    test('should handle WebSocket errors', async () => {
      const errorHandler = jest.fn();
      service.onError(errorHandler);
      
      await service.connect('test.tootsville.org');
      service.socket.onerror(new Error('WebSocket error'));
      
      expect(errorHandler).toHaveBeenCalledWith(expect.any(Error));
    });

    test('should handle connection close', async () => {
      const closeHandler = jest.fn();
      service.onClose(closeHandler);
      
      await service.connect('test.tootsville.org');
      service.socket.onclose({ code: 1000, reason: 'Normal closure' });
      
      expect(closeHandler).toHaveBeenCalledWith({ code: 1000, reason: 'Normal closure' });
      expect(service.isConnected()).toBe(false);
    });

    test('should handle unexpected disconnection', async () => {
      const reconnectSpy = jest.spyOn(service, 'reconnect');
      
      await service.connect('test.tootsville.org');
      service.socket.onclose({ code: 1006, reason: 'Abnormal closure' });
      
      expect(reconnectSpy).toHaveBeenCalled();
    });
  });

  describe('Protocol Compatibility', () => {
    beforeEach(async () => {
      await service.connect('test.tootsville.org');
    });

    test('should maintain backwards compatibility with existing protocol', () => {
      // Test that the service uses the expected command/data format
      const legacyMessage = { command: 'speak', data: 'Hello world' };
      service.send(legacyMessage);
      
      expect(service.socket.send).toHaveBeenCalledWith(
        JSON.stringify({ command: 'speak', data: 'Hello world' })
      );
    });

    test('should handle type/payload format for responses', () => {
      const mockHandler = jest.fn();
      service.onMessage(mockHandler);
      
      const responseMessage = { type: 'speak', payload: 'Hello world' };
      service.socket.onmessage({ data: JSON.stringify(responseMessage) });
      
      expect(mockHandler).toHaveBeenCalledWith(responseMessage);
    });

    test('should handle mixed message formats', () => {
      const mockHandler = jest.fn();
      service.onMessage(mockHandler);
      
      // Send in command/data format
      service.send({ command: 'speak', data: 'Hello' });
      
      // Receive in type/payload format
      service.socket.onmessage({ 
        data: JSON.stringify({ type: 'speak', payload: 'Hello back' }) 
      });
      
      expect(mockHandler).toHaveBeenCalledWith({ 
        type: 'speak', payload: 'Hello back' 
      });
    });
  });

  describe('Edge Cases', () => {
    test('should handle rapid connect/disconnect cycles', async () => {
      for (let i = 0; i < 5; i++) {
        await service.connect('test.tootsville.org');
        service.disconnect();
      }
      
      expect(service.isConnected()).toBe(false);
    });

    test('should handle null/undefined messages', () => {
      expect(() => service.send(null)).toThrow();
      expect(() => service.send(undefined)).toThrow();
    });

    test('should handle empty message objects', () => {
      expect(() => service.send({})).not.toThrow();
    });

    test('should handle very large messages', () => {
      const largeData = 'x'.repeat(1000000); // 1MB string
      const largeMessage = { type: 'large', data: largeData };
      
      expect(() => service.send(largeMessage)).not.toThrow();
    });

    test('should handle special characters in messages', () => {
      const specialMessage = { 
        type: 'special', 
        data: 'Hello 世界! 🎉\n\t\r' 
      };
      
      service.send(specialMessage);
      
      expect(service.socket.send).toHaveBeenCalledWith(
        JSON.stringify({ command: 'special', data: 'Hello 世界! 🎉\n\t\r' })
      );
    });
  });

  describe('Performance', () => {
    test('should handle high message frequency', () => {
      const startTime = Date.now();
      
      for (let i = 0; i < 1000; i++) {
        service.send({ type: 'test', data: `message ${i}` });
      }
      
      const endTime = Date.now();
      expect(endTime - startTime).toBeLessThan(1000); // Should complete in under 1 second
    });

    test('should not leak memory with message handlers', () => {
      const initialHandlerCount = service.messageHandlers.length;
      
      for (let i = 0; i < 100; i++) {
        const handler = jest.fn();
        const removeHandler = service.onMessage(handler);
        removeHandler();
      }
      
      expect(service.messageHandlers.length).toBe(initialHandlerCount);
    });
  });
});
