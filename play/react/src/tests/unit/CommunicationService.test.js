/**
 * Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.
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
    
    // Simulate connection after a short delay
    setTimeout(() => {
      this.readyState = WebSocket.OPEN;
      if (this.onopen) this.onopen();
    }, 10);
  }
}

// Mock global WebSocket
global.WebSocket = MockWebSocket;

describe('CommunicationService', () => {
  let service;
  let mockWebSocket;

  beforeEach(() => {
    service = new CommunicationService();
    mockWebSocket = null;
  });

  afterEach(() => {
    if (service.websocket) {
      service.websocket.close();
    }
  });

  describe('Connection Management', () => {
    test('should connect to WebSocket server', async () => {
      const connectPromise = service.connect('ws://localhost:8080');
      
      await connectPromise;
      
      expect(service.websocket).toBeDefined();
      expect(service.websocket.url).toBe('ws://localhost:8080');
      expect(service.isConnected()).toBe(true);
    });

    test('should handle connection errors', async () => {
      const errorSpy = jest.spyOn(console, 'error').mockImplementation();
      
      // Mock WebSocket to simulate error
      const originalWebSocket = global.WebSocket;
      global.WebSocket = class ErrorWebSocket {
        constructor(url) {
          this.url = url;
          this.readyState = WebSocket.CONNECTING;
          this.onerror = null;
          setTimeout(() => {
            if (this.onerror) this.onerror(new Error('Connection failed'));
          }, 10);
        }
      };

      await expect(service.connect('ws://invalid-url')).rejects.toThrow();
      
      global.WebSocket = originalWebSocket;
      errorSpy.mockRestore();
    });

    test('should disconnect properly', async () => {
      await service.connect('ws://localhost:8080');
      
      service.disconnect();
      
      expect(service.websocket.close).toHaveBeenCalled();
      expect(service.isConnected()).toBe(false);
    });
  });

  describe('Message Sending', () => {
    beforeEach(async () => {
      await service.connect('ws://localhost:8080');
    });

    test('should send messages when connected', () => {
      const message = { type: 'test', data: 'hello' };
      
      service.sendMessage(message);
      
      expect(service.websocket.send).toHaveBeenCalledWith(JSON.stringify(message));
    });

    test('should queue messages when not connected', () => {
      service.disconnect();
      const message = { type: 'test', data: 'hello' };
      
      service.sendMessage(message);
      
      expect(service.messageQueue).toContain(message);
    });

    test('should send queued messages when reconnected', async () => {
      service.disconnect();
      const message = { type: 'test', data: 'hello' };
      service.sendMessage(message);
      
      await service.connect('ws://localhost:8080');
      
      expect(service.websocket.send).toHaveBeenCalledWith(JSON.stringify(message));
      expect(service.messageQueue).toHaveLength(0);
    });
  });

  describe('Message Handling', () => {
    beforeEach(async () => {
      await service.connect('ws://localhost:8080');
    });

    test('should handle incoming messages', () => {
      const messageHandler = jest.fn();
      service.onMessage(messageHandler);
      
      const testMessage = { type: 'test', data: 'response' };
      service.websocket.onmessage({ data: JSON.stringify(testMessage) });
      
      expect(messageHandler).toHaveBeenCalledWith(testMessage);
    });

    test('should handle malformed JSON messages', () => {
      const errorSpy = jest.spyOn(console, 'error').mockImplementation();
      
      service.websocket.onmessage({ data: 'invalid json' });
      
      expect(errorSpy).toHaveBeenCalled();
      errorSpy.mockRestore();
    });
  });

  describe('Protocol Compatibility', () => {
    test('should maintain backwards compatibility with existing protocol', () => {
      const legacyMessage = {
        type: 'legacy',
        version: '1.0',
        data: { action: 'move', x: 100, y: 200 }
      };
      
      expect(() => service.sendMessage(legacyMessage)).not.toThrow();
    });

    test('should handle protocol version negotiation', () => {
      const versionMessage = {
        type: 'protocol_version',
        version: '2.0',
        supported: ['1.0', '2.0']
      };
      
      expect(() => service.sendMessage(versionMessage)).not.toThrow();
    });
  });

  describe('Error Handling', () => {
    test('should handle WebSocket close events', () => {
      const closeHandler = jest.fn();
      service.onClose(closeHandler);
      
      service.websocket.onclose({ code: 1000, reason: 'Normal closure' });
      
      expect(closeHandler).toHaveBeenCalledWith(1000, 'Normal closure');
    });

    test('should handle network errors gracefully', () => {
      const errorHandler = jest.fn();
      service.onError(errorHandler);
      
      const testError = new Error('Network error');
      service.websocket.onerror(testError);
      
      expect(errorHandler).toHaveBeenCalledWith(testError);
    });
  });

  describe('Performance', () => {
    test('should handle high message volume', () => {
      const messages = Array.from({ length: 1000 }, (_, i) => ({
        type: 'test',
        id: i,
        data: `message ${i}`
      }));
      
      messages.forEach(msg => service.sendMessage(msg));
      
      expect(service.websocket.send).toHaveBeenCalledTimes(1000);
    });

    test('should not block on large messages', () => {
      const largeMessage = {
        type: 'large_data',
        data: 'x'.repeat(1000000) // 1MB message
      };
      
      const startTime = Date.now();
      service.sendMessage(largeMessage);
      const endTime = Date.now();
      
      // Should complete quickly (less than 100ms)
      expect(endTime - startTime).toBeLessThan(100);
    });
  });
});



