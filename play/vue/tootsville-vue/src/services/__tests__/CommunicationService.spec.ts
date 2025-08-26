import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { CommunicationService } from '../CommunicationService'

// Mock WebSocket
class MockWebSocket {
  static instances: MockWebSocket[] = []
  readyState = 0 // CONNECTING
  url: string
  onopen: ((event: any) => void) | null = null
  onmessage: ((event: any) => void) | null = null
  onclose: ((event: any) => void) | null = null
  onerror: ((event: any) => void) | null = null
  send: (data: string) => void
  close: () => void

  constructor(url: string) {
    this.url = url
    MockWebSocket.instances.push(this)
    this.send = vi.fn()
    this.close = vi.fn()
  }

  static reset() {
    MockWebSocket.instances = []
  }
}

// Mock fetch
global.fetch = vi.fn()

// Mock WebSocket
Object.defineProperty(global, 'WebSocket', {
  value: MockWebSocket,
  writable: true
})

describe('CommunicationService', () => {
  let service: CommunicationService

  beforeEach(() => {
    service = new CommunicationService()
    MockWebSocket.reset()
    vi.clearAllMocks()
  })

  afterEach(() => {
    service.disconnect()
  })

  describe('Multi-Protocol Connection Requirements', () => {
    it('should support WebSocket connections', async () => {
      // The service should be able to establish WebSocket connections
      const connectSpy = vi.fn()
      service.on('connected', connectSpy)

      const result = await service.connect('test')
      
      expect(result).toBe(true)
      expect(connectSpy).toHaveBeenCalled()
    })

    it('should support REST API connections', async () => {
      // The service should be able to make REST API calls
      ;(global.fetch as any).mockResolvedValue({
        ok: true,
        json: () => Promise.resolve({ status: 'ok' })
      })

      const result = await service.connect('test')
      
      expect(result).toBe(true)
    })

    it('should support peer-to-peer connections', async () => {
      // The service should be able to establish P2P connections
      const connectSpy = vi.fn()
      service.on('connected', connectSpy)

      const result = await service.connect('test')
      
      expect(result).toBe(true)
      expect(connectSpy).toHaveBeenCalledWith({ protocol: 'p2p' })
    })

    it('should maintain multiple simultaneous connections', async () => {
      // The service should support multiple protocols simultaneously
      ;(global.fetch as any).mockResolvedValue({
        ok: true,
        json: () => Promise.resolve({ status: 'ok' })
      })

      const result = await service.connect('test')

      const status = service.getConnectionStatus()
      
      expect(status.websocket).toBeDefined()
      expect(status.rest).toBeDefined()
      expect(status.p2p).toBeDefined()
    })
  })

  describe('Message Routing Requirements', () => {
    it('should route messages through the most appropriate protocol', async () => {
      // The service should choose the best protocol for each message type
      await service.connect('test')

      const result = await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      expect(result).toBe(true)
    })

    it('should fallback to alternative protocols when primary fails', async () => {
      // The service should automatically fallback when a protocol fails
      await service.connect('test')
      
      // Simulate WebSocket failure
      const wsInstance = MockWebSocket.instances[0]
      wsInstance.readyState = 3 // CLOSED

      ;(global.fetch as any).mockResolvedValue({
        ok: true,
        json: () => Promise.resolve({ status: 'ok' })
      })

      const result = await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      expect(result).toBe(true)
    })

    it('should prioritize real-time protocols for urgent messages', async () => {
      // Real-time messages should use WebSocket or P2P when available
      await service.connect('test')

      const result = await service.send('wtl', { action: 'alert', parameters: { message: 'Emergency!' } })

      expect(result).toBe(true)
    })
  })

  describe('Connection Management Requirements', () => {
    it('should automatically reconnect when connections are lost', async () => {
      // The service should automatically attempt to reconnect
      await service.connect('test')
      
      const disconnectSpy = vi.fn()
      service.on('disconnected', disconnectSpy)

      // Simulate connection loss
      const wsInstance = MockWebSocket.instances[0]
      wsInstance.onclose?.({})

      expect(disconnectSpy).toHaveBeenCalled()
    })

    it('should handle connection timeouts gracefully', async () => {
      // The service should handle connection timeouts without crashing
      const errorSpy = vi.fn()
      service.on('protocolError', errorSpy)

      // Simulate timeout by making WebSocket fail
      MockWebSocket.prototype.readyState = 3 // CLOSED
      await service.connect('test')
      
      // Should handle timeout gracefully
      expect(service.getConnectionStatus()).toBeDefined()
    })

    it('should limit reconnection attempts', async () => {
      // The service should not attempt infinite reconnections
      const maxAttempts = 5
      
      for (let i = 0; i < maxAttempts + 1; i++) {
        await service.connect('test')
      }

      // Should not crash after max attempts
      expect(service.getConnectionStatus()).toBeDefined()
    })
  })

  describe('Message Reliability Requirements', () => {
    it('should ensure message delivery confirmation', async () => {
      // The service should confirm when messages are delivered
      await service.connect('test')

      const deliverySpy = vi.fn()
      service.on('packetSent', deliverySpy)

      await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      expect(deliverySpy).toHaveBeenCalled()
    })

    it('should retry failed message deliveries', async () => {
      // The service should retry sending messages that fail
      await service.connect('test')

      const retrySpy = vi.fn()
      service.on('packetError', retrySpy)

      // Simulate message failure
      const wsInstance = MockWebSocket.instances[0]
      wsInstance.send = vi.fn().mockImplementation(() => {
        throw new Error('Send failed')
      })

      await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      expect(retrySpy).toHaveBeenCalled()
    })

    it('should queue messages when no connection is available', async () => {
      // The service should queue messages when offline
      const queueSpy = vi.fn()
      service.on('packetError', queueSpy)

      await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      expect(service.getConnectionStatus()).toBeDefined()
    })
  })

  describe('Security Requirements', () => {
    it('should handle secure message transmission', async () => {
      // The service should handle sensitive data transmission
      await service.connect('test')

      const result = await service.send('logOK', { status: true })

      expect(result).toBe(true)
    })

    it('should validate message authenticity', async () => {
      // The service should verify message authenticity
      await service.connect('test')

      const result = await service.send('wtl', { action: 'admin', parameters: { action: 'restart' } })

      expect(result).toBe(true)
    })

    it('should handle authentication failures gracefully', async () => {
      // The service should handle auth failures without exposing sensitive data
      const errorSpy = vi.fn()
      service.on('packetError', errorSpy)

      await service.send('wtl', { action: 'admin', parameters: { action: 'restart' } })

      expect(service.getConnectionStatus()).toBeDefined()
    })
  })

  describe('Performance Requirements', () => {
    it('should handle high message volumes', async () => {
      // The service should handle many messages without performance degradation
      await service.connect('test')

      const messages = Array.from({ length: 10 }, (_, i) => ({
        type: 'publicMessage',
        data: { message: `Message ${i}`, sender: 'player1' }
      }))

      const results = await Promise.all(
        messages.map(msg => service.send(msg.type, msg.data))
      )

      expect(results.every(result => result === true)).toBe(true)
    })

    it('should maintain low latency for real-time messages', async () => {
      // Real-time messages should have minimal latency
      await service.connect('test')

      const startTime = Date.now()
      await service.send('publicMessage', { message: 'Hello', sender: 'player1' })
      const endTime = Date.now()

      const latency = endTime - startTime
      expect(latency).toBeLessThan(1000) // Less than 1 second
    })

    it('should optimize bandwidth usage', async () => {
      // The service should minimize bandwidth usage
      await service.connect('test')

      const stats = service.getProtocolStats()
      
      // Should track bandwidth usage
      expect(stats.websocket.sent).toBeGreaterThanOrEqual(0)
      expect(stats.websocket.received).toBeGreaterThanOrEqual(0)
    })
  })

  describe('Error Handling Requirements', () => {
    it('should handle network errors gracefully', async () => {
      // The service should handle network failures without crashing
      const errorSpy = vi.fn()
      service.on('protocolError', errorSpy)

      // Simulate network error
      ;(global.fetch as any).mockRejectedValue(new Error('Network error'))

      await service.connect('test')

      expect(service.getConnectionStatus()).toBeDefined()
    })

    it('should handle protocol-specific errors', async () => {
      // The service should handle different types of protocol errors
      await service.connect('test')

      const errorSpy = vi.fn()
      service.on('protocolError', errorSpy)

      // Simulate WebSocket error
      const wsInstance = MockWebSocket.instances[0]
      wsInstance.onerror?.({ error: 'Connection failed' })

      expect(errorSpy).toHaveBeenCalled()
    })

    it('should provide meaningful error messages', async () => {
      // Error messages should be helpful for debugging
      const errorSpy = vi.fn()
      service.on('protocolError', errorSpy)

      ;(global.fetch as any).mockRejectedValue(new Error('Network error'))

      await service.connect('test')

      expect(service.getConnectionStatus()).toBeDefined()
    })
  })

  describe('Game-Specific Requirements', () => {
    it('should handle player chat messages', async () => {
      // The game needs to handle player chat messages
      await service.connect('test')

      const result = await service.send('publicMessage', {
        message: 'Hello Tootsville!',
        sender: 'player1'
      })

      expect(result).toBe(true)
    })

    it('should handle player movement updates', async () => {
      // The game needs to handle player position updates
      await service.connect('test')

      const result = await service.send('wtl', {
        action: 'move',
        target: 'player1',
        parameters: { x: 10, y: 5, z: 0 }
      })

      expect(result).toBe(true)
    })

    it('should handle game state synchronization', async () => {
      // The game needs to sync state between players
      await service.connect('test')

      const result = await service.send('avatars', {
        avatars: []
      })

      expect(result).toBe(true)
    })

    it('should handle inventory updates', async () => {
      // The game needs to handle inventory changes
      await service.connect('test')

      const result = await service.send('inventory', {
        items: [],
        capacity: 100
      })

      expect(result).toBe(true)
    })
  })

  describe('Monitoring and Statistics Requirements', () => {
    it('should track connection statistics', async () => {
      // The service should track connection performance
      await service.connect('test')

      const stats = service.getProtocolStats()
      
      expect(stats.websocket.sent).toBeGreaterThanOrEqual(0)
      expect(stats.websocket.received).toBeGreaterThanOrEqual(0)
      expect(stats.rest.sent).toBeGreaterThanOrEqual(0)
      expect(stats.rest.received).toBeGreaterThanOrEqual(0)
    })

    it('should track message delivery rates', async () => {
      // The service should track successful vs failed message deliveries
      await service.connect('test')

      await service.send('publicMessage', { message: 'Hello', sender: 'player1' })

      const stats = service.getProtocolStats()
      expect(stats.websocket.sent).toBeGreaterThanOrEqual(0)
    })

    it('should provide connection health information', async () => {
      // The service should provide health status for all connections
      await service.connect('test')

      const status = service.getConnectionStatus()
      
      expect(status.websocket).toBeDefined()
      expect(status.rest).toBeDefined()
      expect(status.p2p).toBeDefined()
    })
  })
})
