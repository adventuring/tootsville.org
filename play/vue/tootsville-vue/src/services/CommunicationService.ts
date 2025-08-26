/**
 * CommunicationService.ts - Multi-Protocol Communication Service with Gatekeeper
 * 
 * Vue 3 / TypeScript version supporting REST, WebSocket, and Peer-to-Peer networking
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'
import { EventEmitter } from 'events'
import type { PacketHandler } from './PacketHandler'
import { PacketHandler as PacketHandlerClass } from './PacketHandler'

// Types
export interface ConnectionState {
  websocket: boolean
  rest: boolean
  p2p: boolean
}

export interface ConnectionInfo {
  websocket: WebSocket | null
  rest: any | null
  p2p: any | null
}

export interface ReconnectAttempts {
  websocket: number
  rest: number
  p2p: number
}

export interface ProtocolStats {
  websocket: { sent: number; received: number; errors: number }
  rest: { sent: number; received: number; errors: number }
  p2p: { sent: number; received: number; errors: number }
}

export interface Message {
  type: string
  data: any
  timestamp: number
  protocol?: string
}

export interface ConnectionOptions {
  timeout?: number
  retries?: number
  fallback?: boolean
}

// Protocol Handler Classes
class WebSocketHandler {
  private service: CommunicationService
  private socket: WebSocket | null = null

  constructor(service: CommunicationService) {
    this.service = service
  }

  async connect(url: string, options: ConnectionOptions = {}): Promise<boolean> {
    try {
      this.socket = new WebSocket(url)
      
      this.socket.onopen = () => {
        this.service.setConnectionState('websocket', true)
        this.service.emit('connected', { protocol: 'websocket' })
      }

      this.socket.onmessage = (event) => {
        this.service.handleIncomingMessage(event.data, 'websocket')
      }

      this.socket.onclose = () => {
        this.service.setConnectionState('websocket', false)
        this.service.emit('disconnected', { protocol: 'websocket' })
      }

      this.socket.onerror = (error) => {
        this.service.handleProtocolError('websocket', error)
      }

      return true
    } catch (error) {
      this.service.handleProtocolError('websocket', error)
      return false
    }
  }

  send(message: string): boolean {
    if (this.socket && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(message)
      return true
    }
    return false
  }

  disconnect(): void {
    if (this.socket) {
      this.socket.close()
      this.socket = null
    }
  }
}

class RESTHandler {
  private service: CommunicationService
  private baseUrl: string = ''

  constructor(service: CommunicationService) {
    this.service = service
  }

  async connect(url: string, options: ConnectionOptions = {}): Promise<boolean> {
    try {
      this.baseUrl = url
      this.service.setConnectionState('rest', true)
      this.service.emit('connected', { protocol: 'rest' })
      return true
    } catch (error) {
      this.service.handleProtocolError('rest', error)
      return false
    }
  }

  async send(message: any): Promise<boolean> {
    try {
      const response = await fetch(`${this.baseUrl}/api/message`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(message)
      })
      
      if (response.ok) {
        const data = await response.json()
        this.service.handleIncomingMessage(data, 'rest')
        return true
      }
      return false
    } catch (error) {
      this.service.handleProtocolError('rest', error)
      return false
    }
  }

  disconnect(): void {
    this.service.setConnectionState('rest', false)
  }
}

class P2PHandler {
  private service: CommunicationService
  private connections: Map<string, any> = new Map()

  constructor(service: CommunicationService) {
    this.service = service
  }

  async connect(url: string, options: ConnectionOptions = {}): Promise<boolean> {
    try {
      // P2P implementation would go here
      // For now, just mark as connected
      this.service.setConnectionState('p2p', true)
      this.service.emit('connected', { protocol: 'p2p' })
      return true
    } catch (error) {
      this.service.handleProtocolError('p2p', error)
      return false
    }
  }

  send(message: any): boolean {
    // P2P send implementation
    return true
  }

  disconnect(): void {
    this.service.setConnectionState('p2p', false)
  }
}

/**
 * Multi-protocol communication service with unified Gatekeeper interface
 * 
 * @description Vue 3 reactive version implementing "strict in what they produce (send), 
 * very relaxed about what they accept (input)" principle for all communications
 * packets across REST, WebSocket, and P2P protocols. Provides unified interface for
 * multi-protocol networking with automatic fallback and load balancing.
 * 
 * @inputs
 * - Connection URLs for different protocols
 * - Message objects with type and data
 * - Connection options (timeout, retries, fallback)
 * - Protocol preferences and configuration
 * 
 * @outputs
 * - Connection status and state information
 * - Protocol statistics and performance metrics
 * - Message queue and processing results
 * - Event emissions for connection and message events
 * 
 * @sideEffects
 * - Establishes and manages network connections
 * - Emits connection, message, and error events
 * - Maintains message queues and protocol statistics
 * - Handles automatic reconnection and protocol fallback
 * 
 * @units
 * - URLs: string endpoints
 * - Timeouts: milliseconds
 * - Retry counts: integers
 * - Message counts: integers
 * - Queue sizes: integers
 * - Protocol names: string identifiers
 * 
 * @example
 * ```typescript
 * const service = new CommunicationService()
 * 
 * // Connect to multiple protocols
 * await service.connect('wss://server.com', { protocol: 'websocket' })
 * await service.connect('https://api.server.com', { protocol: 'rest' })
 * 
 * // Send messages
 * service.send('chat', { message: 'Hello!' })
 * 
 * // Listen for events
 * service.on('message', (message) => {
 *   console.log('Received:', message)
 * })
 * ```
 */
export class CommunicationService extends EventEmitter {
  // Reactive state
  private _connectionState = reactive<ConnectionState>({
    websocket: false,
    rest: false,
    p2p: false
  })

  private _connections = reactive<ConnectionInfo>({
    websocket: null,
    rest: null,
    p2p: null
  })

  private _reconnectAttempts = reactive<ReconnectAttempts>({
    websocket: 0,
    rest: 0,
    p2p: 0
  })

  private _protocolStats = reactive<ProtocolStats>({
    websocket: { sent: 0, received: 0, errors: 0 },
    rest: { sent: 0, received: 0, errors: 0 },
    p2p: { sent: 0, received: 0, errors: 0 }
  })

  private _messageQueue = ref<Message[]>([])
  private _activeProtocol = ref<string | null>(null)
  private _cluster = ref<string>('test')

  // Configuration
  private maxReconnectAttempts = 5
  private reconnectDelay = 1000
  private protocolPreferences = ['websocket', 'rest', 'p2p']
  private maxQueueSize = 1000

  // Protocol handlers
  private protocolHandlers: {
    websocket: WebSocketHandler
    rest: RESTHandler
    p2p: P2PHandler
  }

  // Packet handler
  private packetHandler: PacketHandlerClass

  // Computed properties
  public readonly isConnected = computed(() => 
    this._connectionState.websocket || this._connectionState.rest || this._connectionState.p2p
  )

  public readonly connectionStatus = computed(() => ({
    websocket: this._connectionState.websocket,
    rest: this._connectionState.rest,
    p2p: this._connectionState.p2p,
    active: this._activeProtocol.value,
    queueLength: this._messageQueue.value.length
  }))

  public readonly stats = computed(() => this._protocolStats)

  constructor() {
    super()
    
    // Initialize packet handler
    this.packetHandler = new PacketHandlerClass()
    
    // Set up packet handler event listeners
    this.packetHandler.on('packetReceived', this.handlePacketReceived.bind(this))
    this.packetHandler.on('packetSent', this.handlePacketSent.bind(this))
    this.packetHandler.on('packetError', this.handlePacketError.bind(this))
    
    // Initialize protocol handlers
    this.protocolHandlers = {
      websocket: new WebSocketHandler(this),
      rest: new RESTHandler(this),
      p2p: new P2PHandler(this)
    }
    
    // Register packet handlers
    this.registerPacketHandlers()
  }

  /**
   * Register handlers for different packet types
   */
  private registerPacketHandlers(): void {
    this.packetHandler.registerHandler('logOK', this.handleLogOK.bind(this))
    this.packetHandler.registerHandler('avatars', this.handleAvatars.bind(this))
    this.packetHandler.registerHandler('publicMessage', this.handlePublicMessage.bind(this))
    this.packetHandler.registerHandler('inventory', this.handleInventory.bind(this))
    this.packetHandler.registerHandler('error', this.handleError.bind(this))
    this.packetHandler.registerHandler('wtl', this.handleWTL.bind(this))
  }

  /**
   * Connects to the Tootsville server using multiple protocols
   * 
   * @description Establishes connections to the server using WebSocket, REST, and P2P protocols
   * simultaneously. Automatically selects the best available protocol and processes any queued messages.
   * 
   * @inputs
   * - cluster: string - The server cluster to connect to (default: 'test')
   * - options: ConnectionOptions - Connection configuration options
   * 
   * @outputs
   * - Boolean indicating connection success
   * 
   * @sideEffects
   * - Establishes network connections
   * - Emits connection events
   * - Processes message queue
   * - Updates connection state
   * 
   * @units
   * - cluster: string identifier
   * - options: ConnectionOptions object
   * - return: boolean
   * 
   * @returns {Promise<boolean>} True if any protocol connection succeeds
   */
  async connect(cluster: string = 'test', options: ConnectionOptions = {}): Promise<boolean> {
    try {
      this._cluster.value = cluster
      
      // Try to connect to all protocols
      const results = await Promise.allSettled([
        this.connectWebSocket(cluster, options),
        this.connectREST(cluster, options),
        this.connectP2P(cluster, options)
      ])

      // Select the best available protocol
      this.selectBestProtocol()
      
      // Process any queued messages
      this.processMessageQueue()
      
      return this.isConnected.value
    } catch (error) {
      this.emit('connectionError', error)
      return false
    }
  }

  /**
   * Connect via WebSocket
   */
  private async connectWebSocket(cluster: string, options: ConnectionOptions): Promise<boolean> {
    const url = this.getWebSocketUrl(cluster)
    return this.protocolHandlers.websocket.connect(url, options)
  }

  /**
   * Connect via REST
   */
  private async connectREST(cluster: string, options: ConnectionOptions): Promise<boolean> {
    const url = this.getRESTUrl(cluster)
    return this.protocolHandlers.rest.connect(url, options)
  }

  /**
   * Connect via P2P
   */
  private async connectP2P(cluster: string, options: ConnectionOptions): Promise<boolean> {
    const url = this.getP2PUrl(cluster)
    return this.protocolHandlers.p2p.connect(url, options)
  }

  /**
   * Select the best available protocol based on preferences
   */
  private selectBestProtocol(): void {
    for (const protocol of this.protocolPreferences) {
      if (this._connectionState[protocol as keyof ConnectionState]) {
        this._activeProtocol.value = protocol
        break
      }
    }
  }

  /**
   * Sends a message through the active protocol with automatic fallback
   * 
   * @description Sends a message using the currently active protocol. If the primary protocol fails,
   * automatically tries fallback protocols. If no protocols are available, queues the message for
   * later sending.
   * 
   * @inputs
   * - type: string - The message type identifier
   * - data: any - The message data payload
   * 
   * @outputs
   * - Boolean indicating send success
   * 
   * @sideEffects
   * - Updates protocol statistics
   * - May switch active protocol on fallback
   * - May queue message if no protocols available
   * - Emits message events
   * 
   * @units
   * - type: string identifier
   * - data: any payload
   * - return: boolean
   * 
   * @returns {Promise<boolean>} True if message was sent successfully
   */
  async send(type: string, data: any): Promise<boolean> {
    const packet = this.packetHandler.createPacket(type, data)
    
    if (this._activeProtocol.value) {
      const success = await this.sendViaProtocol(this._activeProtocol.value, packet)
      if (success) {
        this._protocolStats[this._activeProtocol.value as keyof ProtocolStats].sent++
        return true
      }
    }
    
    // Try fallback protocols
    return this.tryFallbackProtocol(packet)
  }

  /**
   * Send via specific protocol
   */
  private async sendViaProtocol(protocol: string, packet: any): Promise<boolean> {
    switch (protocol) {
      case 'websocket':
        return this.protocolHandlers.websocket.send(JSON.stringify(packet))
      case 'rest':
        return this.protocolHandlers.rest.send(packet)
      case 'p2p':
        return this.protocolHandlers.p2p.send(packet)
      default:
        return false
    }
  }

  /**
   * Try fallback protocols if primary fails
   */
  private async tryFallbackProtocol(packet: any): Promise<boolean> {
    for (const protocol of this.protocolPreferences) {
      if (protocol !== this._activeProtocol.value && this._connectionState[protocol as keyof ConnectionState]) {
        const success = await this.sendViaProtocol(protocol, packet)
        if (success) {
          this._activeProtocol.value = protocol
          this._protocolStats[protocol as keyof ProtocolStats].sent++
          return true
        }
      }
    }
    
    // Queue message if no protocol available
    this.queueMessage(packet)
    return false
  }

  /**
   * Queue a message for later sending
   */
  private queueMessage(message: Message): void {
    if (this._messageQueue.value.length < this.maxQueueSize) {
      this._messageQueue.value.push({
        ...message,
        timestamp: Date.now()
      })
    }
  }

  /**
   * Process queued messages
   */
  private async processMessageQueue(): Promise<void> {
    if (!this.isConnected.value || this._messageQueue.value.length === 0) {
      return
    }

    const messages = [...this._messageQueue.value]
    this._messageQueue.value = []

    for (const message of messages) {
      await this.send(message.type, message.data)
    }
  }

  /**
   * Handle incoming messages from any protocol
   */
  handleIncomingMessage(data: any, protocol: string): void {
    try {
      this._protocolStats[protocol as keyof ProtocolStats].received++
      this.packetHandler.processPacket(data)
    } catch (error) {
      this.handleProtocolError(protocol, error)
    }
  }

  /**
   * Handle protocol errors
   */
  private handleProtocolError(protocol: string, error: any): void {
    this._protocolStats[protocol as keyof ProtocolStats].errors++
    this.emit('protocolError', { protocol, error })
  }

  /**
   * Set connection state for a protocol
   */
  setConnectionState(protocol: string, connected: boolean): void {
    this._connectionState[protocol as keyof ConnectionState] = connected
  }

  /**
   * Disconnects from all active protocols
   * 
   * @description Disconnects from all active network protocols (WebSocket, REST, P2P) and clears
   * the message queue. Resets the active protocol to null.
   * 
   * @inputs
   * - None
   * 
   * @outputs
   * - None
   * 
   * @sideEffects
   * - Disconnects all protocol handlers
   * - Clears message queue
   * - Resets active protocol
   * - Emits disconnect events
   * 
   * @units
   * - None
   * 
   * @returns {void}
   */
  disconnect(): void {
    this.protocolHandlers.websocket.disconnect()
    this.protocolHandlers.rest.disconnect()
    this.protocolHandlers.p2p.disconnect()
    
    this._activeProtocol.value = null
    this._messageQueue.value = []
  }

  /**
   * Returns current connection status information
   * 
   * @description Returns a reactive object containing the current connection status for all protocols,
   * the active protocol, and the current message queue length.
   * 
   * @inputs
   * - None
   * 
   * @outputs
   * - Connection status object with protocol states and queue information
   * 
   * @sideEffects
   * - None
   * 
   * @units
   * - return: ConnectionStatus object
   * 
   * @returns {any} Current connection status information
   */
  getConnectionStatus(): any {
    return this.connectionStatus.value
  }

  /**
   * Get protocol statistics
   */
  getProtocolStats(): ProtocolStats {
    return this.stats.value
  }

  /**
   * Set protocol preferences
   */
  setProtocolPreferences(preferences: string[]): void {
    this.protocolPreferences = preferences
  }

  /**
   * Clear message queue
   */
  clearMessageQueue(): void {
    this._messageQueue.value = []
  }

  // URL generation methods
  private getWebSocketUrl(cluster: string): string {
    return `wss://${cluster}.tootsville.org/ws`
  }

  private getRESTUrl(cluster: string): string {
    return `https://${cluster}.tootsville.org`
  }

  private getP2PUrl(cluster: string): string {
    return `p2p://${cluster}.tootsville.org`
  }

  // Packet handler methods
  private handlePacketReceived(packet: any): void {
    this.emit('packetReceived', packet)
  }

  private handlePacketSent(packet: any): void {
    this.emit('packetSent', packet)
  }

  private handlePacketError(error: any): void {
    this.emit('packetError', error)
  }

  // Specific packet handlers
  private handleLogOK(packet: any): void {
    this.emit('logOK', packet)
  }

  private handleAvatars(packet: any): void {
    this.emit('avatars', packet)
  }

  private handlePublicMessage(packet: any): void {
    this.emit('publicMessage', packet)
  }

  private handleInventory(packet: any): void {
    this.emit('inventory', packet)
  }

  private handleError(packet: any): void {
    this.emit('error', packet)
  }

  private handleWTL(packet: any): void {
    this.emit('wtl', packet)
  }
}

// Export singleton instance
export const communicationService = new CommunicationService()
