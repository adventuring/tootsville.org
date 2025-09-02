/**
 * MeshNetworkService.ts - Infinity Protocol Mesh Networking Service
 * 
 * Implements the Gossip network as a series of actions in the Infinity protocol
 * pushed between nodes in a mesh formation, where each action is signed by its
 * authoring character with their private key and may be attested-to if rechecked
 * by other nodes by signing attestations with their key.
 * 
 * Public keys are published ONLY through tootsville.net servers, not via gossip.
 * Network cycles are prevented through TTL and via tracking.
 * Peer selection prioritizes low latency and world distance proximity.
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'
import { EventEmitter } from 'events'

// Types for Infinity Protocol
export interface InfinityAction {
  id: string
  type: string
  data: any
  author: string
  timestamp: number
  signature: string
  attestations: Attestation[]
  via: string[] // Tracks path to prevent cycles
  ttl: number // Time-to-live to prevent infinite loops
  recipient?: string
}

export interface Attestation {
  nodeId: string
  signature: string
  timestamp: number
  actionId: string
}

export interface PeerConnection {
  id: string
  connection: RTCPeerConnection
  dataChannel: RTCDataChannel | null
  publicKey: string
  isConnected: boolean
  lastSeen: number
  attestationCount: number
  latency: number // Ping time in milliseconds
  worldDistance: number // Distance in world coordinates
  worldPosition: { x: number; y: number; z: number }
}

export interface KeyPair {
  publicKey: string
  privateKey: string
  fingerprint: string
}

export interface MeshNode {
  id: string
  publicKey: string
  characterName: string
  lastSeen: number
  attestationTrust: number
  connectionCount: number
  worldPosition: { x: number; y: number; z: number }
  latency: number
}

export interface MeshStats {
  totalNodes: number
  connectedPeers: number
  actionsProcessed: number
  attestationsGiven: number
  attestationsReceived: number
  averageLatency: number
  networkHealth: number
  cyclePreventionCount: number
}

/**
 * Infinity Protocol Mesh Networking Service
 * 
 * @description Implements the Gossip network as a series of signed actions
 * pushed between nodes in a mesh formation with cryptographic attestations.
 * Each action is signed by its authoring character and may be attested-to
 * by other nodes for verification and trust establishment.
 * 
 * Key Features:
 * - Public keys published ONLY through tootsville.net servers
 * - Network cycle prevention via TTL and via tracking
 * - Peer selection based on latency and world distance
 * - Cryptographic signing and attestation system
 * 
 * @inputs
 * - Character credentials and key pairs
 * - Server endpoints for public key verification
 * - Mesh network configuration and topology
 * - Infinity protocol actions and data
 * - World position and latency information
 * 
 * @outputs
 * - Mesh network state and statistics
 * - Peer connection status and health
 * - Action processing results and attestations
 * - Network topology and routing information
 * 
 * @sideEffects
 * - Establishes and maintains peer-to-peer connections
 * - Signs and verifies Infinity protocol actions
 * - Manages cryptographic key pairs and attestations
 * - Broadcasts actions through the mesh network
 * - Maintains network topology and routing tables
 * - Prevents network cycles and optimizes peer selection
 * 
 * @units
 * - Node IDs: string identifiers
 * - Public keys: base64-encoded strings (published via tootsville.net only)
 * - Signatures: base64-encoded cryptographic signatures
 * - Timestamps: Unix timestamps (milliseconds)
 * - Latency: milliseconds
 * - World distance: coordinate units
 * - Trust scores: floating point values (0.0-1.0)
 * - Connection counts: integers
 * - TTL: hop count for cycle prevention
 * 
 * @example
 * ```typescript
 * const meshService = new MeshNetworkService()
 * 
 * // Initialize with character credentials
 * await meshService.initialize({
 *   characterName: 'Flora',
 *   characterId: 'uuid-123',
 *   serverEndpoint: 'https://tootsville.net',
 *   worldPosition: { x: 100, y: 50, z: 0 }
 * })
 * 
 * // Connect to mesh network
 * await meshService.connect()
 * 
 * // Send an Infinity action
 * const action = await meshService.createAction('chat', {
 *   message: 'Hello, Tootsville!',
 *   room: 'Grand Toot Station'
 * })
 * 
 * // Listen for incoming actions
 * meshService.on('action', (action) => {
 *   console.log('Received action:', action)
 * })
 * ```
 */
export class MeshNetworkService extends EventEmitter {
  // Reactive state
  private _peers = reactive<Map<string, PeerConnection>>(new Map())
  private _nodes = reactive<Map<string, MeshNode>>(new Map())
  private _actions = ref<InfinityAction[]>([])
  private _keyPair = ref<KeyPair | null>(null)
  private _isConnected = ref(false)
  private _isInitialized = ref(false)
  private _worldPosition = ref<{ x: number; y: number; z: number }>({ x: 0, y: 0, z: 0 })

  // Configuration
  private config = {
    minPeerConnections: 5,
    maxPeerConnections: 20,
    actionTimeout: 30000, // 30 seconds
    attestationThreshold: 3,
    maxActionHistory: 1000,
    heartbeatInterval: 30000, // 30 seconds
    reconnectionDelay: 5000, // 5 seconds
    maxTTL: 10, // Maximum hops to prevent cycles
    latencyWeight: 0.6, // Weight for latency in peer selection
    distanceWeight: 0.4, // Weight for world distance in peer selection
    maxLatency: 1000, // Maximum acceptable latency (ms)
    maxWorldDistance: 1000 // Maximum acceptable world distance
  }

  // Network state
  private _stats = reactive<MeshStats>({
    totalNodes: 0,
    connectedPeers: 0,
    actionsProcessed: 0,
    attestationsGiven: 0,
    attestationsReceived: 0,
    averageLatency: 0,
    networkHealth: 0,
    cyclePreventionCount: 0
  })

  // Character information
  private characterInfo = {
    name: '',
    id: '',
    serverEndpoint: ''
  }

  // ICE servers for WebRTC
  private iceServers: RTCIceServer[] = []

  // Computed properties
  public readonly peers = computed(() => Array.from(this._peers.values()))
  public readonly nodes = computed(() => Array.from(this._nodes.values()))
  public readonly actions = computed(() => this._actions.value)
  public readonly stats = computed(() => this._stats)
  public readonly isConnected = computed(() => this._isConnected.value)
  public readonly isInitialized = computed(() => this._isInitialized.value)
  public readonly worldPosition = computed(() => this._worldPosition.value)

  constructor() {
    super()
    this.setupEventHandlers()
  }

  /**
   * Initialize the mesh network service with character credentials
   * 
   * @param characterInfo - Character identification and server information
   * @param worldPosition - Initial world position for proximity calculations
   */
  async initialize(characterInfo: {
    characterName: string
    characterId: string
    serverEndpoint: string
    worldPosition?: { x: number; y: number; z: number }
  }): Promise<void> {
    this.characterInfo = {
      name: characterInfo.characterName,
      id: characterInfo.characterId,
      serverEndpoint: characterInfo.serverEndpoint
    }
    
    if (characterInfo.worldPosition) {
      this._worldPosition.value = characterInfo.worldPosition
    }
    
    // Generate or load key pair
    await this.ensureKeyPair()
    
    // Fetch ICE servers from central server
    await this.fetchIceServers()
    
    // Initialize network topology
    this.initializeNetworkTopology()
    
    this._isInitialized.value = true
    this.emit('initialized', this.characterInfo)
  }

  /**
   * Connect to the mesh network
   */
  async connect(): Promise<void> {
    if (!this._isInitialized.value) {
      throw new Error('MeshNetworkService must be initialized before connecting')
    }

    try {
      // Connect to central server for peer discovery
      await this.connectToCentralServer()
      
      // Establish initial peer connections with optimized selection
      await this.establishPeerConnections()
      
      // Start mesh network maintenance
      this.startMeshMaintenance()
      
      this._isConnected.value = true
      this.emit('connected')
      
      console.log(`Connected to mesh network with ${this._peers.size} peers`)
    } catch (error) {
      console.error('Failed to connect to mesh network:', error)
      this.emit('error', error)
      throw error
    }
  }

  /**
   * Disconnect from the mesh network
   */
  async disconnect(): Promise<void> {
    // Close all peer connections
    for (const peer of this._peers.values()) {
      await this.closePeerConnection(peer)
    }
    
    // Stop mesh maintenance
    this.stopMeshMaintenance()
    
    this._isConnected.value = false
    this.emit('disconnected')
  }

  /**
   * Create and sign an Infinity protocol action
   * 
   * @param type - Action type (chat, movement, inventory, etc.)
   * @param data - Action data payload
   * @param recipient - Target recipient (default: '$World' for broadcast)
   * @param via - Path tracking to prevent cycles (auto-populated)
   * @returns Promise<InfinityAction> - Signed action ready for broadcast
   */
  async createAction(
    type: string,
    data: any,
    recipient: string = '$World',
    via: string[] = []
  ): Promise<InfinityAction> {
    if (!this._keyPair.value) {
      throw new Error('Key pair not available')
    }

    const action: InfinityAction = {
      id: this.generateActionId(),
      type,
      data,
      author: this.characterInfo.id,
      timestamp: Date.now(),
      signature: '',
      attestations: [],
      via: [...via, this.characterInfo.id], // Add current node to path
      ttl: this.config.maxTTL, // Set initial TTL
      recipient
    }

    // Sign the action
    action.signature = await this.signAction(action)
    
    // Add to local action history
    this._actions.value.push(action)
    if (this._actions.value.length > this.config.maxActionHistory) {
      this._actions.value.shift()
    }

    this.emit('actionCreated', action)
    return action
  }

  /**
   * Broadcast an action to the mesh network
   * 
   * @param action - The action to broadcast
   */
  async broadcastAction(action: InfinityAction): Promise<void> {
    // Verify action signature before broadcasting
    if (!await this.verifyActionSignature(action)) {
      throw new Error('Invalid action signature')
    }

    // Broadcast to all connected peers
    const broadcastPromises = Array.from(this._peers.values())
      .filter(peer => peer.isConnected && peer.dataChannel)
      .map(peer => this.sendActionToPeer(peer, action))

    await Promise.allSettled(broadcastPromises)
    
    this._stats.actionsProcessed++
    this.emit('actionBroadcast', action)
  }

  /**
   * Attest to an action from another node
   * 
   * @param actionId - ID of the action to attest to
   * @returns Promise<Attestation> - Signed attestation
   */
  async attestToAction(actionId: string): Promise<Attestation> {
    const action = this._actions.value.find(a => a.id === actionId)
    if (!action) {
      throw new Error('Action not found')
    }

    if (!this._keyPair.value) {
      throw new Error('Key pair not available')
    }

    const attestation: Attestation = {
      nodeId: this.characterInfo.id,
      signature: '',
      timestamp: Date.now(),
      actionId
    }

    // Sign the attestation
    attestation.signature = await this.signAttestation(attestation)
    
    // Add attestation to action
    action.attestations.push(attestation)
    
    // Broadcast attestation to mesh network
    await this.broadcastAttestation(attestation)
    
    this._stats.attestationsGiven++
    this.emit('attestationGiven', attestation)
    
    return attestation
  }

  /**
   * Process an incoming action from the mesh network
   * 
   * @param action - The incoming action to process
   */
  async processIncomingAction(action: InfinityAction): Promise<void> {
    // Verify action signature
    if (!await this.verifyActionSignature(action)) {
      console.warn('Received action with invalid signature:', action.id)
      return
    }

    // Check if we've already processed this action
    if (this._actions.value.some(a => a.id === action.id)) {
      return
    }

    // Check for network cycles
    if (action.via.includes(this.characterInfo.id)) {
      console.warn('Detected network cycle, dropping action:', action.id)
      this._stats.cyclePreventionCount++
      return
    }

    // Check TTL
    if (action.ttl <= 0) {
      console.warn('Action TTL expired, dropping:', action.id)
      return
    }

    // Decrement TTL
    action.ttl--

    // Add to action history
    this._actions.value.push(action)
    if (this._actions.value.length > this.config.maxActionHistory) {
      this._actions.value.shift()
    }

    // Process action based on type
    await this.handleActionByType(action)

    // Forward to other peers (mesh routing) with cycle prevention
    await this.forwardAction(action)

    this._stats.actionsProcessed++
    this.emit('actionReceived', action)
  }

  /**
   * Update world position for proximity-based peer selection
   * 
   * @param position - New world position
   */
  updateWorldPosition(position: { x: number; y: number; z: number }): void {
    this._worldPosition.value = position
    
    // Update peer distances
    for (const peer of this._peers.values()) {
      peer.worldDistance = this.calculateWorldDistance(position, peer.worldPosition)
    }
    
    // Re-evaluate peer connections based on new position
    this.optimizePeerConnections()
  }

  /**
   * Get network statistics and health information
   * 
   * @returns MeshStats - Current network statistics
   */
  getNetworkStats(): MeshStats {
    const connectedPeers = Array.from(this._peers.values())
      .filter(peer => peer.isConnected).length

    const totalNodes = this._nodes.size
    const networkHealth = this.calculateNetworkHealth()
    const averageLatency = this.calculateAverageLatency()

    return {
      ...this._stats,
      connectedPeers,
      totalNodes,
      networkHealth,
      averageLatency
    }
  }

  /**
   * Get peer connection information
   * 
   * @param peerId - Peer identifier
   * @returns PeerConnection | null - Peer connection info or null if not found
   */
  getPeerInfo(peerId: string): PeerConnection | null {
    return this._peers.get(peerId) || null
  }

  /**
   * Get node information from the mesh network
   * 
   * @param nodeId - Node identifier
   * @returns MeshNode | null - Node info or null if not found
   */
  getNodeInfo(nodeId: string): MeshNode | null {
    return this._nodes.get(nodeId) || null
  }

  // Private methods

  private setupEventHandlers(): void {
    // Handle peer connection events
    this.on('peerConnected', this.handlePeerConnected.bind(this))
    this.on('peerDisconnected', this.handlePeerDisconnected.bind(this))
    this.on('actionReceived', this.handleActionReceived.bind(this))
  }

  private async ensureKeyPair(): Promise<void> {
    // In a real implementation, this would generate or load from secure storage
    // For now, create a mock key pair
    this._keyPair.value = {
      publicKey: `public-key-${this.characterInfo.id}`,
      privateKey: `private-key-${this.characterInfo.id}`,
      fingerprint: `fingerprint-${this.characterInfo.id}`
    }
  }

  private async fetchIceServers(): Promise<void> {
    try {
      const response = await fetch(`${this.characterInfo.serverEndpoint}/gossip/ice-servers`)
      this.iceServers = await response.json()
    } catch (error) {
      console.warn('Failed to fetch ICE servers, using defaults')
      this.iceServers = [
        { urls: 'stun:stun.l.google.com:19302' }
      ]
    }
  }

  private initializeNetworkTopology(): void {
    // Initialize empty network topology
    this._nodes.clear()
    this._peers.clear()
  }

  private async connectToCentralServer(): Promise<void> {
    // Connect to central server for peer discovery and public key verification
    // This would typically use WebSocket or REST API
    console.log('Connecting to central server for peer discovery')
  }

  private async establishPeerConnections(): Promise<void> {
    // Get list of available peers from central server with latency and position data
    const availablePeers = await this.getAvailablePeers()
    
    // Sort peers by combined score (latency + distance)
    const scoredPeers = availablePeers
      .map(peer => ({
        ...peer,
        score: this.calculatePeerScore(peer)
      }))
      .sort((a, b) => b.score - a.score)
    
    // Establish connections to maintain minimum peer count
    const connectionsToEstablish = Math.min(
      this.config.minPeerConnections - this._peers.size,
      scoredPeers.length
    )

    for (let i = 0; i < connectionsToEstablish; i++) {
      const peerInfo = scoredPeers[i]
      await this.connectToPeer(peerInfo)
    }
  }

  private calculatePeerScore(peer: any): number {
    const latencyScore = Math.max(0, 1 - (peer.latency / this.config.maxLatency))
    const distanceScore = Math.max(0, 1 - (peer.worldDistance / this.config.maxWorldDistance))
    
    return (latencyScore * this.config.latencyWeight) + 
           (distanceScore * this.config.distanceWeight)
  }

  private calculateWorldDistance(pos1: { x: number; y: number; z: number }, 
                                pos2: { x: number; y: number; z: number }): number {
    const dx = pos1.x - pos2.x
    const dy = pos1.y - pos2.y
    const dz = pos1.z - pos2.z
    return Math.sqrt(dx * dx + dy * dy + dz * dz)
  }

  private calculateAverageLatency(): number {
    const connectedPeers = Array.from(this._peers.values())
      .filter(peer => peer.isConnected)
    
    if (connectedPeers.length === 0) return 0
    
    const totalLatency = connectedPeers.reduce((sum, peer) => sum + peer.latency, 0)
    return totalLatency / connectedPeers.length
  }

  private async connectToPeer(peerInfo: any): Promise<void> {
    const peerConnection = new RTCPeerConnection({ iceServers: this.iceServers })
    
    const peer: PeerConnection = {
      id: peerInfo.id,
      connection: peerConnection,
      dataChannel: null,
      publicKey: peerInfo.publicKey,
      isConnected: false,
      lastSeen: Date.now(),
      attestationCount: 0,
      latency: peerInfo.latency || 0,
      worldDistance: this.calculateWorldDistance(this._worldPosition.value, peerInfo.worldPosition),
      worldPosition: peerInfo.worldPosition || { x: 0, y: 0, z: 0 }
    }

    // Set up data channel
    const dataChannel = peerConnection.createDataChannel('infinity-protocol', {
      ordered: false,
      negotiated: false
    })

    dataChannel.onopen = () => {
      peer.isConnected = true
      peer.dataChannel = dataChannel
      this._peers.set(peer.id, peer)
      this.emit('peerConnected', peer)
    }

    dataChannel.onmessage = (event) => {
      this.handlePeerMessage(peer, event.data)
    }

    dataChannel.onclose = () => {
      peer.isConnected = false
      this._peers.delete(peer.id)
      this.emit('peerDisconnected', peer)
    }

    // Create and send offer
    const offer = await peerConnection.createOffer()
    await peerConnection.setLocalDescription(offer)
    
    // Send offer to peer (via central server or direct signaling)
    await this.sendOfferToPeer(peerInfo.id, offer)
  }

  private async closePeerConnection(peer: PeerConnection): Promise<void> {
    if (peer.dataChannel) {
      peer.dataChannel.close()
    }
    peer.connection.close()
    this._peers.delete(peer.id)
  }

  private startMeshMaintenance(): void {
    // Start heartbeat to maintain connections
    setInterval(() => {
      this.sendHeartbeat()
    }, this.config.heartbeatInterval)

    // Monitor network health
    setInterval(() => {
      this.updateNetworkHealth()
    }, 10000)

    // Optimize peer connections periodically
    setInterval(() => {
      this.optimizePeerConnections()
    }, 30000) // Every 30 seconds
  }

  private stopMeshMaintenance(): void {
    // Clear intervals (in a real implementation, store interval IDs)
  }

  private optimizePeerConnections(): void {
    const currentPeers = Array.from(this._peers.values())
    
    // If we have too many peers, remove the worst ones
    if (currentPeers.length > this.config.maxPeerConnections) {
      const sortedPeers = currentPeers
        .map(peer => ({ ...peer, score: this.calculatePeerScore(peer) }))
        .sort((a, b) => a.score - b.score)
      
      const peersToRemove = sortedPeers.slice(0, currentPeers.length - this.config.maxPeerConnections)
      
      peersToRemove.forEach(peer => {
        this.closePeerConnection(peer)
      })
    }
  }

  private generateActionId(): string {
    return `action-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }

  private async signAction(action: InfinityAction): Promise<string> {
    // In a real implementation, this would use proper cryptographic signing
    const payload = JSON.stringify({
      id: action.id,
      type: action.type,
      data: action.data,
      author: action.author,
      timestamp: action.timestamp
    })
    
    // Mock signature - replace with actual cryptographic signing
    return `signature-${btoa(payload)}`
  }

  private async verifyActionSignature(action: InfinityAction): Promise<boolean> {
    // In a real implementation, this would verify the cryptographic signature
    // For now, just check if signature exists
    return !!action.signature
  }

  private async signAttestation(attestation: Attestation): Promise<string> {
    // In a real implementation, this would use proper cryptographic signing
    const payload = JSON.stringify({
      nodeId: attestation.nodeId,
      actionId: attestation.actionId,
      timestamp: attestation.timestamp
    })
    
    // Mock signature - replace with actual cryptographic signing
    return `attestation-${btoa(payload)}`
  }

  private async sendActionToPeer(peer: PeerConnection, action: InfinityAction): Promise<void> {
    if (peer.dataChannel && peer.dataChannel.readyState === 'open') {
      peer.dataChannel.send(JSON.stringify({
        type: 'action',
        data: action
      }))
    }
  }

  private async broadcastAttestation(attestation: Attestation): Promise<void> {
    const message = {
      type: 'attestation',
      data: attestation
    }

    const broadcastPromises = Array.from(this._peers.values())
      .filter(peer => peer.isConnected && peer.dataChannel)
      .map(peer => {
        if (peer.dataChannel && peer.dataChannel.readyState === 'open') {
          peer.dataChannel.send(JSON.stringify(message))
        }
      })

    await Promise.allSettled(broadcastPromises)
  }

  private async handleActionByType(action: InfinityAction): Promise<void> {
    switch (action.type) {
      case 'chat':
        this.emit('chatMessage', action.data)
        break
      case 'movement':
        this.emit('playerMovement', action.data)
        break
      case 'inventory':
        this.emit('inventoryUpdate', action.data)
        break
      case 'attestation':
        this.handleAttestation(action.data)
        break
      default:
        this.emit('unknownAction', action)
    }
  }

  private async forwardAction(action: InfinityAction): Promise<void> {
    // Forward action to other peers (excluding the sender and nodes already in via path)
    const forwardPromises = Array.from(this._peers.values())
      .filter(peer => peer.isConnected && peer.dataChannel)
      .filter(peer => !action.via.includes(peer.id)) // Prevent cycles
      .map(peer => this.sendActionToPeer(peer, action))

    await Promise.allSettled(forwardPromises)
  }

  private handlePeerMessage(peer: PeerConnection, message: string): void {
    try {
      const parsed = JSON.parse(message)
      
      // Handle Infinity Protocol messages based on actual wire protocol
      // Messages have a 'from' field that indicates the message type
      if (parsed.from) {
        switch (parsed.from) {
          case 'pub':
            // Public message (speech)
            this.emit('chatMessage', {
              speaker: parsed.u,
              message: parsed.t,
              whisper: parsed.x === 'whisper',
              shout: parsed.x === 'shout'
            })
            break
            
          case 'wtl':
            // Walk The Line - avatar movement
            if (parsed.status) {
              this.emit('playerMovement', {
                name: parsed.n,
                uuid: parsed.u,
                course: parsed.course,
                facing: parsed.facing
              })
            }
            break
            
          case 'joinOK':
            // Player joined room
            if (parsed.status) {
              this.emit('playerJoined', {
                name: parsed.n,
                uuid: parsed.uLs,
                room: parsed.r
              })
            }
            break
            
          case 'bye':
            // Player left
            if (parsed.status) {
              this.emit('playerLeft', {
                name: parsed.n,
                uuid: parsed.u
              })
            }
            break
            
          case 'avatars':
            // Avatar information update
            this.emit('avatarUpdate', {
              avatars: parsed.avatars,
              room: parsed.inRoom
            })
            break
            
          case 'ping':
            // Ping/pong for latency measurement
            if (parsed.ping === 'ping') {
              // Send pong response using standard Infinity protocol
              if (peer.dataChannel && peer.dataChannel.readyState === 'open') {
                peer.dataChannel.send(JSON.stringify({
                  from: 'ping',
                  ping: 'pong',
                  pingStarted: parsed.pingStarted
                }))
              }
            } else if (parsed.ping === 'pong') {
              // Update latency
              if (parsed.pingStarted) {
                peer.latency = Date.now() - parsed.pingStarted
              }
            }
            break
            
          case 'rv':
            // Room variables update
            if (parsed.status) {
              this.emit('roomVariables', parsed.var)
            }
            break
            
          case 'c':
            // Command response
            this.emit('commandResponse', {
              status: parsed.status,
              command: parsed.command,
              error: parsed.error
            })
            break
            
          default:
            // Handle other message types by emitting them
            this.emit('message', {
              type: parsed.from,
              data: parsed,
              peer: peer.id
            })
        }
      } else if (parsed.c) {
        // Handle command messages
        this.emit('command', {
          command: parsed.c,
          data: parsed.d,
          user: parsed.u,
          room: parsed.r
        })
      } else if (parsed.seq) {
        // Handle sequence messages (multiple messages in one)
        for (const seqMessage of parsed.seq) {
          this.handlePeerMessage(peer, JSON.stringify(seqMessage))
        }
      } else {
        console.warn('Unknown message format from peer:', parsed)
      }
    } catch (error) {
      console.error('Failed to parse peer message:', error)
    }
  }

  private handleAttestation(attestation: Attestation): void {
    const action = this._actions.value.find(a => a.id === attestation.actionId)
    if (action) {
      action.attestations.push(attestation)
      this._stats.attestationsReceived++
      this.emit('attestationReceived', attestation)
    }
  }

  private async getAvailablePeers(): Promise<any[]> {
    // In a real implementation, this would fetch from central server
    // with latency and position information
    return []
  }

  private async sendOfferToPeer(peerId: string, offer: RTCSessionDescriptionInit): Promise<void> {
    // In a real implementation, this would send via central server or direct signaling
    console.log('Sending offer to peer:', peerId)
  }

  private sendHeartbeat(): void {
    const heartbeat = {
      type: 'heartbeat',
      timestamp: Date.now(),
      nodeId: this.characterInfo.id,
      latency: this.calculateAverageLatency(),
      position: this._worldPosition.value
    }

    Array.from(this._peers.values())
      .filter(peer => peer.isConnected && peer.dataChannel)
      .forEach(peer => {
        if (peer.dataChannel && peer.dataChannel.readyState === 'open') {
          peer.dataChannel.send(JSON.stringify(heartbeat))
        }
      })
  }

  private updateNetworkHealth(): void {
    const connectedPeers = Array.from(this._peers.values())
      .filter(peer => peer.isConnected).length

    const health = Math.min(connectedPeers / this.config.minPeerConnections, 1.0)
    this._stats.networkHealth = health

    // Attempt to reconnect if health is poor
    if (health < 0.5 && this._isConnected.value) {
      this.attemptReconnection()
    }
  }

  private calculateNetworkHealth(): number {
    const connectedPeers = Array.from(this._peers.values())
      .filter(peer => peer.isConnected).length

    return Math.min(connectedPeers / this.config.minPeerConnections, 1.0)
  }

  private async attemptReconnection(): Promise<void> {
    console.log('Attempting mesh network reconnection...')
    
    // Close existing connections
    for (const peer of this._peers.values()) {
      await this.closePeerConnection(peer)
    }
    
    // Wait before reconnecting
    await new Promise(resolve => setTimeout(resolve, this.config.reconnectionDelay))
    
    // Re-establish connections
    await this.establishPeerConnections()
  }

  private handlePeerConnected(peer: PeerConnection): void {
    console.log(`Peer connected: ${peer.id}`)
    this._stats.connectedPeers = this._peers.size
  }

  private handlePeerDisconnected(peer: PeerConnection): void {
    console.log(`Peer disconnected: ${peer.id}`)
    this._stats.connectedPeers = this._peers.size
  }

  private handleActionReceived(action: InfinityAction): void {
    console.log(`Action received: ${action.type} from ${action.author}`)
  }
}

export default MeshNetworkService
