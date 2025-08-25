/**
 * CommunicationService.js - Multi-Protocol Communication Service with Gatekeeper
 * 
 * Supports REST, WebSocket, and Peer-to-Peer networking simultaneously
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { EventEmitter } from 'events';
import PacketHandler from './PacketHandler';

/**
 * Multi-protocol communication service with unified Gatekeeper interface
 * 
 * Implements "strict in what they produce (send), very relaxed about what they accept (input)"
 * principle for all communications packets across REST, WebSocket, and P2P protocols.
 */
class CommunicationService extends EventEmitter {
  constructor() {
    super();
    
    // Connection state
    this.connections = {
      websocket: null,
      rest: null,
      p2p: null
    };
    
    this.isConnected = {
      websocket: false,
      rest: false,
      p2p: false
    };
    
    this.reconnectAttempts = {
      websocket: 0,
      rest: 0,
      p2p: 0
    };
    
    this.maxReconnectAttempts = 5;
    this.reconnectDelay = 1000;
    this.cluster = 'test';
    
    // Protocol preferences and fallback order
    this.protocolPreferences = ['websocket', 'rest', 'p2p'];
    this.activeProtocol = null;
    
    // Message queue for offline/fallback scenarios
    this.messageQueue = [];
    this.maxQueueSize = 1000;
    
    // Initialize packet handler
    this.packetHandler = new PacketHandler();
    
    // Set up packet handler event listeners
    this.packetHandler.on('packetReceived', this.handlePacketReceived.bind(this));
    this.packetHandler.on('packetSent', this.handlePacketSent.bind(this));
    this.packetHandler.on('packetError', this.handlePacketError.bind(this));
    
    // Register packet handlers
    this.registerPacketHandlers();
    
    // Initialize protocol handlers
    this.initializeProtocolHandlers();
  }

  /**
   * Initialize protocol-specific handlers
   */
  initializeProtocolHandlers() {
    this.protocolHandlers = {
      websocket: new WebSocketHandler(this),
      rest: new RESTHandler(this),
      p2p: new P2PHandler(this)
    };
  }

  /**
   * Register handlers for different packet types
   */
  registerPacketHandlers() {
    // Register handlers for specific packet types
    this.packetHandler.registerHandler('logOK', this.handleLogOK.bind(this));
    this.packetHandler.registerHandler('avatars', this.handleAvatars.bind(this));
    this.packetHandler.registerHandler('publicMessage', this.handlePublicMessage.bind(this));
    this.packetHandler.registerHandler('inventory', this.handleInventory.bind(this));
    this.packetHandler.registerHandler('error', this.handleError.bind(this));
    this.packetHandler.registerHandler('wtl', this.handleWTL.bind(this));
  }

  /**
   * Connect to the Tootsville server using multiple protocols
   * @param {string} cluster - Server cluster to connect to
   * @param {Object} options - Connection options
   */
  async connect(cluster = 'test', options = {}) {
    try {
      this.cluster = cluster;
      
      // Connect to all available protocols
      const connectionPromises = [];
      
      if (options.websocket !== false) {
        connectionPromises.push(this.connectWebSocket(cluster));
      }
      
      if (options.rest !== false) {
        connectionPromises.push(this.connectREST(cluster));
      }
      
      if (options.p2p !== false) {
        connectionPromises.push(this.connectP2P(cluster));
      }
      
      // Wait for at least one connection to succeed
      const results = await Promise.allSettled(connectionPromises);
      
      // Determine the best available protocol
      this.selectBestProtocol();
      
      // Process queued messages if we now have a connection
      if (this.activeProtocol) {
        this.processMessageQueue();
      }
      
      this.emit('connected', { protocol: this.activeProtocol, allResults: results });
      
    } catch (error) {
      console.error('Failed to connect:', error);
      this.emit('error', { message: 'Failed to establish connection', error });
      throw error;
    }
  }

  /**
   * Connect via WebSocket
   * @param {string} cluster - Server cluster
   */
  async connectWebSocket(cluster) {
    try {
      const wsUrl = this.getWebSocketUrl(cluster);
      this.connections.websocket = new WebSocket(wsUrl);
      
      return new Promise((resolve, reject) => {
        const timeout = setTimeout(() => {
          reject(new Error('WebSocket connection timeout'));
        }, 10000);
        
        this.connections.websocket.onopen = () => {
          clearTimeout(timeout);
          console.log('WebSocket connected to Tootsville server');
          this.isConnected.websocket = true;
          this.reconnectAttempts.websocket = 0;
          this.emit('websocketConnected');
          resolve();
        };
        
        this.connections.websocket.onmessage = (event) => {
          this.handleIncomingMessage(event, 'websocket');
        };
        
        this.connections.websocket.onclose = () => {
          console.log('WebSocket disconnected from Tootsville server');
          this.isConnected.websocket = false;
          this.emit('websocketDisconnected');
          this.attemptReconnect('websocket');
        };
        
        this.connections.websocket.onerror = (error) => {
          clearTimeout(timeout);
          console.error('WebSocket error:', error);
          this.emit('error', { message: 'WebSocket connection error', error, protocol: 'websocket' });
          reject(error);
        };
      });
      
    } catch (error) {
      console.error('WebSocket connection failed:', error);
      throw error;
    }
  }

  /**
   * Connect via REST API
   * @param {string} cluster - Server cluster
   */
  async connectREST(cluster) {
    try {
      const restUrl = this.getRESTUrl(cluster);
      
      // Test REST connectivity
      const response = await fetch(`${restUrl}/health`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json'
        }
      });
      
      if (response.ok) {
        console.log('REST API connected to Tootsville server');
        this.isConnected.rest = true;
        this.reconnectAttempts.rest = 0;
        this.emit('restConnected');
        return;
      } else {
        throw new Error(`REST health check failed: ${response.status}`);
      }
      
    } catch (error) {
      console.error('REST connection failed:', error);
      throw error;
    }
  }

  /**
   * Connect via Peer-to-Peer
   * @param {string} cluster - Server cluster
   */
  async connectP2P(cluster) {
    try {
      // Initialize WebRTC for P2P connections
      if (!navigator.mediaDevices || !window.RTCPeerConnection) {
        throw new Error('WebRTC not supported');
      }
      
      // Initialize P2P network discovery
      await this.initializeP2PDiscovery(cluster);
      
      console.log('P2P network initialized');
      this.isConnected.p2p = true;
      this.reconnectAttempts.p2p = 0;
      this.emit('p2pConnected');
      
    } catch (error) {
      console.error('P2P connection failed:', error);
      throw error;
    }
  }

  /**
   * Initialize P2P network discovery
   * @param {string} cluster - Server cluster
   */
  async initializeP2PDiscovery(cluster) {
    // This would integrate with a P2P discovery service
    // For now, we'll simulate P2P connectivity
    return new Promise((resolve) => {
      setTimeout(resolve, 100);
    });
  }

  /**
   * Select the best available protocol based on preferences and status
   */
  selectBestProtocol() {
    for (const protocol of this.protocolPreferences) {
      if (this.isConnected[protocol]) {
        this.activeProtocol = protocol;
        console.log(`Selected ${protocol} as active protocol`);
        return;
      }
    }
    this.activeProtocol = null;
    console.warn('No protocols are currently connected');
  }

  /**
   * Disconnect from all protocols
   */
  disconnect() {
    // Disconnect WebSocket
    if (this.connections.websocket) {
      this.connections.websocket.close();
      this.connections.websocket = null;
    }
    
    // Disconnect REST (no persistent connection to close)
    this.isConnected.rest = false;
    
    // Disconnect P2P
    this.isConnected.p2p = false;
    
    // Reset connection states
    Object.keys(this.isConnected).forEach(protocol => {
      this.isConnected[protocol] = false;
      this.reconnectAttempts[protocol] = 0;
    });
    
    this.activeProtocol = null;
    this.emit('disconnected');
  }

  /**
   * Send a command through the best available protocol
   * @param {string} command - Command name
   * @param {Object} data - Command data
   * @param {string} protocol - Specific protocol to use (optional)
   */
  async send(command, data = {}, protocol = null) {
    const targetProtocol = protocol || this.activeProtocol;
    
    if (!targetProtocol) {
      // Queue message for later if no protocol is available
      this.queueMessage(command, data);
      console.warn('Message queued - no protocols connected');
      return;
    }
    
    try {
      const packet = this.packetHandler.createOutgoingPacket('command', {
        c: command,
        d: data
      });
      
      switch (targetProtocol) {
        case 'websocket':
          await this.sendWebSocket(packet);
          break;
        case 'rest':
          await this.sendREST(command, data);
          break;
        case 'p2p':
          await this.sendP2P(packet);
          break;
        default:
          throw new Error(`Unknown protocol: ${targetProtocol}`);
      }
      
    } catch (error) {
      console.error(`Failed to send via ${targetProtocol}:`, error);
      
      // Try fallback to other protocols
      await this.tryFallbackProtocol(command, data, targetProtocol);
    }
  }

  /**
   * Send via WebSocket
   * @param {Object} packet - Packet to send
   */
  async sendWebSocket(packet) {
    if (!this.isConnected.websocket || !this.connections.websocket) {
      throw new Error('WebSocket not connected');
    }
    
    this.connections.websocket.send(JSON.stringify(packet));
  }

  /**
   * Send via REST API
   * @param {string} command - Command name
   * @param {Object} data - Command data
   */
  async sendREST(command, data) {
    if (!this.isConnected.rest) {
      throw new Error('REST not connected');
    }
    
    const restUrl = this.getRESTUrl(this.cluster);
    const response = await fetch(`${restUrl}/api/${command}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(data)
    });
    
    if (!response.ok) {
      throw new Error(`REST request failed: ${response.status}`);
    }
    
    const result = await response.json();
    this.handleIncomingMessage({ data: JSON.stringify(result) }, 'rest');
  }

  /**
   * Send via P2P
   * @param {Object} packet - Packet to send
   */
  async sendP2P(packet) {
    if (!this.isConnected.p2p) {
      throw new Error('P2P not connected');
    }
    
    // Broadcast to all connected peers
    // This is a simplified implementation
    console.log('P2P broadcast:', packet);
  }

  /**
   * Try fallback to other protocols if primary fails
   * @param {string} command - Command name
   * @param {Object} data - Command data
   * @param {string} failedProtocol - Protocol that failed
   */
  async tryFallbackProtocol(command, data, failedProtocol) {
    const fallbackProtocols = this.protocolPreferences.filter(p => p !== failedProtocol);
    
    for (const protocol of fallbackProtocols) {
      if (this.isConnected[protocol]) {
        try {
          console.log(`Trying fallback to ${protocol}`);
          await this.send(command, data, protocol);
          return;
        } catch (error) {
          console.error(`Fallback to ${protocol} failed:`, error);
        }
      }
    }
    
    // If all protocols fail, queue the message
    this.queueMessage(command, data);
    this.emit('error', { message: 'All protocols failed, message queued' });
  }

  /**
   * Queue message for later transmission
   * @param {string} command - Command name
   * @param {Object} data - Command data
   */
  queueMessage(command, data) {
    if (this.messageQueue.length >= this.maxQueueSize) {
      this.messageQueue.shift(); // Remove oldest message
    }
    
    this.messageQueue.push({
      command,
      data,
      timestamp: Date.now()
    });
  }

  /**
   * Process queued messages
   */
  async processMessageQueue() {
    if (!this.activeProtocol || this.messageQueue.length === 0) {
      return;
    }
    
    const messages = [...this.messageQueue];
    this.messageQueue = [];
    
    for (const message of messages) {
      try {
        await this.send(message.command, message.data, this.activeProtocol);
      } catch (error) {
        console.error('Failed to send queued message:', error);
        // Re-queue failed messages
        this.queueMessage(message.command, message.data);
      }
    }
  }

  /**
   * Handle incoming messages from any protocol
   * @param {MessageEvent|Object} event - Message event or data
   * @param {string} protocol - Source protocol
   */
  handleIncomingMessage(event, protocol = 'unknown') {
    try {
      const rawData = typeof event.data === 'string' ? JSON.parse(event.data) : event.data;
      
      // Process packet with relaxed validation
      const processedPacket = this.packetHandler.handleIncomingPacket(rawData);
      
      // Add protocol metadata
      processedPacket._protocol = protocol;
      processedPacket._timestamp = Date.now();
      
      // Route to appropriate handler
      const packetType = this.packetHandler.determinePacketType(processedPacket);
      const handler = this.packetHandler.getHandler(packetType);
      
      if (handler) {
        handler(processedPacket);
      } else {
        // Emit for general handling
        this.emit(packetType, processedPacket);
      }
      
    } catch (error) {
      console.error('Failed to parse incoming message:', error);
      this.emit('error', { message: 'Failed to parse incoming message', error, protocol });
    }
  }

  /**
   * Handle packet received events from PacketHandler
   * @param {Object} event - Packet received event
   */
  handlePacketReceived(event) {
    console.debug('Packet received:', event.type, event.data);
    this.emit('packetReceived', event);
  }

  /**
   * Handle packet sent events from PacketHandler
   * @param {Object} event - Packet sent event
   */
  handlePacketSent(event) {
    console.debug('Packet sent:', event.type, event.data);
    this.emit('packetSent', event);
  }

  /**
   * Handle packet error events from PacketHandler
   * @param {Object} event - Packet error event
   */
  handlePacketError(event) {
    console.error('Packet error:', event.error);
    this.emit('packetError', event);
  }

  /**
   * Handle logOK packets
   * @param {Object} packet - Processed logOK packet
   */
  handleLogOK(packet) {
    if (packet.status) {
      console.log('Login successful');
      this.emit('loginSuccess', packet);
    } else {
      console.error('Login failed:', packet.error);
      this.emit('loginFailed', packet);
    }
  }

  /**
   * Handle avatar packets
   * @param {Object} packet - Processed avatar packet
   */
  handleAvatars(packet) {
    if (packet.status) {
      this.emit('avatarsUpdate', packet.avatars || {});
    } else {
      console.error('Avatar update failed:', packet.error);
    }
  }

  /**
   * Handle public message packets
   * @param {Object} packet - Processed public message packet
   */
  handlePublicMessage(packet) {
    if (packet.status) {
      this.emit('publicMessage', {
        message: packet.message,
        author: packet.author,
        world: packet.world,
        position: {
          lat: packet.lat,
          long: packet.long,
          alt: packet.alt
        }
      });
    }
  }

  /**
   * Handle inventory packets
   * @param {Object} packet - Processed inventory packet
   */
  handleInventory(packet) {
    if (packet.status) {
      this.emit('inventoryUpdate', {
        items: packet.items || [],
        equipped: packet.equipped || {},
        capacity: packet.capacity || 0
      });
    }
  }

  /**
   * Handle error packets
   * @param {Object} packet - Processed error packet
   */
  handleError(packet) {
    console.error('Server error:', packet.error);
    this.emit('serverError', packet);
  }

  /**
   * Handle WTL (Walk The Line) movement packets
   * @param {Object} packet - Processed WTL packet
   */
  handleWTL(packet) {
    if (packet.status !== false) {
      this.emit('movementUpdate', {
        character: packet.n,
        uuid: packet.u,
        course: packet.course,
        facing: packet.facing
      });
    }
  }

  /**
   * Get WebSocket URL for cluster
   * @param {string} cluster - Server cluster
   * @returns {string} WebSocket URL
   */
  getWebSocketUrl(cluster) {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host;
    return `${protocol}//${host}/world/websocket`;
  }

  /**
   * Get REST API URL for cluster
   * @param {string} cluster - Server cluster
   * @returns {string} REST API URL
   */
  getRESTUrl(cluster) {
    const protocol = window.location.protocol;
    const host = window.location.host;
    return `${protocol}//${host}`;
  }

  /**
   * Attempt to reconnect a specific protocol
   * @param {string} protocol - Protocol to reconnect
   */
  attemptReconnect(protocol) {
    if (this.reconnectAttempts[protocol] < this.maxReconnectAttempts) {
      this.reconnectAttempts[protocol]++;
      console.log(`Attempting to reconnect ${protocol} (${this.reconnectAttempts[protocol]}/${this.maxReconnectAttempts})`);
      
      setTimeout(() => {
        this.connect(this.cluster, { [protocol]: true }).catch(error => {
          console.error(`${protocol} reconnection failed:`, error);
        });
      }, this.reconnectDelay * this.reconnectAttempts[protocol]);
    } else {
      console.error(`Max ${protocol} reconnection attempts reached`);
      this.emit('error', { message: `Max ${protocol} reconnection attempts reached` });
    }
  }

  /**
   * Get connection status for all protocols
   * @returns {Object} Connection status for each protocol
   */
  getConnectionStatus() {
    return {
      ...this.isConnected,
      activeProtocol: this.activeProtocol,
      queuedMessages: this.messageQueue.length
    };
  }

  /**
   * Get packet error statistics
   * @returns {Object} Error statistics
   */
  getPacketErrorStats() {
    return this.packetHandler.getErrorStats();
  }

  /**
   * Reset packet error tracking
   */
  resetPacketErrorTracking() {
    this.packetHandler.resetErrorTracking();
  }

  /**
   * Add custom packet schema
   * @param {string} type - Packet type
   * @param {Object} schema - Schema definition
   */
  addPacketSchema(type, schema) {
    this.packetHandler.addSchema(type, schema);
  }

  /**
   * Register custom packet handler
   * @param {string} type - Packet type
   * @param {Function} handler - Handler function
   */
  registerPacketHandler(type, handler) {
    this.packetHandler.registerHandler(type, handler);
  }

  /**
   * Set protocol preferences
   * @param {Array} preferences - Array of protocol names in order of preference
   */
  setProtocolPreferences(preferences) {
    this.protocolPreferences = preferences;
    this.selectBestProtocol();
  }

  /**
   * Get protocol statistics
   * @returns {Object} Protocol statistics
   */
  getProtocolStats() {
    return {
      activeProtocol: this.activeProtocol,
      preferences: this.protocolPreferences,
      connectionStatus: this.isConnected,
      reconnectAttempts: this.reconnectAttempts,
      queuedMessages: this.messageQueue.length
    };
  }
}

/**
 * WebSocket Protocol Handler
 */
class WebSocketHandler {
  constructor(service) {
    this.service = service;
  }
  
  // WebSocket-specific methods can be added here
}

/**
 * REST Protocol Handler
 */
class RESTHandler {
  constructor(service) {
    this.service = service;
  }
  
  // REST-specific methods can be added here
}

/**
 * P2P Protocol Handler
 */
class P2PHandler {
  constructor(service) {
    this.service = service;
  }
  
  // P2P-specific methods can be added here
}

export default CommunicationService;
