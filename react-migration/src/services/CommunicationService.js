/**
 * CommunicationService - Preserves the existing Tootsville communications protocol
 * This service maintains 100% backwards compatibility with the server
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

/**
 * @class CommunicationService
 * @description WebSocket communication service for Tootsville client
 * 
 * This service handles all communication with the tootsville.net Lisp server
 * using the existing WebSocket protocol. It maintains backwards compatibility
 * while providing modern error handling and reconnection logic.
 * 
 * @example
 * const comm = new CommunicationService();
 * comm.connect('test');
 * comm.send('move', { x: 100, y: 50, z: 200 });
 */
class CommunicationService {
  constructor() {
    this.socket = null;
    this.isConnected = false;
    this.isAuthenticated = false;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
    this.reconnectDelay = 1000;
    this.messageQueue = [];
    this.eventHandlers = new Map();
    this.cluster = 'test';
    this.serverUrl = null;
  }

  /**
   * @method connect
   * @param {string} cluster - Server cluster name
   * @description Connect to the Tootsville server
   * 
   * Establishes WebSocket connection to the tootsville.net Lisp server.
   * Automatically determines the server URL based on the current domain
   * and cluster configuration.
   */
  connect(cluster = 'test') {
    this.cluster = cluster;
    
    // Determine server URL based on current domain
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.hostname;
    const port = window.location.port || (protocol === 'wss:' ? '443' : '80');
    
    // For development, connect to localhost:5000 (tootsville.net server)
    // For production, connect to the same host as the static files
    if (host === 'localhost' || host === '127.0.0.1') {
      this.serverUrl = `${protocol}//localhost:5000/websocket`;
    } else {
      this.serverUrl = `${protocol}//${host}:${port}/websocket`;
    }

    try {
      this.socket = new WebSocket(this.serverUrl);
      
      this.socket.onopen = () => {
        console.log('Connected to Tootsville server');
        this.isConnected = true;
        this.reconnectAttempts = 0;
        this.emit('connected');
        this.processMessageQueue();
      };

      this.socket.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          this.handleMessage(data);
        } catch (error) {
          console.error('Failed to parse message:', error);
          this.emit('error', { message: 'Invalid message format' });
        }
      };

      this.socket.onclose = (event) => {
        console.log('Disconnected from server:', event.code, event.reason);
        this.isConnected = false;
        this.isAuthenticated = false;
        this.emit('disconnected', { code: event.code, reason: event.reason });
        
        // Attempt reconnection if not a clean close
        if (event.code !== 1000 && this.reconnectAttempts < this.maxReconnectAttempts) {
          this.attemptReconnect();
        }
      };

      this.socket.onerror = (error) => {
        console.error('WebSocket error:', error);
        this.emit('error', { message: 'Connection error' });
      };

    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
      this.emit('error', { message: 'Failed to establish connection' });
    }
  }

  /**
   * @method send
   * @param {string} command - Command name
   * @param {Object} data - Command data
   * @description Send command to server
   * 
   * Sends a command to the tootsville.net server using the existing
   * protocol format. If not connected, queues the message for later.
   */
  send(command, data = {}) {
    const message = {
      command: command,
      data: data,
      timestamp: Date.now()
    };

    if (this.isConnected && this.socket && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(JSON.stringify(message));
    } else {
      // Queue message for later if not connected
      this.messageQueue.push(message);
      console.warn('Message queued - not connected to server');
    }
  }

  /**
   * @method handleMessage
   * @param {Object} data - Message data from server
   * @description Handle incoming messages from server
   * 
   * Processes incoming messages from the tootsville.net server and
   * routes them to appropriate handlers based on the message type.
   */
  handleMessage(data) {
    const { type, payload } = data;

    switch (type) {
      case 'logOK':
        this.handleLogOK(payload);
        break;
      case 'avatars':
        this.handleAvatars(payload);
        break;
      case 'publicMessage':
        this.handlePublicMessage(payload);
        break;
      case 'inventory':
        this.handleInventory(payload);
        break;
      case 'ping':
        this.handlePing(payload);
        break;
      case 'error':
        this.handleError(payload);
        break;
      default:
        // Emit unknown message types for custom handling
        this.emit(type, payload);
    }
  }

  /**
   * @method handleLogOK
   * @param {Object} message - Login success message
   * @description Handle successful login
   */
  handleLogOK(message) {
    this.isAuthenticated = true;
    this.emit('authenticated', message);
  }

  /**
   * @method handleAvatars
   * @param {Object} message - Avatar update message
   * @description Handle avatar updates
   */
  handleAvatars(message) {
    this.emit('avatarUpdate', message);
  }

  /**
   * @method handlePublicMessage
   * @param {Object} message - Public chat message
   * @description Handle public chat messages
   */
  handlePublicMessage(message) {
    this.emit('publicMessage', message);
  }

  /**
   * @method handleInventory
   * @param {Object} message - Inventory update message
   * @description Handle inventory updates
   */
  handleInventory(message) {
    this.emit('inventoryUpdate', message);
  }

  /**
   * @method handlePing
   * @param {Object} message - Ping message
   * @description Handle ping/pong for connection health
   */
  handlePing(message) {
    // Respond to ping with pong
    this.send('pong', { timestamp: message.timestamp });
    this.emit('ping', message);
  }

  /**
   * @method handleError
   * @param {Object} message - Error message
   * @description Handle server errors
   */
  handleError(message) {
    console.error('Server error:', message);
    this.emit('error', message);
  }

  /**
   * @method processMessageQueue
   * @description Process queued messages after connection
   */
  processMessageQueue() {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      this.socket.send(JSON.stringify(message));
    }
  }

  /**
   * @method attemptReconnect
   * @description Attempt to reconnect to server
   */
  attemptReconnect() {
    this.reconnectAttempts++;
    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
    
    console.log(`Attempting reconnection ${this.reconnectAttempts}/${this.maxReconnectAttempts} in ${delay}ms`);
    
    setTimeout(() => {
      if (!this.isConnected) {
        this.connect(this.cluster);
      }
    }, delay);
  }

  /**
   * @method on
   * @param {string} event - Event name
   * @param {Function} callback - Event handler
   * @description Register event handler
   */
  on(event, callback) {
    if (!this.eventHandlers.has(event)) {
      this.eventHandlers.set(event, []);
    }
    this.eventHandlers.get(event).push(callback);
  }

  /**
   * @method off
   * @param {string} event - Event name
   * @param {Function} callback - Event handler to remove
   * @description Remove event handler
   */
  off(event, callback) {
    if (this.eventHandlers.has(event)) {
      const handlers = this.eventHandlers.get(event);
      const index = handlers.indexOf(callback);
      if (index > -1) {
        handlers.splice(index, 1);
      }
    }
  }

  /**
   * @method emit
   * @param {string} event - Event name
   * @param {*} data - Event data
   * @description Emit event to registered handlers
   */
  emit(event, data) {
    if (this.eventHandlers.has(event)) {
      this.eventHandlers.get(event).forEach(callback => {
        try {
          callback(data);
        } catch (error) {
          console.error(`Error in event handler for ${event}:`, error);
        }
      });
    }
  }

  /**
   * @method disconnect
   * @description Disconnect from server
   */
  disconnect() {
    if (this.socket) {
      this.socket.close(1000, 'Client disconnect');
      this.socket = null;
    }
    this.isConnected = false;
    this.isAuthenticated = false;
    this.messageQueue = [];
  }

  /**
   * @method getConnectionStatus
   * @returns {Object} Connection status information
   * @description Get current connection status
   */
  getConnectionStatus() {
    return {
      isConnected: this.isConnected,
      isAuthenticated: this.isAuthenticated,
      reconnectAttempts: this.reconnectAttempts,
      serverUrl: this.serverUrl,
      cluster: this.cluster
    };
  }
}

// Create and export singleton instance
const communicationService = new CommunicationService();
export default communicationService;
