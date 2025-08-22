/**
 * CommunicationService.js - WebSocket communication service for Tootsville client
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Handles WebSocket communication with the Tootsville server while preserving
 * backwards compatibility with the existing protocol.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
class CommunicationService {
  constructor() {
    this.socket = null;
    this.isConnecting = false;
    this.messageHandlers = [];
    this.errorHandlers = [];
    this.closeHandlers = [];
    this.messageQueue = [];
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
    this.reconnectDelay = 1000;
  }

  /**
   * Connect to the Tootsville server WebSocket
   * @param {string} cluster - Server cluster (e.g., 'test.tootsville.org')
   * @returns {Promise<boolean>} - Connection success
   */
  async connect(cluster) {
    if (this.isConnected()) {
      throw new Error('Already connected');
    }

    if (this.isConnecting) {
      throw new Error('Connection in progress');
    }

    this.isConnecting = true;

    return new Promise((resolve, reject) => {
      try {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${cluster}/websocket`;
        
        this.socket = new WebSocket(wsUrl);
        
        this.socket.onopen = () => {
          this.isConnecting = false;
          this.reconnectAttempts = 0;
          this.flushMessageQueue();
          resolve(true);
        };

        this.socket.onmessage = (event) => {
          this.handleMessage(event);
        };

        this.socket.onerror = (error) => {
          this.isConnecting = false;
          this.notifyErrorHandlers(error);
          reject(error);
        };

        this.socket.onclose = (event) => {
          this.isConnecting = false;
          this.notifyCloseHandlers(event);
          
          // Auto-reconnect for unexpected disconnections
          if (event.code !== 1000 && this.reconnectAttempts < this.maxReconnectAttempts) {
            setTimeout(() => this.reconnect(), this.reconnectDelay);
          }
        };

      } catch (error) {
        this.isConnecting = false;
        reject(error);
      }
    });
  }

  /**
   * Disconnect from the server
   */
  disconnect() {
    if (this.socket) {
      this.socket.close(1000, 'Client disconnect');
      this.socket = null;
    }
    this.isConnecting = false;
  }

  /**
   * Check if currently connected
   * @returns {boolean} - Connection status
   */
  isConnected() {
    return this.socket && this.socket.readyState === WebSocket.OPEN;
  }

  /**
   * Send a message to the server
   * @param {Object} message - Message to send
   */
  send(message) {
    if (!message) {
      throw new Error('Message cannot be null or undefined');
    }

    if (!this.isConnected()) {
      // Queue message for later if not connected
      this.messageQueue.push(message);
      return;
    }

    try {
      // Preserve backwards compatibility with existing protocol
      const formattedMessage = this.formatMessage(message);
      this.socket.send(JSON.stringify(formattedMessage));
    } catch (error) {
      throw new Error(`Send failed: ${error.message}`);
    }
  }

  /**
   * Format message for server compatibility
   * @param {Object} message - Raw message
   * @returns {Object} - Formatted message
   */
  formatMessage(message) {
    // Handle both new format (type/data) and legacy format (command/data)
    if (message.type && message.data !== undefined) {
      return {
        command: message.type,
        data: message.data
      };
    }
    
    // Legacy format already has command/data
    if (message.command && message.data !== undefined) {
      return message;
    }

    // Default fallback
    return {
      command: 'message',
      data: message
    };
  }

  /**
   * Handle incoming messages
   * @param {MessageEvent} event - WebSocket message event
   */
  handleMessage(event) {
    try {
      const message = JSON.parse(event.data);
      this.notifyMessageHandlers(message);
    } catch (error) {
      console.warn('Failed to parse message:', error);
    }
  }

  /**
   * Register a message handler
   * @param {Function} handler - Message handler function
   * @returns {Function} - Function to remove the handler
   */
  onMessage(handler) {
    if (typeof handler !== 'function') {
      throw new Error('Handler must be a function');
    }

    this.messageHandlers.push(handler);
    
    return () => {
      const index = this.messageHandlers.indexOf(handler);
      if (index > -1) {
        this.messageHandlers.splice(index, 1);
      }
    };
  }

  /**
   * Register an error handler
   * @param {Function} handler - Error handler function
   */
  onError(handler) {
    if (typeof handler !== 'function') {
      throw new Error('Handler must be a function');
    }
    this.errorHandlers.push(handler);
  }

  /**
   * Register a close handler
   * @param {Function} handler - Close handler function
   */
  onClose(handler) {
    if (typeof handler !== 'function') {
      throw new Error('Handler must be a function');
    }
    this.closeHandlers.push(handler);
  }

  /**
   * Notify all message handlers
   * @param {Object} message - Message to send to handlers
   */
  notifyMessageHandlers(message) {
    this.messageHandlers.forEach(handler => {
      try {
        handler(message);
      } catch (error) {
        console.error('Message handler error:', error);
      }
    });
  }

  /**
   * Notify all error handlers
   * @param {Error} error - Error to send to handlers
   */
  notifyErrorHandlers(error) {
    this.errorHandlers.forEach(handler => {
      try {
        handler(error);
      } catch (handlerError) {
        console.error('Error handler error:', handlerError);
      }
    });
  }

  /**
   * Notify all close handlers
   * @param {CloseEvent} event - Close event to send to handlers
   */
  notifyCloseHandlers(event) {
    this.closeHandlers.forEach(handler => {
      try {
        handler(event);
      } catch (error) {
        console.error('Close handler error:', error);
      }
    });
  }

  /**
   * Flush queued messages
   */
  flushMessageQueue() {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      this.send(message);
    }
  }

  /**
   * Attempt to reconnect
   */
  async reconnect() {
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      console.error('Max reconnection attempts reached');
      return;
    }

    this.reconnectAttempts++;
    console.log(`Reconnection attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts}`);

    try {
      // Extract cluster from current socket URL
      const url = this.socket ? this.socket.url : window.location.host;
      const cluster = url.replace(/^wss?:\/\//, '').replace(/\/websocket$/, '');
      
      await this.connect(cluster);
    } catch (error) {
      console.error('Reconnection failed:', error);
      // Exponential backoff
      this.reconnectDelay = Math.min(this.reconnectDelay * 2, 30000);
    }
  }

  /**
   * Get connection statistics
   * @returns {Object} - Connection statistics
   */
  getStats() {
    return {
      connected: this.isConnected(),
      connecting: this.isConnecting,
      reconnectAttempts: this.reconnectAttempts,
      queuedMessages: this.messageQueue.length,
      messageHandlers: this.messageHandlers.length,
      errorHandlers: this.errorHandlers.length,
      closeHandlers: this.closeHandlers.length
    };
  }
}

export { CommunicationService };
