/**
 * Infinity Mode SmartFoxManager - Vue/TypeScript Implementation
 *
 * Equivalent to original ActionScript SmartFoxManager for Infinity Mode protocol
 * Provides WebSocket-based real-time communication with server
 */

import { ref, reactive, computed, type Ref } from 'vue';

// Connection states matching original SmartFox implementation
export enum ConnectionState {
  DISCONNECTED = 'disconnected',
  CONNECTING = 'connecting',
  CONNECTED = 'connected',
  LOGGING_IN = 'loggingIn',
  LOGGED_IN = 'loggedIn',
  DISCONNECTING = 'disconnecting'
}

// Room states for zone/room management
export enum RoomState {
  JOINING = 'joining',
  JOINED = 'joined',
  LEAVING = 'leaving',
  LEFT = 'left'
}

// Message types matching original protocol
export enum MessageType {
  // Connection messages
  CONNECT = 'connect',
  DISCONNECT = 'disconnect',
  LOGIN = 'login',
  LOGOUT = 'logout',

  // Room messages
  JOIN_ROOM = 'joinRoom',
  LEAVE_ROOM = 'leaveRoom',
  ROOM_LIST = 'roomList',
  USER_ENTER_ROOM = 'userEnterRoom',
  USER_EXIT_ROOM = 'userExitRoom',

  // Chat messages
  PUBLIC_MESSAGE = 'publicMessage',
  PRIVATE_MESSAGE = 'privateMessage',
  MODERATOR_MESSAGE = 'moderatorMessage',
  ADMIN_MESSAGE = 'adminMessage',

  // Game messages
  GAME_MESSAGE = 'gameMessage',
  PLAYER_MOVE = 'playerMove',
  PLAYER_ACTION = 'playerAction',

  // System messages
  PING = 'ping',
  PONG = 'pong',
  ERROR = 'error',
  WARNING = 'warning'
}

// User object structure
export interface User {
  id: string;
  name: string;
  isModerator: boolean;
  isAdmin: boolean;
  variables: Record<string, any>;
  position?: {
    x: number;
    y: number;
    z: number;
  };
}

// Room object structure
export interface Room {
  id: string;
  name: string;
  isPrivate: boolean;
  isTemp: boolean;
  maxUsers: number;
  userCount: number;
  users: Map<string, User>;
  variables: Record<string, any>;
  state: RoomState;
}

// Message structure
export interface Message {
  type: MessageType;
  from?: User;
  to?: User | Room;
  data: any;
  timestamp: number;
  room?: Room;
}

// Connection configuration
export interface ConnectionConfig {
  host: string;
  port: number;
  useSSL: boolean;
  zone?: string;
  debug?: boolean;
  reconnectionAttempts?: number;
  reconnectionDelay?: number;
  pingInterval?: number;
  timeout?: number;
}

/**
 * SmartFoxManager - Main Infinity Mode communication manager
 *
 * Provides WebSocket-based real-time communication equivalent to original ActionScript SmartFoxManager
 */
export class SmartFoxManager {
  // Reactive state
  public connectionState = ref<ConnectionState>(ConnectionState.DISCONNECTED);
  public currentUser = ref<User | null>(null);
  public currentRoom = ref<Room | null>(null);
  public rooms = reactive<Map<string, Room>>(new Map());
  public users = reactive<Map<string, User>>(new Map());

  // Configuration
  private config: ConnectionConfig;
  private ws: WebSocket | null = null;
  private reconnectionTimer: number | null = null;
  private pingTimer: number | null = null;
  private messageHandlers = new Map<MessageType, Function[]>();
  private requestId = 0;

  // Connection management
  private reconnectionAttempts = 0;
  private lastPingTime = 0;
  private pendingRequests = new Map<number, { resolve: Function; reject: Function; timeout: number }>();

  constructor(config: ConnectionConfig) {
    this.config = {
      host: 'localhost',
      port: 5004,
      useSSL: false,
      debug: false,
      reconnectionAttempts: 3,
      reconnectionDelay: 2000,
      pingInterval: 30000,
      timeout: 10000,
      ...config
    };

    // Initialize message handlers
    this.setupDefaultHandlers();

    // Setup ping/pong for keepalive
    this.setupPingPong();
  }

  /**
   * Connect to Infinity Mode server
   */
  public async connect(): Promise<void> {
    if (this.connectionState.value === ConnectionState.CONNECTED ||
        this.connectionState.value === ConnectionState.CONNECTING) {
      return Promise.resolve();
    }

    this.connectionState.value = ConnectionState.CONNECTING;

    return new Promise((resolve, reject) => {
      try {
        const protocol = this.config.useSSL ? 'wss:' : 'ws:';
        const url = `${protocol}//${this.config.host}:${this.config.port}/infinity`;

        this.log('Connecting to:', url);
        this.ws = new WebSocket(url);

        this.ws.onopen = () => {
          this.log('WebSocket connected');
          this.connectionState.value = ConnectionState.CONNECTED;
          this.reconnectionAttempts = 0;
          resolve();
        };

        this.ws.onmessage = (event) => {
          this.handleMessage(event.data);
        };

        this.ws.onclose = (event) => {
          this.log('WebSocket closed:', event.code, event.reason);
          this.handleDisconnect();
          if (this.connectionState.value === ConnectionState.CONNECTING) {
            reject(new Error(`Connection failed: ${event.reason || 'Unknown error'}`));
          }
        };

        this.ws.onerror = (error) => {
          this.log('WebSocket error:', error);
          this.handleDisconnect();
          if (this.connectionState.value === ConnectionState.CONNECTING) {
            reject(new Error('WebSocket connection error'));
          }
        };

        // Connection timeout
        setTimeout(() => {
          if (this.connectionState.value === ConnectionState.CONNECTING) {
            this.ws?.close();
            reject(new Error('Connection timeout'));
          }
        }, this.config.timeout);

      } catch (error) {
        this.connectionState.value = ConnectionState.DISCONNECTED;
        reject(error);
      }
    });
  }

  /**
   * Disconnect from server
   */
  public disconnect(): void {
    this.connectionState.value = ConnectionState.DISCONNECTING;

    if (this.pingTimer) {
      clearInterval(this.pingTimer);
      this.pingTimer = null;
    }

    if (this.reconnectionTimer) {
      clearTimeout(this.reconnectionTimer);
      this.reconnectionTimer = null;
    }

    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
      this.ws = null;
    }

    // Clear pending requests
    for (const [id, request] of this.pendingRequests) {
      clearTimeout(request.timeout);
      request.reject(new Error('Connection closed'));
    }
    this.pendingRequests.clear();

    this.connectionState.value = ConnectionState.DISCONNECTED;
    this.currentUser.value = null;
    this.currentRoom.value = null;
    this.rooms.clear();
    this.users.clear();
  }

  /**
   * Login to server
   */
  public async login(username: string, password: string, zone?: string): Promise<User> {
    if (this.connectionState.value !== ConnectionState.CONNECTED) {
      throw new Error('Not connected to server');
    }

    this.connectionState.value = ConnectionState.LOGGING_IN;

    const response = await this.sendRequest(MessageType.LOGIN, {
      username,
      password,
      zone: zone || this.config.zone
    });

    const user: User = {
      id: response.userId,
      name: username,
      isModerator: response.isModerator || false,
      isAdmin: response.isAdmin || false,
      variables: response.variables || {}
    };

    this.currentUser.value = user;
    this.users.set(user.id, user);
    this.connectionState.value = ConnectionState.LOGGED_IN;

    this.emit('loginSuccess', { user });
    return user;
  }

  /**
   * Logout from server
   */
  public async logout(): Promise<void> {
    if (this.connectionState.value !== ConnectionState.LOGGED_IN) {
      return;
    }

    await this.sendRequest(MessageType.LOGOUT, {});
    this.disconnect();
    this.emit('logout');
  }

  /**
   * Join a room
   */
  public async joinRoom(roomId: string, password?: string): Promise<Room> {
    if (this.connectionState.value !== ConnectionState.LOGGED_IN) {
      throw new Error('Not logged in');
    }

    const response = await this.sendRequest(MessageType.JOIN_ROOM, {
      roomId,
      password
    });

    const room: Room = {
      id: roomId,
      name: response.roomName,
      isPrivate: response.isPrivate || false,
      isTemp: response.isTemp || false,
      maxUsers: response.maxUsers || 100,
      userCount: response.userCount || 0,
      users: new Map(),
      variables: response.variables || {},
      state: RoomState.JOINED
    };

    // Add users in room
    if (response.users) {
      for (const userData of response.users) {
        const user: User = {
          id: userData.id,
          name: userData.name,
          isModerator: userData.isModerator || false,
          isAdmin: userData.isAdmin || false,
          variables: userData.variables || {}
        };
        room.users.set(user.id, user);
        this.users.set(user.id, user);
      }
    }

    this.rooms.set(roomId, room);
    this.currentRoom.value = room;

    this.emit('roomJoined', { room });
    return room;
  }

  /**
   * Leave current room
   */
  public async leaveRoom(): Promise<void> {
    if (!this.currentRoom.value) {
      return;
    }

    await this.sendRequest(MessageType.LEAVE_ROOM, {
      roomId: this.currentRoom.value.id
    });

    const room = this.currentRoom.value;
    room.state = RoomState.LEFT;

    this.emit('roomLeft', { room });
    this.currentRoom.value = null;
  }

  /**
   * Send public message to current room
   */
  public async sendPublicMessage(message: string): Promise<void> {
    if (!this.currentRoom.value) {
      throw new Error('Not in a room');
    }

    await this.sendRequest(MessageType.PUBLIC_MESSAGE, {
      roomId: this.currentRoom.value.id,
      message
    });
  }

  /**
   * Send private message to user
   */
  public async sendPrivateMessage(userId: string, message: string): Promise<void> {
    await this.sendRequest(MessageType.PRIVATE_MESSAGE, {
      toUserId: userId,
      message
    });
  }

  /**
   * Send game-specific message
   */
  public async sendGameMessage(data: any, targetUserId?: string): Promise<void> {
    await this.sendRequest(MessageType.GAME_MESSAGE, {
      data,
      toUserId: targetUserId
    });
  }

  /**
   * Send player movement update
   */
  public async sendPlayerMove(position: { x: number; y: number; z: number }, rotation?: { x: number; y: number; z: number }): Promise<void> {
    if (!this.currentUser.value) {
      return;
    }

    this.currentUser.value.position = position;

    await this.sendRequest(MessageType.PLAYER_MOVE, {
      position,
      rotation
    });
  }

  /**
   * Send player action
   */
  public async sendPlayerAction(action: string, data?: any): Promise<void> {
    await this.sendRequest(MessageType.PLAYER_ACTION, {
      action,
      data
    });
  }

  /**
   * Get list of available rooms
   */
  public async getRoomList(): Promise<Room[]> {
    const response = await this.sendRequest(MessageType.ROOM_LIST, {});
    return response.rooms || [];
  }

  /**
   * Add event listener
   */
  public on(eventType: MessageType, handler: Function): void {
    if (!this.messageHandlers.has(eventType)) {
      this.messageHandlers.set(eventType, []);
    }
    this.messageHandlers.get(eventType)!.push(handler);
  }

  /**
   * Remove event listener
   */
  public off(eventType: MessageType, handler?: Function): void {
    if (!this.messageHandlers.has(eventType)) {
      return;
    }

    const handlers = this.messageHandlers.get(eventType)!;
    if (handler) {
      const index = handlers.indexOf(handler);
      if (index > -1) {
        handlers.splice(index, 1);
      }
    } else {
      handlers.length = 0;
    }
  }

  /**
   * Send request with response handling
   */
  private async sendRequest(type: MessageType, data: any): Promise<any> {
    const requestId = ++this.requestId;

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        this.pendingRequests.delete(requestId);
        reject(new Error('Request timeout'));
      }, this.config.timeout);

      this.pendingRequests.set(requestId, { resolve, reject, timeout });

      this.send({
        type,
        requestId,
        data,
        timestamp: Date.now()
      });
    });
  }

  /**
   * Send message to server
   */
  private send(message: any): void {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      throw new Error('WebSocket not connected');
    }

    const messageStr = JSON.stringify(message);
    this.log('Sending:', message);
    this.ws.send(messageStr);
  }

  /**
   * Handle incoming messages
   */
  private handleMessage(data: string): void {
    try {
      const message: Message = JSON.parse(data);
      this.log('Received:', message);

      // Handle response to pending request
      if (message.type === MessageType.ERROR && message.data?.requestId) {
        const request = this.pendingRequests.get(message.data.requestId);
        if (request) {
          clearTimeout(request.timeout);
          this.pendingRequests.delete(message.data.requestId);
          request.reject(new Error(message.data.error || 'Request failed'));
          return;
        }
      }

      // Handle successful response
      if (message.data?.requestId) {
        const request = this.pendingRequests.get(message.data.requestId);
        if (request) {
          clearTimeout(request.timeout);
          this.pendingRequests.delete(message.data.requestId);
          request.resolve(message.data);
          return;
        }
      }

      // Handle regular message
      this.emit(message.type, message);

    } catch (error) {
      this.log('Error parsing message:', error);
      this.emit(MessageType.ERROR, { error: 'Invalid message format' });
    }
  }

  /**
   * Emit event to listeners
   */
  private emit(eventType: MessageType | string, data: any): void {
    const handlers = this.messageHandlers.get(eventType as MessageType);
    if (handlers) {
      handlers.forEach(handler => {
        try {
          handler(data);
        } catch (error) {
          this.log('Error in event handler:', error);
        }
      });
    }
  }

  /**
   * Setup default message handlers
   */
  private setupDefaultHandlers(): void {
    // Handle ping responses
    this.on(MessageType.PONG, (data) => {
      const latency = Date.now() - this.lastPingTime;
      this.emit('ping', { latency });
    });

    // Handle user enter/leave room
    this.on(MessageType.USER_ENTER_ROOM, (data) => {
      if (data.roomId && this.rooms.has(data.roomId)) {
        const room = this.rooms.get(data.roomId)!;
        const user: User = {
          id: data.user.id,
          name: data.user.name,
          isModerator: data.user.isModerator || false,
          isAdmin: data.user.isAdmin || false,
          variables: data.user.variables || {}
        };
        room.users.set(user.id, user);
        this.users.set(user.id, user);
      }
    });

    this.on(MessageType.USER_EXIT_ROOM, (data) => {
      if (data.roomId && this.rooms.has(data.roomId)) {
        const room = this.rooms.get(data.roomId)!;
        if (room.users.has(data.userId)) {
          room.users.delete(data.userId);
        }
        if (this.users.has(data.userId)) {
          this.users.delete(data.userId);
        }
      }
    });

    // Handle connection errors
    this.on(MessageType.ERROR, (data) => {
      this.log('Server error:', data.error);
    });
  }

  /**
   * Setup ping/pong for connection keepalive
   */
  private setupPingPong(): void {
    this.pingTimer = setInterval(() => {
      if (this.connectionState.value === ConnectionState.CONNECTED ||
          this.connectionState.value === ConnectionState.LOGGED_IN) {
        this.lastPingTime = Date.now();
        this.send({ type: MessageType.PING, timestamp: this.lastPingTime });
      }
    }, this.config.pingInterval);
  }

  /**
   * Handle disconnection and attempt reconnection
   */
  private handleDisconnect(): void {
    this.connectionState.value = ConnectionState.DISCONNECTED;

    // Clear current connection
    if (this.ws) {
      this.ws = null;
    }

    // Attempt reconnection if configured
    if (this.reconnectionAttempts < (this.config.reconnectionAttempts || 0)) {
      this.reconnectionAttempts++;
      this.reconnectionTimer = setTimeout(() => {
        this.log(`Attempting reconnection ${this.reconnectionAttempts}/${this.config.reconnectionAttempts}`);
        this.connect().catch(error => {
          this.log('Reconnection failed:', error);
        });
      }, this.config.reconnectionDelay);
    } else {
      this.emit('connectionLost', {});
    }
  }

  /**
   * Logging utility
   */
  private log(...args: any[]): void {
    if (this.config.debug) {
      console.log('[SmartFoxManager]', ...args);
    }
  }
}

// Singleton instance for global use
let smartFoxInstance: SmartFoxManager | null = null;

/**
 * Get or create SmartFoxManager singleton
 */
export function useSmartFox(config?: ConnectionConfig): SmartFoxManager {
  if (!smartFoxInstance) {
    smartFoxInstance = new SmartFoxManager(config || {});
  }
  return smartFoxInstance;
}

/**
 * Cleanup singleton instance
 */
export function destroySmartFox(): void {
  if (smartFoxInstance) {
    smartFoxInstance.disconnect();
    smartFoxInstance = null;
  }
}

export default SmartFoxManager;
