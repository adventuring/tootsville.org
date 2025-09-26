/**
 * Infinity Mode Event System - Vue/TypeScript Implementation
 *
 * Equivalent to original ActionScript event system (WorldEvent, custom events)
 * Provides comprehensive event dispatching and handling with type safety
 */

import { ref, reactive, type Ref } from 'vue';

// Base event types matching original ActionScript patterns
export enum EventType {
  // Connection Events
  CONNECT = 'connect',
  DISCONNECT = 'disconnect',
  CONNECTION_LOST = 'connectionLost',
  RECONNECT = 'reconnect',

  // Authentication Events
  LOGIN_SUCCESS = 'loginSuccess',
  LOGIN_FAILURE = 'loginFailure',
  LOGOUT = 'logout',

  // Room Events
  JOIN_ROOM = 'joinRoom',
  LEAVE_ROOM = 'leaveRoom',
  ROOM_LIST_UPDATE = 'roomListUpdate',
  USER_ENTER_ROOM = 'userEnterRoom',
  USER_EXIT_ROOM = 'userExitRoom',

  // Chat Events
  PUBLIC_MESSAGE = 'publicMessage',
  PRIVATE_MESSAGE = 'privateMessage',
  MODERATOR_MESSAGE = 'moderatorMessage',
  ADMIN_MESSAGE = 'adminMessage',
  SYSTEM_MESSAGE = 'systemMessage',

  // Game Events
  GAME_START = 'gameStart',
  GAME_END = 'gameEnd',
  PLAYER_MOVE = 'playerMove',
  PLAYER_ACTION = 'playerAction',
  GAME_STATE_UPDATE = 'gameStateUpdate',

  // Item/Equipment Events
  ITEM_USE = 'itemUse',
  ITEM_EQUIP = 'itemEquip',
  ITEM_UNEQUIP = 'itemUnequip',
  EQUIPMENT_EFFECT = 'equipmentEffect',
  EQUIPMENT_STATUS = 'equipmentStatus',

  // World Events
  WORLD_LOAD = 'worldLoad',
  ZONE_CHANGE = 'zoneChange',
  SCENE_UPDATE = 'sceneUpdate',
  OBJECT_SPAWN = 'objectSpawn',
  OBJECT_DESPAWN = 'objectDespawn',

  // Social Events
  BUDDY_ONLINE = 'buddyOnline',
  BUDDY_OFFLINE = 'buddyOffline',
  BUDDY_UPDATE = 'buddyUpdate',
  FRIEND_REQUEST = 'friendRequest',
  MESSAGE_RECEIVED = 'messageReceived',

  // Commerce Events
  PURCHASE_SUCCESS = 'purchaseSuccess',
  PURCHASE_FAILURE = 'purchaseFailure',
  BALANCE_UPDATE = 'balanceUpdate',
  INVENTORY_UPDATE = 'inventoryUpdate',

  // UI Events
  UI_PANEL_OPEN = 'uiPanelOpen',
  UI_PANEL_CLOSE = 'uiPanelClose',
  UI_NOTIFICATION = 'uiNotification',
  UI_DIALOG_SHOW = 'uiDialogShow',
  UI_DIALOG_HIDE = 'uiDialogHide',

  // Error Events
  ERROR = 'error',
  WARNING = 'warning',
  NETWORK_ERROR = 'networkError',
  AUTHENTICATION_ERROR = 'authenticationError',

  // Custom Events (for extensibility)
  CUSTOM = 'custom'
}

// Event priority levels
export enum EventPriority {
  LOW = 0,
  NORMAL = 1,
  HIGH = 2,
  CRITICAL = 3
}

// Event phases (matching DOM event phases)
export enum EventPhase {
  CAPTURING = 1,
  AT_TARGET = 2,
  BUBBLING = 3
}

/**
 * Base Event Interface
 *
 * Matches the structure of original ActionScript events
 */
export interface IEvent {
  readonly type: EventType | string;
  readonly target?: any;
  readonly currentTarget?: any;
  readonly eventPhase?: EventPhase;
  readonly bubbles?: boolean;
  readonly cancelable?: boolean;
  readonly timestamp: number;
  readonly data?: any;

  // Methods
  stopPropagation(): void;
  preventDefault(): void;
  isDefaultPrevented(): boolean;
  isPropagationStopped(): boolean;
}

/**
 * Base Event Class
 *
 * Equivalent to ActionScript's Event class
 */
export class BaseEvent implements IEvent {
  public readonly type: EventType | string;
  public readonly target?: any;
  public readonly currentTarget?: any;
  public readonly eventPhase?: EventPhase;
  public readonly bubbles: boolean;
  public readonly cancelable: boolean;
  public readonly timestamp: number;
  public readonly data?: any;

  private _defaultPrevented: boolean = false;
  private _propagationStopped: boolean = false;

  constructor(
    type: EventType | string,
    options: {
      target?: any;
      bubbles?: boolean;
      cancelable?: boolean;
      data?: any;
    } = {}
  ) {
    this.type = type;
    this.target = options.target;
    this.bubbles = options.bubbles ?? false;
    this.cancelable = options.cancelable ?? false;
    this.timestamp = Date.now();
    this.data = options.data;
  }

  public stopPropagation(): void {
    this._propagationStopped = true;
  }

  public preventDefault(): void {
    if (this.cancelable) {
      this._defaultPrevented = true;
    }
  }

  public isDefaultPrevented(): boolean {
    return this._defaultPrevented;
  }

  public isPropagationStopped(): boolean {
    return this._propagationStopped;
  }
}

/**
 * Custom Event Class
 *
 * Equivalent to ActionScript's custom event classes
 */
export class CustomEvent<T = any> extends BaseEvent {
  constructor(
    type: EventType | string,
    data?: T,
    options: {
      target?: any;
      bubbles?: boolean;
      cancelable?: boolean;
    } = {}
  ) {
    super(type, {
      ...options,
      data
    });
  }
}

/**
 * World Event Class
 *
 * Equivalent to original ActionScript WorldEvent
 */
export class WorldEvent extends BaseEvent {
  public readonly worldId: string;
  public readonly zoneId?: string;
  public readonly position?: { x: number; y: number; z: number };

  constructor(
    type: EventType,
    worldId: string,
    options: {
      zoneId?: string;
      position?: { x: number; y: number; z: number };
      target?: any;
      data?: any;
    } = {}
  ) {
    super(type, {
      target: options.target,
      bubbles: true,
      cancelable: false,
      data: options.data
    });

    this.worldId = worldId;
    this.zoneId = options.zoneId;
    this.position = options.position;
  }
}

/**
 * Player Event Class
 */
export class PlayerEvent extends BaseEvent {
  public readonly playerId: string;
  public readonly playerName: string;
  public readonly position?: { x: number; y: number; z: number };
  public readonly action?: string;

  constructor(
    type: EventType,
    playerId: string,
    playerName: string,
    options: {
      position?: { x: number; y: number; z: number };
      action?: string;
      target?: any;
      data?: any;
    } = {}
  ) {
    super(type, {
      target: options.target,
      bubbles: true,
      cancelable: true,
      data: options.data
    });

    this.playerId = playerId;
    this.playerName = playerName;
    this.position = options.position;
    this.action = options.action;
  }
}

/**
 * Equipment Event Class
 *
 * Equivalent to original equipment-related events
 */
export class EquipmentEvent extends BaseEvent {
  public readonly itemId: string;
  public readonly itemName: string;
  public readonly effect?: string;
  public readonly duration?: number;
  public readonly targetPlayerId?: string;

  constructor(
    type: EventType,
    itemId: string,
    itemName: string,
    options: {
      effect?: string;
      duration?: number;
      targetPlayerId?: string;
      target?: any;
      data?: any;
    } = {}
  ) {
    super(type, {
      target: options.target,
      bubbles: true,
      cancelable: true,
      data: options.data
    });

    this.itemId = itemId;
    this.itemName = itemName;
    this.effect = options.effect;
    this.duration = options.duration;
    this.targetPlayerId = options.targetPlayerId;
  }
}

/**
 * Chat Event Class
 */
export class ChatEvent extends BaseEvent {
  public readonly message: string;
  public readonly senderId: string;
  public readonly senderName: string;
  public readonly recipientId?: string;
  public readonly recipientName?: string;
  public readonly isPrivate: boolean;

  constructor(
    type: EventType,
    message: string,
    senderId: string,
    senderName: string,
    options: {
      recipientId?: string;
      recipientName?: string;
      target?: any;
      data?: any;
    } = {}
  ) {
    super(type, {
      target: options.target,
      bubbles: true,
      cancelable: false,
      data: options.data
    });

    this.message = message;
    this.senderId = senderId;
    this.senderName = senderName;
    this.recipientId = options.recipientId;
    this.recipientName = options.recipientName;
    this.isPrivate = !!options.recipientId;
  }
}

/**
 * Error Event Class
 */
export class ErrorEvent extends BaseEvent {
  public readonly error: Error;
  public readonly errorCode?: string;
  public readonly context?: any;

  constructor(
    type: EventType,
    error: Error,
    options: {
      errorCode?: string;
      context?: any;
      target?: any;
      data?: any;
    } = {}
  ) {
    super(type, {
      target: options.target,
      bubbles: true,
      cancelable: false,
      data: options.data
    });

    this.error = error;
    this.errorCode = options.errorCode;
    this.context = options.context;
  }
}

/**
 * Event Listener Interface
 */
export interface IEventListener {
  handleEvent(event: IEvent): void;
  priority?: EventPriority;
}

/**
 * Event Listener Function Type
 */
export type EventListenerFunction<T extends IEvent = IEvent> = (event: T) => void;

/**
 * Event Dispatcher
 *
 * Core event dispatching system equivalent to ActionScript's event dispatching
 */
export class EventDispatcher {
  private listeners = new Map<EventType | string, Array<{
    listener: IEventListener | EventListenerFunction;
    priority: EventPriority;
    once: boolean;
  }>>();

  private parentDispatcher?: EventDispatcher;
  private childDispatchers = new Set<EventDispatcher>();

  constructor(parent?: EventDispatcher) {
    this.parentDispatcher = parent;
    if (parent) {
      parent.addChildDispatcher(this);
    }
  }

  /**
   * Add event listener
   */
  public addEventListener<T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>,
    priority: EventPriority = EventPriority.NORMAL
  ): void {
    if (!this.listeners.has(type)) {
      this.listeners.set(type, []);
    }

    const listeners = this.listeners.get(type)!;
    listeners.push({
      listener,
      priority,
      once: false
    });

    // Sort by priority (higher priority first)
    listeners.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Add one-time event listener
   */
  public addEventListenerOnce<T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>,
    priority: EventPriority = EventPriority.NORMAL
  ): void {
    if (!this.listeners.has(type)) {
      this.listeners.set(type, []);
    }

    const listeners = this.listeners.get(type)!;
    listeners.push({
      listener,
      priority,
      once: true
    });

    // Sort by priority (higher priority first)
    listeners.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Remove event listener
   */
  public removeEventListener<T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>
  ): void {
    const listeners = this.listeners.get(type);
    if (!listeners) return;

    const index = listeners.findIndex(item => item.listener === listener);
    if (index > -1) {
      listeners.splice(index, 1);
    }
  }

  /**
   * Remove all listeners for a type
   */
  public removeAllListeners(type?: EventType | string): void {
    if (type) {
      this.listeners.delete(type);
    } else {
      this.listeners.clear();
    }
  }

  /**
   * Check if listener exists
   */
  public hasEventListener(type: EventType | string): boolean {
    return this.listeners.has(type) && this.listeners.get(type)!.length > 0;
  }

  /**
   * Dispatch event
   */
  public dispatchEvent(event: IEvent): boolean {
    // Set target if not already set
    if (!event.target) {
      (event as any).target = this;
    }

    // Set current target
    (event as any).currentTarget = this;

    // Get listeners for this event type
    const listeners = this.listeners.get(event.type);
    if (listeners) {
      // Create a copy to avoid modification during iteration
      const listenersCopy = [...listeners];

      for (const listenerItem of listenersCopy) {
        // Remove once listeners before calling
        if (listenerItem.once) {
          const index = listeners.indexOf(listenerItem);
          if (index > -1) {
            listeners.splice(index, 1);
          }
        }

        try {
          if (typeof listenerItem.listener === 'function') {
            (listenerItem.listener as EventListenerFunction)(event);
          } else {
            listenerItem.listener.handleEvent(event);
          }
        } catch (error) {
          console.error(`Error in event listener for ${event.type}:`, error);
        }

        // Stop if propagation is stopped
        if (event.isPropagationStopped()) {
          break;
        }
      }
    }

    // Dispatch to parent if event bubbles and not stopped
    if (event.bubbles && !event.isPropagationStopped() && this.parentDispatcher) {
      (event as any).eventPhase = EventPhase.BUBBLING;
      this.parentDispatcher.dispatchEvent(event);
    }

    // Dispatch to child dispatchers if event bubbles
    if (event.bubbles && !event.isPropagationStopped()) {
      for (const child of this.childDispatchers) {
        (event as any).eventPhase = EventPhase.BUBBLING;
        child.dispatchEvent(event);
      }
    }

    return !event.isDefaultPrevented();
  }

  /**
   * Add child dispatcher
   */
  private addChildDispatcher(child: EventDispatcher): void {
    this.childDispatchers.add(child);
  }

  /**
   * Remove child dispatcher
   */
  private removeChildDispatcher(child: EventDispatcher): void {
    this.childDispatchers.delete(child);
  }
}

/**
 * Global Event Bus
 *
 * Singleton event dispatcher for global application events
 */
class GlobalEventBus extends EventDispatcher {
  private static instance: GlobalEventBus;

  private constructor() {
    super();
  }

  public static getInstance(): GlobalEventBus {
    if (!GlobalEventBus.instance) {
      GlobalEventBus.instance = new GlobalEventBus();
    }
    return GlobalEventBus.instance;
  }
}

/**
 * Vue composable for event handling
 */
export function useEventSystem(target?: EventDispatcher) {
  const dispatcher = target || GlobalEventBus.getInstance();

  const emit = (event: IEvent) => {
    dispatcher.dispatchEvent(event);
  };

  const on = <T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>,
    priority: EventPriority = EventPriority.NORMAL
  ) => {
    dispatcher.addEventListener(type, listener, priority);
  };

  const once = <T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>,
    priority: EventPriority = EventPriority.NORMAL
  ) => {
    dispatcher.addEventListenerOnce(type, listener, priority);
  };

  const off = <T extends IEvent = IEvent>(
    type: EventType | string,
    listener: IEventListener | EventListenerFunction<T>
  ) => {
    dispatcher.removeEventListener(type, listener);
  };

  const has = (type: EventType | string): boolean => {
    return dispatcher.hasEventListener(type);
  };

  return {
    emit,
    on,
    once,
    off,
    has,
    dispatcher
  };
}

/**
 * Event Factory Functions
 *
 * Convenience functions for creating common events
 */
export const EventFactory = {
  // Connection events
  connect: (data?: any) => new BaseEvent(EventType.CONNECT, { bubbles: true, data }),
  disconnect: (data?: any) => new BaseEvent(EventType.DISCONNECT, { bubbles: true, data }),

  // Authentication events
  loginSuccess: (userData: any) => new CustomEvent(EventType.LOGIN_SUCCESS, userData, { bubbles: true }),
  loginFailure: (error: Error) => new ErrorEvent(EventType.LOGIN_FAILURE, error, { bubbles: true }),

  // World events
  worldLoad: (worldId: string, data?: any) => new WorldEvent(EventType.WORLD_LOAD, worldId, { data }),
  zoneChange: (worldId: string, zoneId: string, position?: any) =>
    new WorldEvent(EventType.ZONE_CHANGE, worldId, { zoneId, position }),

  // Player events
  playerMove: (playerId: string, playerName: string, position: any) =>
    new PlayerEvent(EventType.PLAYER_MOVE, playerId, playerName, { position }),
  playerAction: (playerId: string, playerName: string, action: string, data?: any) =>
    new PlayerEvent(EventType.PLAYER_ACTION, playerId, playerName, { action, data }),

  // Equipment events
  equipmentEffect: (itemId: string, itemName: string, effect: string, targetPlayerId?: string) =>
    new EquipmentEvent(EventType.EQUIPMENT_EFFECT, itemId, itemName, { effect, targetPlayerId }),
  equipmentStatus: (itemId: string, itemName: string, status: string, data?: any) =>
    new EquipmentEvent(EventType.EQUIPMENT_STATUS, itemId, itemName, { data: { status, ...data } }),

  // Chat events
  publicMessage: (message: string, senderId: string, senderName: string) =>
    new ChatEvent(EventType.PUBLIC_MESSAGE, message, senderId, senderName),
  privateMessage: (message: string, senderId: string, senderName: string, recipientId: string, recipientName: string) =>
    new ChatEvent(EventType.PRIVATE_MESSAGE, message, senderId, senderName, { recipientId, recipientName }),

  // Error events
  error: (error: Error, context?: any) => new ErrorEvent(EventType.ERROR, error, { context }),
  networkError: (error: Error) => new ErrorEvent(EventType.NETWORK_ERROR, error)
};

// Export singleton instance
export const globalEventBus = GlobalEventBus.getInstance();

// Export types
export type {
  IEvent,
  IEventListener,
  EventListenerFunction
};
