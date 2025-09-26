/**
 * Infinity Mode AMFManager - Vue/TypeScript Implementation
 *
 * Equivalent to original ActionScript AMFPHP for REST API communication
 * Provides HTTP-based API communication with automatic serialization/deserialization
 */

import { ref, reactive } from 'vue';

// HTTP methods
export enum HttpMethod {
  GET = 'GET',
  POST = 'POST',
  PUT = 'PUT',
  DELETE = 'DELETE',
  PATCH = 'PATCH'
}

// Response types
export enum ResponseType {
  JSON = 'json',
  TEXT = 'text',
  BLOB = 'blob',
  ARRAY_BUFFER = 'arraybuffer'
}

// API endpoint categories matching original AMFPHP structure
export enum APIEndpoint {
  // Authentication
  LOGIN = 'auth/login',
  LOGOUT = 'auth/logout',
  REGISTER = 'auth/register',
  FORGOT_PASSWORD = 'auth/forgot-password',

  // User Management
  GET_USER_PROFILE = 'user/profile',
  UPDATE_USER_PROFILE = 'user/profile',
  GET_AVATAR_LIST = 'user/avatars',
  SET_AVATAR = 'user/avatar',

  // Character/Toot Management
  GET_CHARACTER_LIST = 'character/list',
  CREATE_CHARACTER = 'character/create',
  UPDATE_CHARACTER = 'character/update',
  DELETE_CHARACTER = 'character/delete',

  // World/Navigation
  GET_WORLD_INFO = 'world/info',
  GET_ZONE_LIST = 'world/zones',
  GET_ROOM_INFO = 'world/room',
  TELEPORT = 'world/teleport',

  // Social Features
  GET_BUDDY_LIST = 'social/buddies',
  ADD_BUDDY = 'social/buddy/add',
  REMOVE_BUDDY = 'social/buddy/remove',
  SEND_MESSAGE = 'social/message',
  GET_MESSAGES = 'social/messages',

  // Inventory & Items
  GET_INVENTORY = 'inventory/items',
  USE_ITEM = 'inventory/use',
  BUY_ITEM = 'store/buy',
  SELL_ITEM = 'store/sell',
  GIFT_ITEM = 'store/gift',

  // Commerce & Currency
  GET_BALANCE = 'commerce/balance',
  GET_STORE_ITEMS = 'store/items',
  GET_TRANSACTION_HISTORY = 'commerce/history',

  // Game Features
  GET_ACHIEVEMENTS = 'game/achievements',
  GET_LEADERBOARD = 'game/leaderboard',
  START_MINIGAME = 'game/minigame/start',
  END_MINIGAME = 'game/minigame/end',
  SAVE_GAME_STATE = 'game/state/save',
  LOAD_GAME_STATE = 'game/state/load',

  // Admin/Moderation
  REPORT_USER = 'admin/report',
  GET_MODERATOR_TOOLS = 'admin/tools',

  // Real Estate
  GET_REAL_ESTATE_OFFICES = 'realestate/offices',
  GET_LAND_INFO = 'realestate/land',
  BUY_LAND = 'realestate/buy',
  MOVE_HOUSE = 'realestate/move',

  // Events & Activities
  GET_DAILY_EVENTS = 'events/daily',
  GET_TOURNAMENTS = 'events/tournaments',
  JOIN_EVENT = 'events/join',
  LEAVE_EVENT = 'events/leave',

  // Photo Booth
  TAKE_PHOTO = 'photobooth/take',
  GET_PHOTOS = 'photobooth/photos',
  SHARE_PHOTO = 'photobooth/share'
}

// Request configuration
export interface APIRequest {
  endpoint: APIEndpoint | string;
  method?: HttpMethod;
  data?: any;
  headers?: Record<string, string>;
  responseType?: ResponseType;
  timeout?: number;
  retries?: number;
}

// Response structure
export interface APIResponse<T = any> {
  success: boolean;
  data?: T;
  error?: {
    code: string;
    message: string;
    details?: any;
  };
  metadata?: {
    timestamp: number;
    requestId: string;
    processingTime: number;
  };
}

// Connection configuration
export interface AMFConfig {
  baseUrl: string;
  apiVersion?: string;
  timeout?: number;
  retries?: number;
  debug?: boolean;
  headers?: Record<string, string>;
}

// Request queue item
interface QueuedRequest {
  request: APIRequest;
  resolve: Function;
  reject: Function;
  retries: number;
  startTime: number;
}

/**
 * AMFManager - REST API communication manager
 *
 * Equivalent to original ActionScript AMFPHP implementation
 * Handles HTTP requests with automatic serialization, error handling, and queue management
 */
export class AMFManager {
  // Configuration
  private config: AMFConfig;
  private defaultHeaders: Record<string, string> = {
    'Content-Type': 'application/json',
    'Accept': 'application/json',
    'X-API-Version': '1.0'
  };

  // Request management
  private requestQueue: QueuedRequest[] = [];
  private activeRequests = 0;
  private maxConcurrentRequests = 6;
  private requestId = 0;

  // Reactive state
  public isOnline = ref(true);
  public pendingRequests = ref(0);

  // Authentication
  private authToken: string | null = null;
  private sessionId: string | null = null;

  constructor(config: AMFConfig) {
    this.config = {
      baseUrl: 'http://localhost:5000/api',
      apiVersion: 'v1',
      timeout: 30000,
      retries: 3,
      debug: false,
      headers: {},
      ...config
    };

    // Merge default headers with config
    this.defaultHeaders = {
      ...this.defaultHeaders,
      ...this.config.headers
    };

    // Setup network monitoring
    this.setupNetworkMonitoring();
  }

  /**
   * Set authentication token
   */
  public setAuthToken(token: string): void {
    this.authToken = token;
    this.defaultHeaders['Authorization'] = `Bearer ${token}`;
  }

  /**
   * Clear authentication
   */
  public clearAuth(): void {
    this.authToken = null;
    delete this.defaultHeaders['Authorization'];
  }

  /**
   * Set session ID
   */
  public setSessionId(sessionId: string): void {
    this.sessionId = sessionId;
    this.defaultHeaders['X-Session-ID'] = sessionId;
  }

  /**
   * Make API request
   */
  public async request<T = any>(requestConfig: APIRequest): Promise<APIResponse<T>> {
    const request: APIRequest = {
      method: HttpMethod.POST,
      responseType: ResponseType.JSON,
      timeout: this.config.timeout,
      retries: this.config.retries,
      ...requestConfig
    };

    return new Promise((resolve, reject) => {
      const queuedRequest: QueuedRequest = {
        request,
        resolve,
        reject,
        retries: request.retries || 0,
        startTime: Date.now()
      };

      this.requestQueue.push(queuedRequest);
      this.pendingRequests.value = this.requestQueue.length;
      this.processQueue();
    });
  }

  /**
   * Convenience methods for common HTTP methods
   */
  public async get<T = any>(endpoint: APIEndpoint | string, data?: any): Promise<APIResponse<T>> {
    return this.request<T>({
      endpoint,
      method: HttpMethod.GET,
      data
    });
  }

  public async post<T = any>(endpoint: APIEndpoint | string, data?: any): Promise<APIResponse<T>> {
    return this.request<T>({
      endpoint,
      method: HttpMethod.POST,
      data
    });
  }

  public async put<T = any>(endpoint: APIEndpoint | string, data?: any): Promise<APIResponse<T>> {
    return this.request<T>({
      endpoint,
      method: HttpMethod.PUT,
      data
    });
  }

  public async delete<T = any>(endpoint: APIEndpoint | string, data?: any): Promise<APIResponse<T>> {
    return this.request<T>({
      endpoint,
      method: HttpMethod.DELETE,
      data
    });
  }

  /**
   * Authentication methods
   */
  public async login(username: string, password: string): Promise<APIResponse> {
    const response = await this.post(APIEndpoint.LOGIN, { username, password });

    if (response.success && response.data?.token) {
      this.setAuthToken(response.data.token);
      if (response.data.sessionId) {
        this.setSessionId(response.data.sessionId);
      }
    }

    return response;
  }

  public async logout(): Promise<APIResponse> {
    const response = await this.post(APIEndpoint.LOGOUT);
    this.clearAuth();
    return response;
  }

  public async register(userData: {
    username: string;
    email: string;
    password: string;
    parentEmail?: string;
  }): Promise<APIResponse> {
    return this.post(APIEndpoint.REGISTER, userData);
  }

  /**
   * User management methods
   */
  public async getUserProfile(userId?: string): Promise<APIResponse> {
    const endpoint = userId ? `${APIEndpoint.GET_USER_PROFILE}/${userId}` : APIEndpoint.GET_USER_PROFILE;
    return this.get(endpoint);
  }

  public async updateUserProfile(profileData: any): Promise<APIResponse> {
    return this.put(APIEndpoint.UPDATE_USER_PROFILE, profileData);
  }

  /**
   * Character/Toot management
   */
  public async getCharacterList(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_CHARACTER_LIST);
  }

  public async createCharacter(characterData: any): Promise<APIResponse> {
    return this.post(APIEndpoint.CREATE_CHARACTER, characterData);
  }

  public async updateCharacter(characterId: string, characterData: any): Promise<APIResponse> {
    return this.put(`${APIEndpoint.UPDATE_CHARACTER}/${characterId}`, characterData);
  }

  /**
   * Social features
   */
  public async getBuddyList(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_BUDDY_LIST);
  }

  public async addBuddy(userId: string): Promise<APIResponse> {
    return this.post(APIEndpoint.ADD_BUDDY, { userId });
  }

  public async sendMessage(toUserId: string, message: string): Promise<APIResponse> {
    return this.post(APIEndpoint.SEND_MESSAGE, { toUserId, message });
  }

  /**
   * Inventory and commerce
   */
  public async getInventory(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_INVENTORY);
  }

  public async buyItem(itemId: string, quantity: number = 1): Promise<APIResponse> {
    return this.post(APIEndpoint.BUY_ITEM, { itemId, quantity });
  }

  public async useItem(itemId: string, target?: any): Promise<APIResponse> {
    return this.post(APIEndpoint.USE_ITEM, { itemId, target });
  }

  public async getBalance(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_BALANCE);
  }

  /**
   * World and navigation
   */
  public async getWorldInfo(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_WORLD_INFO);
  }

  public async getZoneList(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_ZONE_LIST);
  }

  public async teleport(zoneId: string, position?: { x: number; y: number; z: number }): Promise<APIResponse> {
    return this.post(APIEndpoint.TELEPORT, { zoneId, position });
  }

  /**
   * Real estate
   */
  public async getRealEstateOffices(): Promise<APIResponse> {
    return this.get(APIEndpoint.GET_REAL_ESTATE_OFFICES);
  }

  public async buyLand(officeId: string, landId: string): Promise<APIResponse> {
    return this.post(APIEndpoint.BUY_LAND, { officeId, landId });
  }

  public async moveHouse(fromLandId: string, toLandId: string): Promise<APIResponse> {
    return this.post(APIEndpoint.MOVE_HOUSE, { fromLandId, toLandId });
  }

  /**
   * Process request queue
   */
  private async processQueue(): void {
    if (this.activeRequests >= this.maxConcurrentRequests || this.requestQueue.length === 0) {
      return;
    }

    const queuedRequest = this.requestQueue.shift();
    if (!queuedRequest) return;

    this.activeRequests++;
    this.pendingRequests.value = this.requestQueue.length;

    try {
      const response = await this.executeRequest(queuedRequest.request);
      queuedRequest.resolve(response);
    } catch (error) {
      if (queuedRequest.retries > 0) {
        queuedRequest.retries--;
        this.requestQueue.unshift(queuedRequest);
        this.pendingRequests.value = this.requestQueue.length;
        setTimeout(() => this.processQueue(), 1000); // Retry delay
      } else {
        queuedRequest.reject(error);
      }
    } finally {
      this.activeRequests--;
      this.processQueue();
    }
  }

  /**
   * Execute HTTP request
   */
  private async executeRequest(request: APIRequest): Promise<APIResponse> {
    const url = this.buildUrl(request.endpoint);
    const headers = { ...this.defaultHeaders, ...request.headers };

    // Add query parameters for GET requests
    let finalUrl = url;
    if (request.method === HttpMethod.GET && request.data) {
      const params = new URLSearchParams();
      this.serializeParams(request.data, params);
      finalUrl += `?${params.toString()}`;
    }

    this.log(`Making ${request.method} request to:`, finalUrl);

    const fetchOptions: RequestInit = {
      method: request.method,
      headers,
      signal: AbortSignal.timeout(request.timeout || this.config.timeout!)
    };

    // Add body for non-GET requests
    if (request.method !== HttpMethod.GET && request.data) {
      fetchOptions.body = JSON.stringify(request.data);
    }

    try {
      const response = await fetch(finalUrl, fetchOptions);

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      let data: any;
      switch (request.responseType) {
        case ResponseType.JSON:
          data = await response.json();
          break;
        case ResponseType.TEXT:
          data = await response.text();
          break;
        case ResponseType.BLOB:
          data = await response.blob();
          break;
        case ResponseType.ARRAY_BUFFER:
          data = await response.arrayBuffer();
          break;
        default:
          data = await response.json();
      }

      const apiResponse: APIResponse = {
        success: true,
        data,
        metadata: {
          timestamp: Date.now(),
          requestId: (++this.requestId).toString(),
          processingTime: Date.now() - Date.now() // Would need to track start time
        }
      };

      this.log('Response:', apiResponse);
      return apiResponse;

    } catch (error) {
      this.log('Request failed:', error);
      throw error;
    }
  }

  /**
   * Build full URL from endpoint
   */
  private buildUrl(endpoint: string): string {
    const baseUrl = this.config.baseUrl!.replace(/\/$/, '');
    const cleanEndpoint = endpoint.replace(/^\//, '');
    const apiVersion = this.config.apiVersion ? `/${this.config.apiVersion}` : '';
    return `${baseUrl}${apiVersion}/${cleanEndpoint}`;
  }

  /**
   * Serialize parameters for query string
   */
  private serializeParams(data: any, params: URLSearchParams, prefix = ''): void {
    if (data === null || data === undefined) return;

    if (Array.isArray(data)) {
      data.forEach((item, index) => {
        this.serializeParams(item, params, `${prefix}[${index}]`);
      });
    } else if (typeof data === 'object') {
      Object.keys(data).forEach(key => {
        const value = data[key];
        const newPrefix = prefix ? `${prefix}[${key}]` : key;
        if (typeof value === 'object') {
          this.serializeParams(value, params, newPrefix);
        } else {
          params.append(newPrefix, String(value));
        }
      });
    } else {
      params.append(prefix, String(data));
    }
  }

  /**
   * Setup network monitoring
   */
  private setupNetworkMonitoring(): void {
    window.addEventListener('online', () => {
      this.isOnline.value = true;
      this.log('Network connection restored');
      this.processQueue(); // Retry pending requests
    });

    window.addEventListener('offline', () => {
      this.isOnline.value = false;
      this.log('Network connection lost');
    });
  }

  /**
   * Logging utility
   */
  private log(...args: any[]): void {
    if (this.config.debug) {
      console.log('[AMFManager]', ...args);
    }
  }
}

// Singleton instance for global use
let amfInstance: AMFManager | null = null;

/**
 * Get or create AMFManager singleton
 */
export function useAMF(config?: AMFConfig): AMFManager {
  if (!amfInstance) {
    amfInstance = new AMFManager(config || {});
  }
  return amfInstance;
}

/**
 * Cleanup singleton instance
 */
export function destroyAMF(): void {
  amfInstance = null;
}

export default AMFManager;
