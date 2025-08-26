/**
 * MobilePlatformService.ts - Mobile platform detection and optimization service
 * 
 * Vue 3 / TypeScript version for platform detection and optimization
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'

// Types
export interface PlatformInfo {
  type: string
  isMobile: boolean
  isTablet: boolean
  isTV: boolean
  isNative: boolean
  version?: string
  model?: string
}

export interface DeviceCapabilities {
  touch: boolean
  multiTouch: boolean
  maxTouchPoints: number
  orientation: boolean
  geolocation: boolean
  camera: boolean
  microphone: boolean
  vibration: boolean
  webGL: boolean
  webAudio: boolean
  serviceWorker: boolean
  pushNotifications: boolean
  storage: {
    localStorage: boolean
    sessionStorage: boolean
    indexedDB: boolean
  }
  network: {
    online: boolean
    connectionType?: string
    downlink?: number
    rtt?: number
  }
}

export interface OptimizationSettings {
  graphics: {
    quality: 'low' | 'medium' | 'high'
    shadows: boolean
    particles: boolean
    antialiasing: boolean
  }
  audio: {
    enabled: boolean
    quality: 'low' | 'medium' | 'high'
    spatialAudio: boolean
  }
  performance: {
    targetFPS: number
    maxDrawCalls: number
    textureQuality: 'low' | 'medium' | 'high'
  }
  controls: {
    touchSensitivity: number
    virtualJoystick: boolean
    gestureSupport: boolean
  }
}

export interface NetworkInfo {
  effectiveType: string
  downlink: number
  rtt: number
  saveData: boolean
}

/**
 * Mobile Platform Detection and Optimization Service
 * 
 * Provides platform detection, capability assessment, and optimization
 * settings for mobile devices, tablets, and smart TV platforms.
 * 
 * @class MobilePlatformService
 * @description Detects device platform, capabilities, and provides optimization settings
 * 
 * @example
 * ```typescript
 * import { mobilePlatformService } from './MobilePlatformService'
 * 
 * // Get platform information
 * const platform = mobilePlatformService.getPlatform()
 * console.log(`Platform: ${platform.type}, Mobile: ${platform.isMobile}`)
 * 
 * // Get optimization settings
 * const optimizations = mobilePlatformService.getOptimizations()
 * console.log(`Graphics quality: ${optimizations.graphics.quality}`)
 * ```
 */
export class MobilePlatformService {
  // Reactive state
  private _platform = ref<PlatformInfo>({
    type: 'unknown',
    isMobile: false,
    isTablet: false,
    isTV: false,
    isNative: false
  })

  private _capabilities = ref<DeviceCapabilities>({
    touch: false,
    multiTouch: false,
    maxTouchPoints: 0,
    orientation: false,
    geolocation: false,
    camera: false,
    microphone: false,
    vibration: false,
    webGL: false,
    webAudio: false,
    serviceWorker: false,
    pushNotifications: false,
    storage: {
      localStorage: false,
      sessionStorage: false,
      indexedDB: false
    },
    network: {
      online: navigator.onLine
    }
  })

  private _optimizations = ref<OptimizationSettings>({
    graphics: {
      quality: 'medium',
      shadows: true,
      particles: true,
      antialiasing: true
    },
    audio: {
      enabled: true,
      quality: 'medium',
      spatialAudio: false
    },
    performance: {
      targetFPS: 60,
      maxDrawCalls: 1000,
      textureQuality: 'medium'
    },
    controls: {
      touchSensitivity: 1.0,
      virtualJoystick: false,
      gestureSupport: true
    }
  })

  private _networkInfo = ref<NetworkInfo>({
    effectiveType: '4g',
    downlink: 10,
    rtt: 50,
    saveData: false
  })

  // Computed properties
  public readonly platform = computed(() => this._platform.value)
  public readonly capabilities = computed(() => this._capabilities.value)
  public readonly optimizations = computed(() => this._optimizations.value)
  public readonly networkInfo = computed(() => this._networkInfo.value)

  /**
   * Constructor - Initializes the service
   * 
   * @description Automatically detects platform, capabilities, and sets optimization settings
   * @sideEffects - Modifies reactive state (_platform, _capabilities, _optimizations, _networkInfo)
   * @sideEffects - Sets up event listeners for network, orientation, and resize events
   * @sideEffects - Calls detectPlatform(), detectCapabilities(), setOptimizations(), setupEventListeners()
   */
  constructor() {
    this.initialize()
  }

  /**
   * Initialize the service
   * 
   * @description Sets up platform detection, capabilities assessment, and event listeners
   * @sideEffects - Calls detectPlatform(), detectCapabilities(), setOptimizations(), setupEventListeners()
   * @private
   */
  private initialize(): void {
    this.detectPlatform()
    this.detectCapabilities()
    this.setOptimizations()
    this.setupEventListeners()
  }

  /**
   * Detect the current platform
   * 
   * @description Analyzes user agent, platform, screen dimensions, and touch capabilities
   * @sideEffects - Updates _platform.value with detected platform information
   * @inputs - navigator.userAgent (string), navigator.platform (string), navigator.maxTouchPoints (number), window.innerWidth (pixels), window.innerHeight (pixels)
   * @outputs - PlatformInfo object with type, isMobile, isTablet, isTV, isNative flags
   * @units - Screen dimensions in pixels (px), touch points as count (integer)
   * @private
   */
  private detectPlatform(): void {
    const userAgent = navigator.userAgent.toLowerCase()
    const platform = navigator.platform.toLowerCase()
    const maxTouchPoints = navigator.maxTouchPoints || 0
    const screenWidth = window.innerWidth
    const screenHeight = window.innerHeight

    // iOS detection
    if (userAgent.includes('iphone') || userAgent.includes('ipad') || userAgent.includes('ipod')) {
      const isTablet = userAgent.includes('ipad') || 
                      (userAgent.includes('macintosh') && maxTouchPoints > 0) ||
                      (screenWidth >= 768 && screenHeight >= 1024)
      
      this._platform.value = {
        type: 'ios',
        isMobile: !isTablet,
        isTablet: isTablet,
        isTV: false,
        isNative: true,
        version: this.extractIOSVersion(userAgent)
      }
      return
    }

    // Android detection
    if (userAgent.includes('android')) {
      // FireTV detection
      if (userAgent.includes('aftn') || userAgent.includes('aftb')) {
        this._platform.value = {
          type: 'firetv',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        }
        return
      }

      // LG WebOS detection
      if (userAgent.includes('webos')) {
        this._platform.value = {
          type: 'lgwebos',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        }
        return
      }

      // Regular Android
      const isTablet = screenWidth >= 600 && screenHeight >= 960
      this._platform.value = {
        type: 'android',
        isMobile: !isTablet,
        isTablet: isTablet,
        isTV: false,
        isNative: true,
        version: this.extractAndroidVersion(userAgent)
      }
      return
    }

    // Roku detection
    if (userAgent.includes('roku')) {
      this._platform.value = {
        type: 'roku',
        isMobile: false,
        isTablet: false,
        isTV: true,
        isNative: true
      }
      return
    }

    // Windows detection
    if (userAgent.includes('windows')) {
      // Windows Phone
      if (userAgent.includes('windows phone')) {
        this._platform.value = {
          type: 'windowsphone',
          isMobile: true,
          isTablet: false,
          isTV: false,
          isNative: true
        }
        return
      }

      // Windows TV
      if (userAgent.includes('xbox') || userAgent.includes('smart-tv')) {
        this._platform.value = {
          type: 'windowstv',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        }
        return
      }

      // Regular Windows
      this._platform.value = {
        type: 'windows',
        isMobile: false,
        isTablet: false,
        isTV: false,
        isNative: false
      }
      return
    }

    // macOS detection
    if (userAgent.includes('macintosh')) {
      this._platform.value = {
        type: 'macos',
        isMobile: false,
        isTablet: false,
        isTV: false,
        isNative: false
      }
      return
    }

    // Linux detection (but check for TV platforms first)
    if (userAgent.includes('linux')) {
      // Check if it's a TV platform that uses Linux
      if (userAgent.includes('smart-tv') || userAgent.includes('tizen')) {
        this._platform.value = {
          type: 'samsungtv',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        }
        return
      }
      
      // Regular Linux
      this._platform.value = {
        type: 'linux',
        isMobile: false,
        isTablet: false,
        isTV: false,
        isNative: false
      }
      return
    }

    // Default to desktop
    this._platform.value = {
      type: 'desktop',
      isMobile: false,
      isTablet: false,
      isTV: false,
      isNative: false
    }
  }

  /**
   * Detect device capabilities
   * 
   * @description Tests for various device capabilities including touch, media, storage, and network
   * @sideEffects - Updates _capabilities.value with detected capabilities
   * @inputs - window.ontouchstart (boolean), navigator.maxTouchPoints (number), window.DeviceOrientationEvent (function), navigator.geolocation (object), navigator.mediaDevices (object), navigator.vibrate (function), window.WebGLRenderingContext (function), window.AudioContext (function), navigator.serviceWorker (object), window.PushManager (function), window.localStorage (object), window.sessionStorage (object), window.indexedDB (object), navigator.onLine (boolean), navigator.connection (object)
   * @outputs - DeviceCapabilities object with boolean flags and numeric values
   * @units - Touch points as count (integer), network speed in Mbps (number), network latency in milliseconds (number)
   * @private
   */
  private detectCapabilities(): void {
    const capabilities = this._capabilities.value

    // Touch capabilities
    capabilities.touch = 'ontouchstart' in window || navigator.maxTouchPoints > 0
    capabilities.maxTouchPoints = navigator.maxTouchPoints || 0
    capabilities.multiTouch = capabilities.maxTouchPoints > 1

    // Device orientation
    capabilities.orientation = 'DeviceOrientationEvent' in window

    // Geolocation
    capabilities.geolocation = 'geolocation' in navigator

    // Media capabilities
    capabilities.camera = 'mediaDevices' in navigator
    capabilities.microphone = capabilities.camera

    // Vibration
    capabilities.vibration = 'vibrate' in navigator

    // WebGL
    capabilities.webGL = this.detectWebGL()

    // Web Audio
    capabilities.webAudio = 'AudioContext' in window || 'webkitAudioContext' in window

    // Service Worker
    capabilities.serviceWorker = 'serviceWorker' in navigator

    // Push Notifications
    capabilities.pushNotifications = 'PushManager' in window

    // Storage capabilities
    capabilities.storage.localStorage = this.testLocalStorage()
    capabilities.storage.sessionStorage = this.testSessionStorage()
    capabilities.storage.indexedDB = this.testIndexedDB()

    // Network capabilities
    capabilities.network.online = navigator.onLine
    if ('connection' in navigator) {
      const connection = (navigator as any).connection
      capabilities.network.connectionType = connection.effectiveType
      capabilities.network.downlink = connection.downlink
      capabilities.network.rtt = connection.rtt
    }
  }

  /**
   * Set optimization settings based on platform
   * 
   * @description Configures graphics, audio, performance, and control settings based on detected platform and capabilities
   * @sideEffects - Updates _optimizations.value with platform-specific settings
   * @inputs - _platform.value (PlatformInfo), _capabilities.value (DeviceCapabilities)
   * @outputs - OptimizationSettings object with quality levels, boolean flags, and numeric values
   * @units - FPS as frames per second (integer), draw calls as count (integer), touch sensitivity as multiplier (float)
   * @private
   */
  private setOptimizations(): void {
    const platform = this._platform.value
    const capabilities = this._capabilities.value
    const optimizations = this._optimizations.value

    // Mobile optimizations
    if (platform.isMobile) {
      optimizations.graphics.quality = 'low'
      optimizations.graphics.shadows = false
      optimizations.graphics.particles = false
      optimizations.graphics.antialiasing = false
      optimizations.performance.targetFPS = 30
      optimizations.performance.maxDrawCalls = 500
      optimizations.performance.textureQuality = 'low'
      optimizations.controls.virtualJoystick = true
    }

    // Tablet optimizations
    if (platform.isTablet) {
      optimizations.graphics.quality = 'medium'
      optimizations.graphics.shadows = true
      optimizations.graphics.particles = true
      optimizations.graphics.antialiasing = false
      optimizations.performance.targetFPS = 45
      optimizations.performance.maxDrawCalls = 750
      optimizations.performance.textureQuality = 'medium'
      optimizations.controls.virtualJoystick = true
    }

    // TV optimizations
    if (platform.isTV) {
      optimizations.graphics.quality = 'high'
      optimizations.graphics.shadows = true
      optimizations.graphics.particles = true
      optimizations.graphics.antialiasing = true
      optimizations.performance.targetFPS = 60
      optimizations.performance.maxDrawCalls = 1500
      optimizations.performance.textureQuality = 'high'
      optimizations.controls.virtualJoystick = false
      optimizations.controls.gestureSupport = false
    }

    // Desktop optimizations (default)
    if (!platform.isMobile && !platform.isTablet && !platform.isTV) {
      optimizations.graphics.quality = 'high'
      optimizations.graphics.shadows = true
      optimizations.graphics.particles = true
      optimizations.graphics.antialiasing = true
      optimizations.performance.targetFPS = 60
      optimizations.performance.maxDrawCalls = 1000
      optimizations.performance.textureQuality = 'high'
      optimizations.controls.virtualJoystick = false
      optimizations.controls.gestureSupport = false
    }

    // Low-end device optimizations (overrides desktop for truly low-end devices)
    if (capabilities.maxTouchPoints === 0 && !capabilities.webGL && platform.isMobile) {
      optimizations.graphics.quality = 'low'
      optimizations.graphics.shadows = false
      optimizations.graphics.particles = false
      optimizations.graphics.antialiasing = false
      optimizations.performance.targetFPS = 30
      optimizations.performance.maxDrawCalls = 300
      optimizations.performance.textureQuality = 'low'
    }
  }

  /**
   * Setup event listeners
   * 
   * @description Sets up listeners for network, orientation, and resize events
   * @sideEffects - Adds event listeners to window and navigator.connection
   * @inputs - window (object), navigator.connection (object), _capabilities.value.orientation (boolean)
   * @private
   */
  private setupEventListeners(): void {
    // Network status changes
    window.addEventListener('online', () => {
      this._capabilities.value.network.online = true
    })

    window.addEventListener('offline', () => {
      this._capabilities.value.network.online = false
    })

    // Connection changes
    if ('connection' in navigator) {
      const connection = (navigator as any).connection
      connection.addEventListener('change', () => {
        this._capabilities.value.network.connectionType = connection.effectiveType
        this._capabilities.value.network.downlink = connection.downlink
        this._capabilities.value.network.rtt = connection.rtt
      })
    }

    // Screen orientation changes
    if (this._capabilities.value.orientation) {
      window.addEventListener('orientationchange', () => {
        this.updateOptimizationsInternal()
      })
    }

    // Resize events
    window.addEventListener('resize', () => {
      this.updateOptimizationsInternal()
    })
  }

  /**
   * Update optimizations based on current conditions
   * 
   * @description Re-detects platform and updates optimization settings
   * @sideEffects - Calls detectPlatform() and setOptimizations()
   * @private
   */
  private updateOptimizationsInternal(): void {
    // Re-detect platform for orientation changes
    this.detectPlatform()
    
    // Update optimizations
    this.setOptimizations()
  }

  /**
   * Extract iOS version from user agent
   * 
   * @description Parses iOS version from user agent string
   * @inputs - userAgent (string) - iOS user agent string
   * @outputs - string | undefined - iOS version (e.g., "14.0") or undefined
   * @private
   */
  private extractIOSVersion(userAgent: string): string | undefined {
    const match = userAgent.match(/os (\d+)_(\d+)_?(\d+)?/)
    if (match) {
      return `${match[1]}.${match[2]}${match[3] ? `.${match[3]}` : ''}`
    }
    return undefined
  }

  /**
   * Extract Android version from user agent
   * 
   * @description Parses Android version from user agent string
   * @inputs - userAgent (string) - Android user agent string
   * @outputs - string | undefined - Android version (e.g., "10") or undefined
   * @private
   */
  private extractAndroidVersion(userAgent: string): string | undefined {
    const match = userAgent.match(/android (\d+\.\d+)/)
    return match ? match[1] : undefined
  }

  /**
   * Detect WebGL support
   * 
   * @description Tests for WebGL support by creating a canvas and getting WebGL context
   * @inputs - window.WebGLRenderingContext (function), document.createElement (function)
   * @outputs - boolean - true if WebGL is supported, false otherwise
   * @sideEffects - Creates temporary canvas element
   * @private
   */
  private detectWebGL(): boolean {
    try {
      const canvas = document.createElement('canvas')
      const context = canvas.getContext('webgl') || canvas.getContext('experimental-webgl')
      return !!(window.WebGLRenderingContext && context)
    } catch (e) {
      return false
    }
  }

  /**
   * Test localStorage support
   * 
   * @description Tests if localStorage is available and functional
   * @inputs - window.localStorage (object)
   * @outputs - boolean - true if localStorage works, false otherwise
   * @sideEffects - May modify localStorage (sets and removes test item)
   * @private
   */
  private testLocalStorage(): boolean {
    try {
      const test = 'test'
      localStorage.setItem(test, test)
      localStorage.removeItem(test)
      return true
    } catch (e) {
      return false
    }
  }

  /**
   * Test sessionStorage support
   * 
   * @description Tests if sessionStorage is available and functional
   * @inputs - window.sessionStorage (object)
   * @outputs - boolean - true if sessionStorage works, false otherwise
   * @sideEffects - May modify sessionStorage (sets and removes test item)
   * @private
   */
  private testSessionStorage(): boolean {
    try {
      const test = 'test'
      sessionStorage.setItem(test, test)
      sessionStorage.removeItem(test)
      return true
    } catch (e) {
      return false
    }
  }

  /**
   * Test IndexedDB support
   * 
   * @description Tests if IndexedDB is available
   * @inputs - window.indexedDB (object)
   * @outputs - boolean - true if IndexedDB is available, false otherwise
   * @private
   */
  private testIndexedDB(): boolean {
    return 'indexedDB' in window
  }

  /**
   * Get platform information
   * 
   * @description Returns current platform detection results
   * @outputs - PlatformInfo object with type, isMobile, isTablet, isTV, isNative flags
   * @returns {PlatformInfo} Current platform information
   */
  getPlatform(): PlatformInfo {
    return this.platform.value
  }

  /**
   * Get device capabilities
   * 
   * @description Returns current device capability assessment
   * @outputs - DeviceCapabilities object with boolean flags and numeric values
   * @returns {DeviceCapabilities} Current device capabilities
   */
  getCapabilities(): DeviceCapabilities {
    return this.capabilities.value
  }

  /**
   * Get optimization settings
   * 
   * @description Returns current optimization settings based on platform and capabilities
   * @outputs - OptimizationSettings object with quality levels, boolean flags, and numeric values
   * @returns {OptimizationSettings} Current optimization settings
   */
  getOptimizations(): OptimizationSettings {
    return this.optimizations.value
  }

  /**
   * Update optimization settings
   * 
   * @description Manually updates optimization settings
   * @inputs - settings (Partial<OptimizationSettings>) - Partial settings to merge
   * @sideEffects - Updates _optimizations.value by merging with provided settings
   * @param {Partial<OptimizationSettings>} settings - Settings to update
   */
  updateOptimizations(settings: Partial<OptimizationSettings>): void {
    this._optimizations.value = { ...this._optimizations.value, ...settings }
  }

  /**
   * Check if device supports a specific feature
   * 
   * @description Tests if a specific capability is available
   * @inputs - feature (keyof DeviceCapabilities) - Feature name to test
   * @outputs - boolean - true if feature is supported, false otherwise
   * @param {keyof DeviceCapabilities} feature - Feature to check
   * @returns {boolean} Whether the feature is supported
   */
  supportsFeature(feature: keyof DeviceCapabilities): boolean {
    return this._capabilities.value[feature] as boolean
  }

  /**
   * Get network information
   * 
   * @description Returns current network status and performance metrics
   * @outputs - NetworkInfo object with connection type, speed, and latency
   * @returns {NetworkInfo} Current network information
   */
  getNetworkInfo(): NetworkInfo {
    return this.networkInfo.value
  }

  /**
   * Check if device is low-end
   * 
   * @description Determines if device has limited capabilities
   * @inputs - _capabilities.value.maxTouchPoints (number), _capabilities.value.webGL (boolean)
   * @outputs - boolean - true if device is low-end, false otherwise
   * @returns {boolean} Whether the device is considered low-end
   */
  isLowEndDevice(): boolean {
    const capabilities = this._capabilities.value
    return capabilities.maxTouchPoints === 0 && !capabilities.webGL
  }

  /**
   * Get recommended settings for current device
   * 
   * @description Returns optimal settings based on platform and capabilities
   * @inputs - _platform.value (PlatformInfo), _capabilities.value (DeviceCapabilities)
   * @outputs - OptimizationSettings object with recommended configuration
   * @returns {OptimizationSettings} Recommended settings for current device
   */
  getRecommendedSettings(): OptimizationSettings {
    const platform = this._platform.value
    const capabilities = this._capabilities.value

    if (platform.isTV) {
      return {
        graphics: { quality: 'high', shadows: true, particles: true, antialiasing: true },
        audio: { enabled: true, quality: 'high', spatialAudio: true },
        performance: { targetFPS: 60, maxDrawCalls: 1500, textureQuality: 'high' },
        controls: { touchSensitivity: 1.0, virtualJoystick: false, gestureSupport: false }
      }
    }

    if (platform.isTablet) {
      return {
        graphics: { quality: 'medium', shadows: true, particles: true, antialiasing: false },
        audio: { enabled: true, quality: 'medium', spatialAudio: false },
        performance: { targetFPS: 45, maxDrawCalls: 750, textureQuality: 'medium' },
        controls: { touchSensitivity: 1.0, virtualJoystick: true, gestureSupport: true }
      }
    }

    if (platform.isMobile) {
      return {
        graphics: { quality: 'low', shadows: false, particles: false, antialiasing: false },
        audio: { enabled: true, quality: 'low', spatialAudio: false },
        performance: { targetFPS: 30, maxDrawCalls: 500, textureQuality: 'low' },
        controls: { touchSensitivity: 1.2, virtualJoystick: true, gestureSupport: true }
      }
    }

    return {
      graphics: { quality: 'high', shadows: true, particles: true, antialiasing: true },
      audio: { enabled: true, quality: 'high', spatialAudio: true },
      performance: { targetFPS: 60, maxDrawCalls: 1000, textureQuality: 'high' },
      controls: { touchSensitivity: 1.0, virtualJoystick: false, gestureSupport: false }
    }
  }
}

// Export singleton instance
export const mobilePlatformService = new MobilePlatformService()
