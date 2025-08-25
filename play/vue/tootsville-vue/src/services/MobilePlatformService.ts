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

  constructor() {
    this.initialize()
  }

  /**
   * Initialize the service
   */
  private initialize(): void {
    this.detectPlatform()
    this.detectCapabilities()
    this.getOptimizations()
    this.setupEventListeners()
  }

  /**
   * Detect the current platform
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

      // Samsung TV detection
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

    // Linux detection
    if (userAgent.includes('linux')) {
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
   * Get optimization settings based on platform
   */
  private getOptimizations(): void {
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

    // Low-end device optimizations
    if (capabilities.maxTouchPoints === 0 && !capabilities.webGL) {
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
   */
  private updateOptimizationsInternal(): void {
    // Re-detect platform for orientation changes
    this.detectPlatform()
    
    // Update optimizations
    this.getOptimizations()
  }

  /**
   * Extract iOS version from user agent
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
   */
  private extractAndroidVersion(userAgent: string): string | undefined {
    const match = userAgent.match(/android (\d+\.\d+)/)
    return match ? match[1] : undefined
  }

  /**
   * Detect WebGL support
   */
  private detectWebGL(): boolean {
    try {
      const canvas = document.createElement('canvas')
      return !!(window.WebGLRenderingContext && 
               (canvas.getContext('webgl') || canvas.getContext('experimental-webgl')))
    } catch (e) {
      return false
    }
  }

  /**
   * Test localStorage support
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
   */
  private testIndexedDB(): boolean {
    return 'indexedDB' in window
  }

  /**
   * Get platform information
   */
  getPlatform(): PlatformInfo {
    return this.platform.value
  }

  /**
   * Get device capabilities
   */
  getCapabilities(): DeviceCapabilities {
    return this.capabilities.value
  }

  /**
   * Get optimization settings
   */
  getOptimizations(): OptimizationSettings {
    return this.optimizations.value
  }

  /**
   * Update optimization settings
   */
  updateOptimizations(settings: Partial<OptimizationSettings>): void {
    this._optimizations.value = { ...this._optimizations.value, ...settings }
  }

  /**
   * Check if device supports a specific feature
   */
  supportsFeature(feature: keyof DeviceCapabilities): boolean {
    return this._capabilities.value[feature] as boolean
  }

  /**
   * Get network information
   */
  getNetworkInfo(): NetworkInfo {
    return this.networkInfo.value
  }

  /**
   * Check if device is low-end
   */
  isLowEndDevice(): boolean {
    const capabilities = this._capabilities.value
    return capabilities.maxTouchPoints === 0 && !capabilities.webGL
  }

  /**
   * Get recommended settings for current device
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
