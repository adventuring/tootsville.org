import { describe, it, expect, beforeEach, vi } from 'vitest'
import { MobilePlatformService } from '../MobilePlatformService'

// Mock navigator and window objects
const mockNavigator = {
  userAgent: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
  platform: 'Win32',
  maxTouchPoints: 0,
  onLine: true,
  geolocation: {},
  mediaDevices: {},
  vibrate: vi.fn(),
  serviceWorker: {},
  connection: {
    effectiveType: '4g',
    downlink: 10,
    rtt: 50,
    addEventListener: vi.fn(),
    removeEventListener: vi.fn()
  }
}

const mockWindow = {
  innerWidth: 1920,
  innerHeight: 1080,
  devicePixelRatio: 1,
  orientation: 0,
  addEventListener: vi.fn(),
  removeEventListener: vi.fn(),
  WebGLRenderingContext: {},
  AudioContext: {},
  webkitAudioContext: {},
  PushManager: {},
  indexedDB: {},
  ontouchstart: null,
  createElement: vi.fn(() => ({
    getContext: vi.fn(() => null)
  })),
  localStorage: {
    setItem: vi.fn(),
    removeItem: vi.fn()
  },
  sessionStorage: {
    setItem: vi.fn(),
    removeItem: vi.fn()
  }
}

describe('MobilePlatformService', () => {
  let mobilePlatformService: MobilePlatformService

  beforeEach(() => {
    // Reset mocks
    vi.clearAllMocks()
    
    // Mock global objects
    Object.defineProperty(global, 'navigator', {
      value: mockNavigator,
      writable: true
    })
    
    Object.defineProperty(global, 'window', {
      value: mockWindow,
      writable: true
    })
    
    mobilePlatformService = new MobilePlatformService()
  })

  describe('Platform Detection', () => {
    it('should detect desktop platform by default', () => {
      const platform = mobilePlatformService.getPlatform()
      
      expect(platform.type).toBe('desktop')
      expect(platform.isMobile).toBe(false)
      expect(platform.isTablet).toBe(false)
      expect(platform.isTV).toBe(false)
      expect(platform.isNative).toBe(false)
    })

    it('should detect iOS platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)'
      mockNavigator.platform = 'iPhone'
      mockNavigator.maxTouchPoints = 1
      mockWindow.innerWidth = 375
      mockWindow.innerHeight = 667

      const newService = new MobilePlatformService()
      const platform = newService.getPlatform()
      
      expect(platform.type).toBe('ios')
      expect(platform.isMobile).toBe(true)
      expect(platform.isTablet).toBe(false)
      expect(platform.isTV).toBe(false)
      expect(platform.isNative).toBe(true)
    })

    it('should detect Android platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Linux; Android 10; SM-G973F)'
      mockNavigator.platform = 'Linux armv8l'
      mockNavigator.maxTouchPoints = 1
      mockWindow.innerWidth = 360
      mockWindow.innerHeight = 640

      const newService = new MobilePlatformService()
      const platform = newService.getPlatform()
      
      expect(platform.type).toBe('android')
      expect(platform.isMobile).toBe(true)
      expect(platform.isTablet).toBe(false)
      expect(platform.isTV).toBe(false)
      expect(platform.isNative).toBe(true)
    })

    it('should detect FireTV platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Linux; Android 7.1.2; AFTN)'
      mockNavigator.platform = 'Linux armv7l'

      const newService = new MobilePlatformService()
      const platform = newService.getPlatform()
      
      expect(platform.type).toBe('firetv')
      expect(platform.isMobile).toBe(false)
      expect(platform.isTablet).toBe(false)
      expect(platform.isTV).toBe(true)
      expect(platform.isNative).toBe(true)
    })

    it('should detect Samsung TV platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 6.0)'
      mockNavigator.platform = 'Linux armv7l'

      const newService = new MobilePlatformService()
      const platform = newService.getPlatform()
      
      expect(platform.type).toBe('samsungtv')
      expect(platform.isMobile).toBe(false)
      expect(platform.isTablet).toBe(false)
      expect(platform.isTV).toBe(true)
      expect(platform.isNative).toBe(true)
    })
  })

  describe('Device Capabilities', () => {
    it('should detect basic capabilities', () => {
      const capabilities = mobilePlatformService.getCapabilities()
      
      expect(capabilities.touch).toBe(false)
      expect(capabilities.multiTouch).toBe(false)
      expect(capabilities.maxTouchPoints).toBe(0)
      expect(capabilities.geolocation).toBe(true)
      expect(capabilities.camera).toBe(true)
      expect(capabilities.microphone).toBe(true)
      expect(capabilities.vibration).toBe(true)
      expect(capabilities.serviceWorker).toBe(true)
      expect(capabilities.pushNotifications).toBe(true)
    })

    it('should detect touch capabilities', () => {
      mockNavigator.maxTouchPoints = 5
      mockWindow.ontouchstart = null

      const newService = new MobilePlatformService()
      const capabilities = newService.getCapabilities()
      
      expect(capabilities.touch).toBe(true)
      expect(capabilities.multiTouch).toBe(true)
      expect(capabilities.maxTouchPoints).toBe(5)
    })

    it('should detect storage capabilities', () => {
      const capabilities = mobilePlatformService.getCapabilities()
      
      expect(capabilities.storage.localStorage).toBe(true)
      expect(capabilities.storage.sessionStorage).toBe(true)
      expect(capabilities.storage.indexedDB).toBe(true)
    })

    it('should detect network capabilities', () => {
      const capabilities = mobilePlatformService.getCapabilities()
      
      expect(capabilities.network.online).toBe(true)
      expect(capabilities.network.connectionType).toBe('4g')
      expect(capabilities.network.downlink).toBe(10)
      expect(capabilities.network.rtt).toBe(50)
    })
  })

  describe('Optimization Settings', () => {
    it('should provide default optimization settings', () => {
      const optimizations = mobilePlatformService.getOptimizations()
      
      expect(optimizations.graphics.quality).toBe('medium')
      expect(optimizations.graphics.shadows).toBe(true)
      expect(optimizations.graphics.particles).toBe(true)
      expect(optimizations.graphics.antialiasing).toBe(true)
      expect(optimizations.audio.enabled).toBe(true)
      expect(optimizations.audio.quality).toBe('medium')
      expect(optimizations.performance.targetFPS).toBe(60)
    })

    it('should provide mobile optimizations for mobile platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)'
      mockNavigator.platform = 'iPhone'
      mockNavigator.maxTouchPoints = 1
      mockWindow.innerWidth = 375
      mockWindow.innerHeight = 667

      const newService = new MobilePlatformService()
      const optimizations = newService.getOptimizations()
      
      expect(optimizations.graphics.quality).toBe('low')
      expect(optimizations.graphics.shadows).toBe(false)
      expect(optimizations.graphics.particles).toBe(false)
      expect(optimizations.graphics.antialiasing).toBe(false)
      expect(optimizations.performance.targetFPS).toBe(30)
      expect(optimizations.controls.virtualJoystick).toBe(true)
    })

    it('should provide TV optimizations for TV platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 6.0)'
      mockNavigator.platform = 'Linux armv7l'

      const newService = new MobilePlatformService()
      const optimizations = newService.getOptimizations()
      
      expect(optimizations.graphics.quality).toBe('high')
      expect(optimizations.graphics.shadows).toBe(true)
      expect(optimizations.graphics.particles).toBe(true)
      expect(optimizations.graphics.antialiasing).toBe(true)
      expect(optimizations.performance.targetFPS).toBe(60)
      expect(optimizations.controls.virtualJoystick).toBe(false)
      expect(optimizations.controls.gestureSupport).toBe(false)
    })
  })

  describe('Feature Support', () => {
    it('should check if device supports specific features', () => {
      expect(mobilePlatformService.supportsFeature('touch')).toBe(false)
      expect(mobilePlatformService.supportsFeature('geolocation')).toBe(true)
      expect(mobilePlatformService.supportsFeature('camera')).toBe(true)
      expect(mobilePlatformService.supportsFeature('vibration')).toBe(true)
    })
  })

  describe('Network Information', () => {
    it('should provide network information', () => {
      const networkInfo = mobilePlatformService.getNetworkInfo()
      
      expect(networkInfo.effectiveType).toBe('4g')
      expect(networkInfo.downlink).toBe(10)
      expect(networkInfo.rtt).toBe(50)
      expect(networkInfo.saveData).toBe(false)
    })
  })

  describe('Low-End Device Detection', () => {
    it('should detect low-end devices', () => {
      expect(mobilePlatformService.isLowEndDevice()).toBe(true)
    })

    it('should not detect high-end devices as low-end', () => {
      mockNavigator.maxTouchPoints = 5
      mockWindow.WebGLRenderingContext = { prototype: {} }

      const newService = new MobilePlatformService()
      expect(newService.isLowEndDevice()).toBe(false)
    })
  })

  describe('Recommended Settings', () => {
    it('should provide recommended settings for desktop', () => {
      const settings = mobilePlatformService.getRecommendedSettings()
      
      expect(settings.graphics.quality).toBe('high')
      expect(settings.graphics.shadows).toBe(true)
      expect(settings.graphics.particles).toBe(true)
      expect(settings.graphics.antialiasing).toBe(true)
      expect(settings.audio.quality).toBe('high')
      expect(settings.performance.targetFPS).toBe(60)
      expect(settings.controls.virtualJoystick).toBe(false)
    })

    it('should provide recommended settings for mobile', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X)'
      mockNavigator.platform = 'iPhone'
      mockNavigator.maxTouchPoints = 1
      mockWindow.innerWidth = 375
      mockWindow.innerHeight = 667

      const newService = new MobilePlatformService()
      const settings = newService.getRecommendedSettings()
      
      expect(settings.graphics.quality).toBe('low')
      expect(settings.graphics.shadows).toBe(false)
      expect(settings.graphics.particles).toBe(false)
      expect(settings.graphics.antialiasing).toBe(false)
      expect(settings.audio.quality).toBe('low')
      expect(settings.performance.targetFPS).toBe(30)
      expect(settings.controls.virtualJoystick).toBe(true)
    })
  })

  describe('Settings Updates', () => {
    it('should update optimization settings', () => {
      const newSettings = {
        graphics: {
          quality: 'high' as const,
          shadows: false,
          particles: false,
          antialiasing: false
        },
        performance: {
          targetFPS: 30,
          maxDrawCalls: 500,
          textureQuality: 'low' as const
        }
      }

      mobilePlatformService.updateOptimizations(newSettings)
      const optimizations = mobilePlatformService.getOptimizations()
      
      expect(optimizations.graphics.quality).toBe('high')
      expect(optimizations.graphics.shadows).toBe(false)
      expect(optimizations.graphics.particles).toBe(false)
      expect(optimizations.graphics.antialiasing).toBe(false)
      expect(optimizations.performance.targetFPS).toBe(30)
      expect(optimizations.performance.maxDrawCalls).toBe(500)
      expect(optimizations.performance.textureQuality).toBe('low')
    })
  })
})
