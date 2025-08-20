/**
 * MobilePlatformService.js - Mobile platform detection and optimization
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides platform detection, optimization, and native app bundling support
 * for Android, iOS, FireTV, and other smart device platforms.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

/**
 * Mobile platform detection and optimization service
 * Supports Android, iOS, FireTV, and other smart device platforms
 */
class MobilePlatformService {
  constructor() {
    this.platform = this.detectPlatform();
    this.capabilities = this.detectCapabilities();
    this.optimizations = this.getOptimizations();
  }

  /**
   * Detect the current mobile platform
   * @returns {Object} Platform information including type, version, and features
   */
  detectPlatform() {
    const userAgent = navigator.userAgent;
    const platform = {
      type: 'desktop',
      name: 'unknown',
      version: 'unknown',
      isMobile: false,
      isTablet: false,
      isTV: false,
      isNative: false,
      features: {}
    };

    // Android detection
    if (/Android/i.test(userAgent)) {
      platform.type = 'android';
      platform.name = 'Android';
      platform.isMobile = true;
      
      const match = userAgent.match(/Android\s([0-9.]*)/);
      if (match) platform.version = match[1];
      
      // Check for native app wrapper (WebView)
      if (window.AndroidInterface || window.Android) {
        platform.isNative = true;
        platform.features.nativeInterface = true;
      }
      
      // Check for FireTV
      if (/AFT/i.test(userAgent) || /Amazon Fire TV/i.test(userAgent)) {
        platform.type = 'firetv';
        platform.name = 'Amazon Fire TV';
        platform.isTV = true;
        platform.isMobile = false;
      }
    }
    
    // iOS detection
    else if (/iPhone|iPad|iPod/i.test(userAgent)) {
      platform.type = 'ios';
      platform.isMobile = true;
      
      if (/iPad/i.test(userAgent)) {
        platform.name = 'iPad';
        platform.isTablet = true;
      } else if (/iPhone/i.test(userAgent)) {
        platform.name = 'iPhone';
      } else {
        platform.name = 'iPod';
      }
      
      const match = userAgent.match(/OS\s([0-9_]*)/);
      if (match) platform.version = match[1].replace(/_/g, '.');
      
      // Check for native app wrapper (WKWebView)
      if (window.webkit && window.webkit.messageHandlers) {
        platform.isNative = true;
        platform.features.nativeInterface = true;
      }
    }
    
    // Samsung Smart TV
    else if (/SmartTV|SamsungBrowser/i.test(userAgent)) {
      platform.type = 'samsungtv';
      platform.name = 'Samsung Smart TV';
      platform.isTV = true;
    }
    
    // LG WebOS TV
    else if (/WebOS/i.test(userAgent)) {
      platform.type = 'lgwebos';
      platform.name = 'LG WebOS TV';
      platform.isTV = true;
    }
    
    // Roku TV
    else if (/Roku/i.test(userAgent)) {
      platform.type = 'roku';
      platform.name = 'Roku TV';
      platform.isTV = true;
    }

    return platform;
  }

  /**
   * Detect platform capabilities and limitations
   * @returns {Object} Capabilities object with performance and feature flags
   */
  detectCapabilities() {
    const capabilities = {
      webGL: this.checkWebGLSupport(),
      touch: 'ontouchstart' in window,
      orientation: 'onorientationchange' in window,
      vibration: 'vibrate' in navigator,
      geolocation: 'geolocation' in navigator,
      deviceMotion: 'ondevicemotion' in window,
      deviceOrientation: 'ondeviceorientation' in window,
      battery: 'getBattery' in navigator,
      network: 'connection' in navigator,
      storage: this.checkStorageSupport(),
      performance: this.assessPerformance()
    };

    return capabilities;
  }

  /**
   * Check WebGL support and capabilities
   * @returns {Object} WebGL support information
   */
  checkWebGLSupport() {
    const canvas = document.createElement('canvas');
    const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
    
    if (!gl) {
      return { supported: false, version: null, maxTextureSize: 0 };
    }

    const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
    const maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE);
    const maxVertexUniformVectors = gl.getParameter(gl.MAX_VERTEX_UNIFORM_VECTORS);
    const maxFragmentUniformVectors = gl.getParameter(gl.MAX_FRAGMENT_UNIFORM_VECTORS);

    return {
      supported: true,
      version: gl.getParameter(gl.VERSION),
      renderer: debugInfo ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL) : 'unknown',
      maxTextureSize,
      maxVertexUniformVectors,
      maxFragmentUniformVectors,
      isLowEnd: maxTextureSize < 2048 || maxVertexUniformVectors < 128
    };
  }

  /**
   * Check storage support (localStorage, IndexedDB, etc.)
   * @returns {Object} Storage capabilities
   */
  checkStorageSupport() {
    return {
      localStorage: this.testLocalStorage(),
      sessionStorage: this.testSessionStorage(),
      indexedDB: this.testIndexedDB(),
      webSQL: this.testWebSQL()
    };
  }

  /**
   * Test localStorage availability
   * @returns {boolean} True if localStorage is available
   */
  testLocalStorage() {
    try {
      const test = '__localStorage_test__';
      localStorage.setItem(test, test);
      localStorage.removeItem(test);
      return true;
    } catch (e) {
      return false;
    }
  }

  /**
   * Test sessionStorage availability
   * @returns {boolean} True if sessionStorage is available
   */
  testSessionStorage() {
    try {
      const test = '__sessionStorage_test__';
      sessionStorage.setItem(test, test);
      sessionStorage.removeItem(test);
      return true;
    } catch (e) {
      return false;
    }
  }

  /**
   * Test IndexedDB availability
   * @returns {boolean} True if IndexedDB is available
   */
  testIndexedDB() {
    return !!(window.indexedDB || window.webkitIndexedDB || window.mozIndexedDB);
  }

  /**
   * Test WebSQL availability
   * @returns {boolean} True if WebSQL is available
   */
  testWebSQL() {
    return !!(window.openDatabase);
  }

  /**
   * Assess device performance characteristics
   * @returns {Object} Performance assessment
   */
  assessPerformance() {
    const start = performance.now();
    let operations = 0;
    
    // Simple performance test
    for (let i = 0; i < 1000000; i++) {
      operations++;
    }
    
    const duration = performance.now() - start;
    const opsPerMs = operations / duration;
    
    return {
      operationsPerMs: opsPerMs,
      isLowEnd: opsPerMs < 1000,
      isMidRange: opsPerMs >= 1000 && opsPerMs < 5000,
      isHighEnd: opsPerMs >= 5000,
      memory: this.estimateMemory()
    };
  }

  /**
   * Estimate available memory
   * @returns {Object} Memory information
   */
  estimateMemory() {
    if ('memory' in performance) {
      return {
        used: performance.memory.usedJSHeapSize,
        total: performance.memory.totalJSHeapSize,
        limit: performance.memory.jsHeapSizeLimit,
        available: performance.memory.jsHeapSizeLimit - performance.memory.usedJSHeapSize
      };
    }
    
    // Fallback estimation based on platform
    const platform = this.platform;
    if (platform.type === 'android') {
      return { estimated: '2-8GB', isLowEnd: platform.version < 8 };
    } else if (platform.type === 'ios') {
      return { estimated: '2-6GB', isLowEnd: platform.version < 12 };
    }
    
    return { estimated: 'unknown' };
  }

  /**
   * @function getOptimizations
   * @description Get platform-specific optimizations and settings
   * @returns {Object} Optimization settings for the current platform
   */
  getOptimizations() {
    const platform = this.platform;
    const capabilities = this.capabilities;
    
    const optimizations = {
      // Rendering optimizations
      renderScale: 1.0,
      maxFPS: 60,
      enableShadows: true,
      enablePostProcessing: true,
      enableAntiAliasing: true,
      
      // Performance optimizations
      enableLOD: true,
      maxDrawDistance: 1000,
      enableFrustumCulling: true,
      enableOcclusionCulling: false,
      
      // Mobile-specific optimizations
      enableTouchOptimization: platform.isMobile,
      enableBatteryOptimization: platform.isMobile,
      enableNetworkOptimization: true,
      
      // TV-specific optimizations
      enableTVOptimization: platform.isTV,
      enableRemoteControl: platform.isTV,
      
      // Native app optimizations
      enableNativeFeatures: platform.isNative
    };

    // Apply performance-based adjustments
    if (capabilities.performance.isLowEnd) {
      optimizations.renderScale = 0.5;
      optimizations.maxFPS = 30;
      optimizations.enableShadows = false;
      optimizations.enablePostProcessing = false;
      optimizations.enableAntiAliasing = false;
      optimizations.maxDrawDistance = 500;
    } else if (capabilities.performance.isMidRange) {
      optimizations.renderScale = 0.75;
      optimizations.maxFPS = 45;
      optimizations.enableShadows = true;
      optimizations.enablePostProcessing = false;
      optimizations.maxDrawDistance = 750;
    }

    // Apply WebGL-based adjustments
    if (capabilities.webGL.isLowEnd) {
      optimizations.enableShadows = false;
      optimizations.enablePostProcessing = false;
      optimizations.maxDrawDistance = 300;
    }

    // Apply platform-specific adjustments
    if (platform.type === 'firetv') {
      optimizations.enableTVOptimization = true;
      optimizations.enableRemoteControl = true;
      optimizations.maxFPS = 60; // FireTV can handle 60fps
    }

    if (platform.type === 'ios') {
      optimizations.enableBatteryOptimization = true;
      optimizations.enableTouchOptimization = true;
    }

    if (platform.type === 'android') {
      optimizations.enableBatteryOptimization = true;
      optimizations.enableTouchOptimization = true;
    }

    return optimizations;
  }

  /**
   * Get native app interface if available
   * @returns {Object|null} Native interface object
   */
  getNativeInterface() {
    if (this.platform.type === 'android' && window.AndroidInterface) {
      return window.AndroidInterface;
    }
    
    if (this.platform.type === 'ios' && window.webkit && window.webkit.messageHandlers) {
      return window.webkit.messageHandlers;
    }
    
    return null;
  }

  /**
   * Send message to native app
   * @param {string} action - Action to perform
   * @param {Object} data - Data to send
   */
  sendToNative(action, data = {}) {
    const nativeInterface = this.getNativeInterface();
    
    if (!nativeInterface) {
      console.warn('No native interface available');
      return;
    }

    if (this.platform.type === 'android') {
      nativeInterface.postMessage(JSON.stringify({ action, data }));
    } else if (this.platform.type === 'ios') {
      if (nativeInterface[action]) {
        nativeInterface[action].postMessage(data);
      }
    }
  }

  /**
   * Request fullscreen mode
   */
  requestFullscreen() {
    if (this.platform.isNative) {
      this.sendToNative('requestFullscreen');
    } else {
      const elem = document.documentElement;
      if (elem.requestFullscreen) {
        elem.requestFullscreen();
      } else if (elem.webkitRequestFullscreen) {
        elem.webkitRequestFullscreen();
      } else if (elem.msRequestFullscreen) {
        elem.msRequestFullscreen();
      }
    }
  }

  /**
   * Lock screen orientation
   * @param {string} orientation - 'portrait', 'landscape', or 'auto'
   */
  lockOrientation(orientation) {
    if (this.platform.isNative) {
      this.sendToNative('lockOrientation', { orientation });
    } else if (screen.orientation && screen.orientation.lock) {
      screen.orientation.lock(orientation);
    }
  }

  /**
   * Vibrate device (if supported)
   * @param {number|Array} pattern - Vibration pattern in milliseconds
   */
  vibrate(pattern = 200) {
    if (this.capabilities.vibration) {
      navigator.vibrate(pattern);
    }
  }

  /**
   * Get device orientation
   * @returns {Object} Orientation information
   */
  getOrientation() {
    return {
      angle: window.orientation || 0,
      type: window.orientation === 0 || window.orientation === 180 ? 'portrait' : 'landscape'
    };
  }

  /**
   * Get network information
   * @returns {Object} Network information
   */
  getNetworkInfo() {
    if (this.capabilities.network) {
      const connection = navigator.connection;
      return {
        effectiveType: connection.effectiveType,
        downlink: connection.downlink,
        rtt: connection.rtt,
        saveData: connection.saveData
      };
    }
    return { effectiveType: 'unknown' };
  }

  /**
   * Get battery information
   * @returns {Promise<Object>} Battery information
   */
  async getBatteryInfo() {
    if (this.capabilities.battery) {
      try {
        const battery = await navigator.getBattery();
        return {
          level: battery.level,
          charging: battery.charging,
          chargingTime: battery.chargingTime,
          dischargingTime: battery.dischargingTime
        };
      } catch (error) {
        console.warn('Failed to get battery info:', error);
      }
    }
    return { level: 1, charging: true };
  }

  /**
   * Apply platform-specific optimizations to Three.js renderer
   * @param {THREE.WebGLRenderer} renderer - Three.js renderer
   */
  optimizeRenderer(renderer) {
    const optimizations = this.optimizations;
    
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    renderer.shadowMap.enabled = optimizations.enableShadows;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    
    if (!optimizations.enableAntiAliasing) {
      renderer.antialias = false;
    }
    
    // Set render target size based on scale
    const canvas = renderer.domElement;
    const width = canvas.clientWidth * optimizations.renderScale;
    const height = canvas.clientHeight * optimizations.renderScale;
    renderer.setSize(width, height, false);
  }

  /**
   * Get platform-specific input handling
   * @returns {Object} Input handling configuration
   */
  getInputConfig() {
    const config = {
      enableTouch: this.capabilities.touch,
      enableGamepad: 'getGamepads' in navigator,
      enableKeyboard: true,
      enableMouse: !this.platform.isMobile,
      
      // Touch-specific settings
      touchSensitivity: 1.0,
      enablePinchZoom: this.platform.isMobile,
      enableSwipeGestures: this.platform.isMobile,
      
      // TV-specific settings
      enableRemoteControl: this.platform.isTV,
      enableVoiceControl: this.platform.isTV
    };

    // Platform-specific adjustments
    if (this.platform.type === 'firetv') {
      config.enableRemoteControl = true;
      config.enableVoiceControl = true;
    }

    return config;
  }

  /**
   * Get platform-specific UI configuration
   * @returns {Object} UI configuration
   */
  getUIConfig() {
    const config = {
      // Layout
      layout: this.platform.isTV ? 'tv' : this.platform.isTablet ? 'tablet' : 'mobile',
      
      // Touch targets
      minTouchTarget: this.platform.isMobile ? 44 : 32,
      
      // Typography
      baseFontSize: this.platform.isTV ? '18px' : '16px',
      headingScale: this.platform.isTV ? 1.5 : 1.2,
      
      // Spacing
      spacing: this.platform.isTV ? 'large' : 'normal',
      
      // Colors
      highContrast: this.platform.isTV,
      
      // Animations
      enableAnimations: !this.capabilities.performance.isLowEnd,
      animationDuration: this.capabilities.performance.isLowEnd ? 0.1 : 0.3
    };

    return config;
  }
}

// Export singleton instance
const mobilePlatformService = new MobilePlatformService();

export default mobilePlatformService;
