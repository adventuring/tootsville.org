/**
 * MobilePlatformService.js - Mobile platform detection and optimization service
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides platform detection, capability assessment, and optimization
 * settings for mobile devices, tablets, and smart TV platforms.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
class MobilePlatformService {
  constructor() {
    this.platform = this.detectPlatform();
    this.capabilities = this.detectCapabilities();
    this.optimizations = this.getOptimizations();
  }

  /**
   * Detect the current platform
   * @returns {Object} Platform information
   */
  detectPlatform() {
    const userAgent = navigator.userAgent.toLowerCase();
    const platform = navigator.platform.toLowerCase();
    const maxTouchPoints = navigator.maxTouchPoints || 0;
    const screenWidth = window.innerWidth;
    const screenHeight = window.innerHeight;

    // iOS detection
    if (userAgent.includes('iphone') || userAgent.includes('ipad') || userAgent.includes('ipod')) {
      const isTablet = userAgent.includes('ipad') || 
                      (userAgent.includes('macintosh') && maxTouchPoints > 0) ||
                      (screenWidth >= 768 && screenHeight >= 1024);
      
      return {
        type: 'ios',
        isMobile: !isTablet,
        isTablet: isTablet,
        isTV: false,
        isNative: true
      };
    }

    // Android detection
    if (userAgent.includes('android')) {
      // FireTV detection
      if (userAgent.includes('aftn') || userAgent.includes('aftb')) {
        return {
          type: 'firetv',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        };
      }

      // Samsung TV detection
      if (userAgent.includes('smart-tv') || userAgent.includes('tizen')) {
        return {
          type: 'samsungtv',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        };
      }

      // LG WebOS detection
      if (userAgent.includes('webos')) {
        return {
          type: 'lgwebos',
          isMobile: false,
          isTablet: false,
          isTV: true,
          isNative: true
        };
      }

      // Regular Android
      const isTablet = screenWidth >= 600 && screenHeight >= 960;
      return {
        type: 'android',
        isMobile: !isTablet,
        isTablet: isTablet,
        isTV: false,
        isNative: true
      };
    }

    // Roku detection
    if (userAgent.includes('roku')) {
      return {
        type: 'roku',
        isMobile: false,
        isTablet: false,
        isTV: true,
        isNative: true
      };
    }

    // Desktop detection
    return {
      type: 'desktop',
      isMobile: false,
      isTablet: false,
      isTV: false,
      isNative: false
    };
  }

  /**
   * Detect device capabilities
   * @returns {Object} Capability information
   */
  detectCapabilities() {
    const capabilities = {
      webGL: this.detectWebGLCapabilities(),
      performance: this.detectPerformanceCapabilities(),
      network: this.detectNetworkCapabilities(),
      touch: this.detectTouchCapabilities(),
      orientation: this.detectOrientationCapabilities()
    };

    return capabilities;
  }

  /**
   * Detect WebGL capabilities
   * @returns {Object} WebGL capability information
   */
  detectWebGLCapabilities() {
    try {
      const canvas = document.createElement('canvas');
      const gl = canvas.getContext('webgl2') || canvas.getContext('webgl');
      
      if (!gl) {
        return {
          supported: false,
          version: 'unknown',
          vendor: 'unknown',
          renderer: 'unknown',
          isLowEnd: true
        };
      }

      const vendor = gl.getParameter(gl.VENDOR) || 'unknown';
      const renderer = gl.getParameter(gl.RENDERER) || 'unknown';
      const version = gl.getParameter(gl.VERSION) || 'unknown';
      const maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE) || 0;

      return {
        supported: true,
        version: version,
        vendor: vendor,
        renderer: renderer,
        maxTextureSize: maxTextureSize,
        isLowEnd: maxTextureSize < 2048
      };
    } catch (error) {
      return {
        supported: false,
        version: 'unknown',
        vendor: 'unknown',
        renderer: 'unknown',
        error: error.message,
        isLowEnd: true
      };
    }
  }

  /**
   * Detect performance capabilities
   * @returns {Object} Performance capability information
   */
  detectPerformanceCapabilities() {
    const cores = navigator.hardwareConcurrency || 1;
    const memory = navigator.deviceMemory || 1;
    
    return {
      cores: cores,
      memory: memory,
      isHighEnd: cores >= 8 && memory >= 8,
      isMidRange: cores >= 4 && memory >= 4,
      isLowEnd: cores < 4 || memory < 4
    };
  }

  /**
   * Detect network capabilities
   * @returns {Object} Network capability information
   */
  detectNetworkCapabilities() {
    const connection = navigator.connection || navigator.mozConnection || navigator.webkitConnection;
    
    if (!connection) {
      return {
        type: 'unknown',
        speed: 0,
        latency: 0,
        isFast: false
      };
    }

    const effectiveType = connection.effectiveType || 'unknown';
    const downlink = connection.downlink || 0;
    const rtt = connection.rtt || 0;

    return {
      type: effectiveType,
      speed: downlink,
      latency: rtt,
      isFast: effectiveType === '4g' && downlink >= 10
    };
  }

  /**
   * Detect touch capabilities
   * @returns {Object} Touch capability information
   */
  detectTouchCapabilities() {
    const maxTouchPoints = navigator.maxTouchPoints || 0;
    
    return {
      supported: maxTouchPoints > 0,
      points: maxTouchPoints,
      isMultiTouch: maxTouchPoints > 1
    };
  }

  /**
   * Detect orientation capabilities
   * @returns {Object} Orientation capability information
   */
  detectOrientationCapabilities() {
    const orientation = window.orientation || 0;
    
    return {
      supported: 'orientation' in window,
      current: orientation,
      isLandscape: Math.abs(orientation) === 90
    };
  }

  /**
   * Get platform-specific optimizations
   * @returns {Object} Optimization settings
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
      optimizations.maxFPS = 60;
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
   * Get native interface for platform-specific features
   * @returns {Object} Native interface methods
   */
  getNativeInterface() {
    const platform = this.platform;
    
    return {
      vibrate: platform.isMobile ? this.vibrate.bind(this) : null,
      requestFullscreen: this.requestFullscreen.bind(this),
      lockOrientation: platform.isMobile ? this.lockOrientation.bind(this) : null,
      sendToNative: platform.isNative ? this.sendToNative.bind(this) : null
    };
  }

  /**
   * Get input configuration for the platform
   * @returns {Object} Input configuration
   */
  getInputConfig() {
    const platform = this.platform;
    const capabilities = this.capabilities;
    
    return {
      touch: {
        enabled: capabilities.touch.supported,
        points: capabilities.touch.points,
        isMultiTouch: capabilities.touch.isMultiTouch
      },
      mouse: {
        enabled: !platform.isMobile || platform.isTablet
      },
      keyboard: {
        enabled: !platform.isMobile || platform.isTablet
      },
      remote: {
        enabled: platform.isTV
      }
    };
  }

  /**
   * Get UI configuration for the platform
   * @returns {Object} UI configuration
   */
  getUIConfig() {
    const platform = this.platform;
    const screenWidth = window.innerWidth;
    const screenHeight = window.innerHeight;
    
    let layout = 'desktop';
    let scale = 1.0;
    let fontSize = 'medium';
    
    if (platform.isMobile) {
      layout = 'mobile';
      scale = Math.min(screenWidth / 375, screenHeight / 667);
      fontSize = scale < 0.8 ? 'small' : 'medium';
    } else if (platform.isTablet) {
      layout = 'tablet';
      scale = Math.min(screenWidth / 768, screenHeight / 1024);
      fontSize = scale < 0.8 ? 'small' : 'medium';
    } else if (platform.isTV) {
      layout = 'tv';
      scale = Math.max(screenWidth / 1920, screenHeight / 1080);
      fontSize = scale > 1.5 ? 'xlarge' : 'large';
    }
    
    return {
      layout: layout,
      scale: scale,
      fontSize: fontSize,
      screenWidth: screenWidth,
      screenHeight: screenHeight
    };
  }

  /**
   * Start performance monitoring
   * @returns {Object} Performance monitor
   */
  startPerformanceMonitoring() {
    let frameCount = 0;
    let lastTime = performance.now();
    let fps = 0;
    let memory = 0;
    let isRunning = true;

    const measure = () => {
      if (!isRunning) return;
      
      frameCount++;
      const currentTime = performance.now();
      
      if (currentTime - lastTime >= 1000) {
        fps = Math.round((frameCount * 1000) / (currentTime - lastTime));
        frameCount = 0;
        lastTime = currentTime;
        
        // Get memory usage if available
        if (performance.memory) {
          memory = Math.round(performance.memory.usedJSHeapSize / 1024 / 1024);
        }
      }
      
      requestAnimationFrame(measure);
    };

    requestAnimationFrame(measure);

    return {
      fps: () => fps,
      memory: () => memory,
      stop: () => { isRunning = false; }
    };
  }

  /**
   * Detect performance issues
   * @returns {Object} Performance issues
   */
  detectPerformanceIssues() {
    const capabilities = this.capabilities;
    
    return {
      lowFPS: false, // Would be set by performance monitoring
      highMemory: false, // Would be set by performance monitoring
      networkLatency: capabilities.network.latency > 100,
      lowEndDevice: capabilities.performance.isLowEnd,
      noWebGL: !capabilities.webGL.supported
    };
  }

  // Native interface methods
  vibrate(pattern) {
    if (navigator.vibrate) {
      navigator.vibrate(pattern);
    }
  }

  requestFullscreen() {
    const element = document.documentElement;
    if (element.requestFullscreen) {
      element.requestFullscreen();
    } else if (element.webkitRequestFullscreen) {
      element.webkitRequestFullscreen();
    } else if (element.msRequestFullscreen) {
      element.msRequestFullscreen();
    }
  }

  lockOrientation(orientation) {
    if (screen.orientation && screen.orientation.lock) {
      screen.orientation.lock(orientation);
    }
  }

  sendToNative(data) {
    // Platform-specific native communication
    if (window.ReactNativeWebView) {
      window.ReactNativeWebView.postMessage(JSON.stringify(data));
    }
  }
}

export default MobilePlatformService;
