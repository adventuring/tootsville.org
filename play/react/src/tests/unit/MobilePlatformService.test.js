/**
 * Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.
 */

import MobilePlatformService from '../../services/MobilePlatformService';

// Mock browser APIs
const mockNavigator = {
  userAgent: '',
  platform: '',
  maxTouchPoints: 0,
  hardwareConcurrency: 4,
  deviceMemory: 8,
  connection: {
    effectiveType: '4g',
    downlink: 10,
    rtt: 50
  }
};

const mockWindow = {
  innerWidth: 1920,
  innerHeight: 1080,
  devicePixelRatio: 1,
  orientation: {
    type: 'landscape-primary',
    angle: 0
  },
  addEventListener: jest.fn(),
  removeEventListener: jest.fn()
};

const mockDocument = {
  createElement: jest.fn(() => ({
    getContext: jest.fn(() => ({
      getParameter: jest.fn(() => 'WebGL 2.0'),
      getExtension: jest.fn(() => ({})),
      drawArrays: jest.fn(),
      clear: jest.fn()
    }))
  }))
};

// Mock HTMLCanvasElement
class MockHTMLCanvasElement {
  constructor() {
    this.getContext = jest.fn(() => ({
      getParameter: jest.fn(() => 'WebGL 2.0'),
      getExtension: jest.fn(() => ({})),
      drawArrays: jest.fn(),
      clear: jest.fn()
    }));
  }
}

// Mock WebGLRenderingContext
class MockWebGLRenderingContext {
  constructor() {
    this.getParameter = jest.fn(() => 'WebGL 2.0');
    this.getExtension = jest.fn(() => ({}));
    this.drawArrays = jest.fn();
    this.clear = jest.fn();
  }
}

describe('MobilePlatformService', () => {
  let service;
  let originalNavigator;
  let originalWindow;
  let originalDocument;

  beforeEach(() => {
    // Store original globals
    originalNavigator = global.navigator;
    originalWindow = global.window;
    originalDocument = global.document;

    // Mock globals
    global.navigator = { ...mockNavigator };
    global.window = { ...mockWindow };
    global.document = { ...mockDocument };
    global.HTMLCanvasElement = MockHTMLCanvasElement;
    global.WebGLRenderingContext = MockWebGLRenderingContext;

    service = new MobilePlatformService();
  });

  afterEach(() => {
    // Restore original globals
    global.navigator = originalNavigator;
    global.window = originalWindow;
    global.document = originalDocument;
  });

  describe('Platform Detection', () => {
    test('should detect Android platform', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Linux; Android 10; SM-G975F) AppleWebKit/537.36';

      const platform = service.detectPlatform();

      expect(platform).toBe('android');
    });

    test('should detect iOS platform', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15';

      const platform = service.detectPlatform();

      expect(platform).toBe('ios');
    });

    test('should detect iPad platform', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (iPad; CPU OS 14_0 like Mac OS X) AppleWebKit/605.1.15';

      const platform = service.detectPlatform();

      expect(platform).toBe('ipad');
    });

    test('should detect FireTV platform', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Linux; Android 7.1.2; AFTT) AppleWebKit/537.36';

      const platform = service.detectPlatform();

      expect(platform).toBe('firetv');
    });

    test('should detect Windows platform', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36';

      const platform = service.detectPlatform();

      expect(platform).toBe('windows');
    });

    test('should detect desktop platform as fallback', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36';

      const platform = service.detectPlatform();

      expect(platform).toBe('desktop');
    });
  });

  describe('Capability Detection', () => {
    test('should detect WebGL support', () => {
      const hasWebGL = service.hasWebGL();

      expect(hasWebGL).toBe(true);
    });

    test('should detect touch support', () => {
      global.navigator.maxTouchPoints = 5;

      const hasTouch = service.hasTouchSupport();

      expect(hasTouch).toBe(true);
    });

    test('should detect orientation support', () => {
      const hasOrientation = service.hasOrientationSupport();

      expect(hasOrientation).toBe(true);
    });

    test('should get device performance level', () => {
      global.navigator.hardwareConcurrency = 8;
      global.navigator.deviceMemory = 16;

      const performance = service.getPerformanceLevel();

      expect(performance).toBe('high');
    });

    test('should get network quality', () => {
      global.navigator.connection.effectiveType = '4g';
      global.navigator.connection.downlink = 20;

      const networkQuality = service.getNetworkQuality();

      expect(networkQuality).toBe('excellent');
    });

    test('should get screen characteristics', () => {
      global.window.innerWidth = 1920;
      global.window.innerHeight = 1080;
      global.window.devicePixelRatio = 2;

      const screen = service.getScreenCharacteristics();

      expect(screen.width).toBe(1920);
      expect(screen.height).toBe(1080);
      expect(screen.density).toBe(2);
      expect(screen.orientation).toBe('landscape');
    });
  });

  describe('Optimization Settings', () => {
    test('should get optimized graphics settings for high-end device', () => {
      global.navigator.hardwareConcurrency = 8;
      global.navigator.deviceMemory = 16;
      global.window.devicePixelRatio = 2;

      const settings = service.getOptimizedSettings();

      expect(settings.graphics.quality).toBe('high');
      expect(settings.graphics.shadows).toBe(true);
      expect(settings.graphics.antialiasing).toBe(true);
      expect(settings.performance.targetFPS).toBe(60);
    });

    test('should get optimized graphics settings for low-end device', () => {
      global.navigator.hardwareConcurrency = 2;
      global.navigator.deviceMemory = 2;
      global.window.devicePixelRatio = 1;

      const settings = service.getOptimizedSettings();

      expect(settings.graphics.quality).toBe('low');
      expect(settings.graphics.shadows).toBe(false);
      expect(settings.graphics.antialiasing).toBe(false);
      expect(settings.performance.targetFPS).toBe(30);
    });

    test('should get optimized network settings', () => {
      global.navigator.connection.effectiveType = '3g';
      global.navigator.connection.downlink = 2;

      const settings = service.getOptimizedSettings();

      expect(settings.network.compression).toBe(true);
      expect(settings.network.caching).toBe(true);
      expect(settings.network.batchSize).toBe(5);
    });
  });

  describe('Native Interface', () => {
    test('should detect native app wrapper', () => {
      global.window.ReactNativeWebView = {};

      const isNative = service.isNativeApp();

      expect(isNative).toBe(true);
    });

    test('should detect Capacitor wrapper', () => {
      global.window.Capacitor = {};

      const isCapacitor = service.isCapacitorApp();

      expect(isCapacitor).toBe(true);
    });

    test('should get native capabilities', () => {
      global.window.ReactNativeWebView = {};
      global.navigator.mediaDevices = { getUserMedia: jest.fn() };

      const capabilities = service.getNativeCapabilities();

      expect(capabilities.camera).toBe(true);
      expect(capabilities.microphone).toBe(true);
      expect(capabilities.vibration).toBe(true);
    });
  });

  describe('Input Configuration', () => {
    test('should configure touch input for mobile', () => {
      global.navigator.maxTouchPoints = 5;

      const inputConfig = service.getInputConfiguration();

      expect(inputConfig.primary).toBe('touch');
      expect(inputConfig.gestures).toBe(true);
      expect(inputConfig.virtualJoystick).toBe(true);
    });

    test('should configure keyboard/mouse input for desktop', () => {
      global.navigator.maxTouchPoints = 0;

      const inputConfig = service.getInputConfiguration();

      expect(inputConfig.primary).toBe('keyboard');
      expect(inputConfig.gestures).toBe(false);
      expect(inputConfig.virtualJoystick).toBe(false);
    });

    test('should configure gamepad input for TV platforms', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Linux; Android 7.1.2; AFTT) AppleWebKit/537.36';

      const inputConfig = service.getInputConfiguration();

      expect(inputConfig.primary).toBe('gamepad');
      expect(inputConfig.navigation).toBe('dpad');
    });
  });

  describe('UI Configuration', () => {
    test('should get mobile UI settings', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Linux; Android 10; SM-G975F) AppleWebKit/537.36';

      const uiConfig = service.getUIConfiguration();

      expect(uiConfig.layout).toBe('mobile');
      expect(uiConfig.fontSize).toBe('large');
      expect(uiConfig.buttonSize).toBe('large');
      expect(uiConfig.spacing).toBe('generous');
    });

    test('should get TV UI settings', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (Linux; Android 7.1.2; AFTT) AppleWebKit/537.36';

      const uiConfig = service.getUIConfiguration();

      expect(uiConfig.layout).toBe('tv');
      expect(uiConfig.fontSize).toBe('extra-large');
      expect(uiConfig.buttonSize).toBe('extra-large');
      expect(uiConfig.spacing).toBe('very-generous');
    });

    test('should get desktop UI settings', () => {
      global.navigator.userAgent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36';

      const uiConfig = service.getUIConfiguration();

      expect(uiConfig.layout).toBe('desktop');
      expect(uiConfig.fontSize).toBe('normal');
      expect(uiConfig.buttonSize).toBe('normal');
      expect(uiConfig.spacing).toBe('normal');
    });
  });

  describe('Performance Monitoring', () => {
    test('should start performance monitoring', () => {
      const monitor = service.startPerformanceMonitoring();

      expect(monitor).toBeDefined();
      expect(typeof monitor.getMetrics).toBe('function');
    });

    test('should get performance metrics', () => {
      const monitor = service.startPerformanceMonitoring();

      // Simulate some time passing
      setTimeout(() => {
        const metrics = monitor.getMetrics();

        expect(metrics.fps).toBeDefined();
        expect(metrics.memory).toBeDefined();
        expect(metrics.network).toBeDefined();
      }, 100);
    });

    test('should detect performance issues', () => {
      const monitor = service.startPerformanceMonitoring();

      // Simulate low performance
      monitor.simulateLowPerformance();

      const issues = monitor.getPerformanceIssues();

      expect(issues.length).toBeGreaterThan(0);
      expect(issues[0].type).toBeDefined();
      expect(issues[0].severity).toBeDefined();
    });
  });

  describe('Edge Cases', () => {
    test('should handle missing navigator properties', () => {
      delete global.navigator.hardwareConcurrency;
      delete global.navigator.deviceMemory;

      const performance = service.getPerformanceLevel();

      expect(performance).toBe('unknown');
    });

    test('should handle missing window properties', () => {
      delete global.window.innerWidth;
      delete global.window.innerHeight;

      const screen = service.getScreenCharacteristics();

      expect(screen.width).toBe(0);
      expect(screen.height).toBe(0);
    });

    test('should handle missing connection info', () => {
      delete global.navigator.connection;

      const networkQuality = service.getNetworkQuality();

      expect(networkQuality).toBe('unknown');
    });

    test('should handle WebGL context creation failure', () => {
      global.document.createElement = jest.fn(() => ({
        getContext: jest.fn(() => null)
      }));

      const hasWebGL = service.hasWebGL();

      expect(hasWebGL).toBe(false);
    });
  });

  describe('Error Handling', () => {
    test('should handle platform detection errors gracefully', () => {
      global.navigator.userAgent = null;

      const platform = service.detectPlatform();

      expect(platform).toBe('unknown');
    });

    test('should handle capability detection errors', () => {
      global.navigator = null;

      const hasTouch = service.hasTouchSupport();

      expect(hasTouch).toBe(false);
    });

    test('should handle optimization errors', () => {
      global.window = null;

      const settings = service.getOptimizedSettings();

      expect(settings).toBeDefined();
      expect(settings.graphics.quality).toBe('low');
    });
  });
});
