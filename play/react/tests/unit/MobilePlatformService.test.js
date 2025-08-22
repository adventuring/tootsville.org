/**
 * MobilePlatformService.test.js - Unit tests for MobilePlatformService
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the MobilePlatformService including
 * platform detection, optimization, and mobile-specific features.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import MobilePlatformService from '../../services/MobilePlatformService';

// Mock navigator and window objects
const mockNavigator = {
  userAgent: '',
  platform: '',
  maxTouchPoints: 0,
  hardwareConcurrency: 4,
  deviceMemory: 4,
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
  orientation: 0,
  screen: {
    width: 1920,
    height: 1080,
    availWidth: 1920,
    availHeight: 1040
  }
};

// Mock WebGL context
const mockWebGLContext = {
  getParameter: jest.fn((param) => {
    switch (param) {
      case 0x1F00: return 'WebKit'; // VENDOR
      case 0x1F02: return 'WebKit WebGL'; // RENDERER
      case 0x8B8C: return '2.0'; // VERSION
      case 0x8B8D: return 16; // MAX_TEXTURE_SIZE
      case 0x8B8E: return 8; // MAX_VIEWPORT_DIMS
      case 0x8B8F: return 16; // MAX_VERTEX_ATTRIBS
      case 0x8B90: return 16; // MAX_VERTEX_UNIFORM_VECTORS
      case 0x8B91: return 16; // MAX_VERTEX_TEXTURE_IMAGE_UNITS
      case 0x8B92: return 16; // MAX_VERTEX_UNIFORM_COMPONENTS
      case 0x8B93: return 16; // MAX_VERTEX_UNIFORM_BLOCKS
      case 0x8B94: return 16; // MAX_VERTEX_OUTPUT_COMPONENTS
      case 0x8B95: return 16; // MAX_VERTEX_UNIFORM_VECTORS
      case 0x8B96: return 16; // MAX_FRAGMENT_UNIFORM_VECTORS
      case 0x8B97: return 16; // MAX_FRAGMENT_UNIFORM_COMPONENTS
      case 0x8B98: return 16; // MAX_FRAGMENT_UNIFORM_BLOCKS
      case 0x8B99: return 16; // MAX_FRAGMENT_INPUT_COMPONENTS
      case 0x8B9A: return 16; // MAX_TEXTURE_IMAGE_UNITS
      case 0x8B9B: return 16; // MAX_COMBINED_TEXTURE_IMAGE_UNITS
      case 0x8B9C: return 16; // MAX_UNIFORM_BUFFER_BINDINGS
      case 0x8B9D: return 16; // MAX_UNIFORM_BLOCK_SIZE
      case 0x8B9E: return 16; // MAX_COMBINED_UNIFORM_BLOCKS
      case 0x8B9F: return 16; // MAX_UNIFORM_LOCATIONS
      case 0x8BA0: return 16; // MAX_UNIFORM_MATRIX_STRIDE
      case 0x8BA1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BA9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAD: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BAF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB0: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BB9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBD: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BBF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC0: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BC9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCD: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BCF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD0: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BD9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDD: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BDF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE0: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BE9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BEA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BEB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BEC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BED: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BEE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BEF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF0: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF1: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF2: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF3: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF4: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF5: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF6: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF7: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF8: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BF9: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFA: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFB: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFC: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFD: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFE: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      case 0x8BFF: return 16; // MAX_UNIFORM_BLOCK_STRIDE
      default: return 16;
    }
  }),
  getExtension: jest.fn(() => ({})),
  createTexture: jest.fn(() => ({})),
  createBuffer: jest.fn(() => ({})),
  createFramebuffer: jest.fn(() => ({})),
  createRenderbuffer: jest.fn(() => ({})),
  createShader: jest.fn(() => ({})),
  createProgram: jest.fn(() => ({})),
  createVertexArray: jest.fn(() => ({})),
  bindTexture: jest.fn(),
  bindBuffer: jest.fn(),
  bindFramebuffer: jest.fn(),
  bindRenderbuffer: jest.fn(),
  bindVertexArray: jest.fn(),
  texImage2D: jest.fn(),
  texParameteri: jest.fn(),
  bufferData: jest.fn(),
  framebufferTexture2D: jest.fn(),
  framebufferRenderbuffer: jest.fn(),
  renderbufferStorage: jest.fn(),
  shaderSource: jest.fn(),
  compileShader: jest.fn(),
  attachShader: jest.fn(),
  linkProgram: jest.fn(),
  useProgram: jest.fn(),
  getAttribLocation: jest.fn(() => 0),
  getUniformLocation: jest.fn(() => ({})),
  enableVertexAttribArray: jest.fn(),
  vertexAttribPointer: jest.fn(),
  uniformMatrix4fv: jest.fn(),
  uniform1i: jest.fn(),
  uniform1f: jest.fn(),
  uniform3f: jest.fn(),
  uniform4f: jest.fn(),
  clearColor: jest.fn(),
  clear: jest.fn(),
  viewport: jest.fn(),
  drawArrays: jest.fn(),
  drawElements: jest.fn(),
  finish: jest.fn(),
  flush: jest.fn()
};

const mockCanvas = {
  getContext: jest.fn(() => mockWebGLContext),
  toDataURL: jest.fn(() => 'data:image/png;base64,test'),
  width: 1920,
  height: 1080
};

// Mock document
const mockDocument = {
  createElement: jest.fn(() => mockCanvas),
  getElementById: jest.fn(() => mockCanvas),
  querySelector: jest.fn(() => mockCanvas),
  addEventListener: jest.fn(),
  removeEventListener: jest.fn()
};

// Mock window
const mockWindow = {
  navigator: mockNavigator,
  screen: mockWindow.screen,
  innerWidth: mockWindow.innerWidth,
  innerHeight: mockWindow.innerHeight,
  devicePixelRatio: mockWindow.devicePixelRatio,
  orientation: mockWindow.orientation,
  addEventListener: jest.fn(),
  removeEventListener: jest.fn(),
  requestAnimationFrame: jest.fn((cb) => setTimeout(cb, 16)),
  cancelAnimationFrame: jest.fn(),
  matchMedia: jest.fn(() => ({
    matches: false,
    addListener: jest.fn(),
    removeListener: jest.fn()
  }))
};

// Mock global objects
global.navigator = mockNavigator;
global.window = mockWindow;
global.document = mockDocument;

describe('MobilePlatformService', () => {
  let service;

  beforeEach(() => {
    jest.clearAllMocks();
    service = new MobilePlatformService();
  });

  describe('Platform Detection', () => {
    test('should detect desktop platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36';
      mockNavigator.platform = 'Win32';
      mockNavigator.maxTouchPoints = 0;
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('desktop');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(false);
    });

    test('should detect iOS platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15';
      mockNavigator.platform = 'iPhone';
      mockNavigator.maxTouchPoints = 5;
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('ios');
      expect(platform.isMobile).toBe(true);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(false);
    });

    test('should detect Android platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Linux; Android 10; SM-G973F) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv8l';
      mockNavigator.maxTouchPoints = 5;
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('android');
      expect(platform.isMobile).toBe(true);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(false);
    });

    test('should detect FireTV platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Linux; Android 7.1.2; AFTN Build/NS6264) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      mockWindow.innerWidth = 1920;
      mockWindow.innerHeight = 1080;
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('firetv');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(true);
    });

    test('should detect Samsung TV platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 5.5) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('samsungtv');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(true);
    });

    test('should detect LG WebOS platform', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (WebOS; Linux/SmartTV) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('lgwebos');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(true);
    });

    test('should detect Roku platform', () => {
      mockNavigator.userAgent = 'Roku/DVP-9.0 (519.00E04142A)';
      mockNavigator.platform = 'Linux armv7l';
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('roku');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(false);
      expect(platform.isTV).toBe(true);
    });

    test('should detect iPad as tablet', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPad; CPU OS 14_0 like Mac OS X) AppleWebKit/605.1.15';
      mockNavigator.platform = 'iPad';
      mockNavigator.maxTouchPoints = 5;
      mockWindow.innerWidth = 1024;
      mockWindow.innerHeight = 768;
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('ios');
      expect(platform.isMobile).toBe(false);
      expect(platform.isTablet).toBe(true);
      expect(platform.isTV).toBe(false);
    });
  });

  describe('Capability Detection', () => {
    test('should detect WebGL capabilities', () => {
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.webGL.supported).toBe(true);
      expect(capabilities.webGL.version).toBe('2.0');
      expect(capabilities.webGL.vendor).toBe('WebKit');
      expect(capabilities.webGL.renderer).toBe('WebKit WebGL');
    });

    test('should detect performance capabilities', () => {
      mockNavigator.hardwareConcurrency = 8;
      mockNavigator.deviceMemory = 8;
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.performance.cores).toBe(8);
      expect(capabilities.performance.memory).toBe(8);
      expect(capabilities.performance.isHighEnd).toBe(true);
    });

    test('should detect network capabilities', () => {
      mockNavigator.connection.effectiveType = '4g';
      mockNavigator.connection.downlink = 10;
      mockNavigator.connection.rtt = 50;
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.network.type).toBe('4g');
      expect(capabilities.network.speed).toBe(10);
      expect(capabilities.network.latency).toBe(50);
      expect(capabilities.network.isFast).toBe(true);
    });

    test('should detect touch capabilities', () => {
      mockNavigator.maxTouchPoints = 5;
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.touch.supported).toBe(true);
      expect(capabilities.touch.points).toBe(5);
      expect(capabilities.touch.isMultiTouch).toBe(true);
    });

    test('should detect orientation capabilities', () => {
      mockWindow.orientation = 90;
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.orientation.supported).toBe(true);
      expect(capabilities.orientation.current).toBe(90);
      expect(capabilities.orientation.isLandscape).toBe(true);
    });
  });

  describe('Optimization Settings', () => {
    test('should get optimizations for desktop', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36';
      mockNavigator.platform = 'Win32';
      mockNavigator.maxTouchPoints = 0;
      
      const optimizations = service.getOptimizations();
      
      expect(optimizations.renderScale).toBe(1.0);
      expect(optimizations.maxFPS).toBe(60);
      expect(optimizations.enableShadows).toBe(true);
      expect(optimizations.enablePostProcessing).toBe(true);
      expect(optimizations.enableTouchOptimization).toBe(false);
      expect(optimizations.enableTVOptimization).toBe(false);
    });

    test('should get optimizations for mobile', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15';
      mockNavigator.platform = 'iPhone';
      mockNavigator.maxTouchPoints = 5;
      
      const optimizations = service.getOptimizations();
      
      expect(optimizations.enableTouchOptimization).toBe(true);
      expect(optimizations.enableBatteryOptimization).toBe(true);
      expect(optimizations.renderScale).toBeLessThanOrEqual(1.0);
    });

    test('should get optimizations for low-end devices', () => {
      mockNavigator.hardwareConcurrency = 2;
      mockNavigator.deviceMemory = 2;
      
      const optimizations = service.getOptimizations();
      
      expect(optimizations.renderScale).toBe(0.5);
      expect(optimizations.maxFPS).toBe(30);
      expect(optimizations.enableShadows).toBe(false);
      expect(optimizations.enablePostProcessing).toBe(false);
    });

    test('should get optimizations for TV platforms', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 5.5) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      
      const optimizations = service.getOptimizations();
      
      expect(optimizations.enableTVOptimization).toBe(true);
      expect(optimizations.enableRemoteControl).toBe(true);
      expect(optimizations.maxFPS).toBe(60);
    });
  });

  describe('Native Interface', () => {
    test('should get native interface for mobile', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15';
      mockNavigator.platform = 'iPhone';
      
      const nativeInterface = service.getNativeInterface();
      
      expect(nativeInterface.vibrate).toBeDefined();
      expect(nativeInterface.requestFullscreen).toBeDefined();
      expect(nativeInterface.lockOrientation).toBeDefined();
    });

    test('should get native interface for TV', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 5.5) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      
      const nativeInterface = service.getNativeInterface();
      
      expect(nativeInterface.sendToNative).toBeDefined();
      expect(nativeInterface.requestFullscreen).toBeDefined();
    });
  });

  describe('Input Configuration', () => {
    test('should get input config for touch devices', () => {
      mockNavigator.maxTouchPoints = 5;
      
      const inputConfig = service.getInputConfig();
      
      expect(inputConfig.touch.enabled).toBe(true);
      expect(inputConfig.touch.points).toBe(5);
      expect(inputConfig.mouse.enabled).toBe(true);
      expect(inputConfig.keyboard.enabled).toBe(true);
    });

    test('should get input config for TV platforms', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 5.5) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      
      const inputConfig = service.getInputConfig();
      
      expect(inputConfig.remote.enabled).toBe(true);
      expect(inputConfig.touch.enabled).toBe(false);
      expect(inputConfig.mouse.enabled).toBe(false);
    });
  });

  describe('UI Configuration', () => {
    test('should get UI config for mobile', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_0 like Mac OS X) AppleWebKit/605.1.15';
      mockNavigator.platform = 'iPhone';
      mockWindow.innerWidth = 375;
      mockWindow.innerHeight = 667;
      
      const uiConfig = service.getUIConfig();
      
      expect(uiConfig.layout).toBe('mobile');
      expect(uiConfig.scale).toBeLessThan(1.0);
      expect(uiConfig.fontSize).toBe('small');
    });

    test('should get UI config for TV', () => {
      mockNavigator.userAgent = 'Mozilla/5.0 (SMART-TV; Linux; Tizen 5.5) AppleWebKit/537.36';
      mockNavigator.platform = 'Linux armv7l';
      mockWindow.innerWidth = 1920;
      mockWindow.innerHeight = 1080;
      
      const uiConfig = service.getUIConfig();
      
      expect(uiConfig.layout).toBe('tv');
      expect(uiConfig.scale).toBeGreaterThan(1.0);
      expect(uiConfig.fontSize).toBe('large');
    });
  });

  describe('Performance Monitoring', () => {
    test('should monitor frame rate', () => {
      const monitor = service.startPerformanceMonitoring();
      
      expect(monitor.fps).toBeDefined();
      expect(monitor.memory).toBeDefined();
      expect(monitor.stop).toBeDefined();
      
      monitor.stop();
    });

    test('should detect performance issues', () => {
      const issues = service.detectPerformanceIssues();
      
      expect(issues.lowFPS).toBeDefined();
      expect(issues.highMemory).toBeDefined();
      expect(issues.networkLatency).toBeDefined();
    });
  });

  describe('Edge Cases', () => {
    test('should handle missing navigator properties', () => {
      delete mockNavigator.hardwareConcurrency;
      delete mockNavigator.deviceMemory;
      delete mockNavigator.connection;
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.performance.cores).toBe(1);
      expect(capabilities.performance.memory).toBe(1);
      expect(capabilities.network.type).toBe('unknown');
    });

    test('should handle missing WebGL context', () => {
      mockCanvas.getContext.mockReturnValue(null);
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.webGL.supported).toBe(false);
      expect(capabilities.webGL.version).toBe('unknown');
    });

    test('should handle very small screens', () => {
      mockWindow.innerWidth = 240;
      mockWindow.innerHeight = 320;
      
      const uiConfig = service.getUIConfig();
      
      expect(uiConfig.scale).toBeLessThan(0.5);
      expect(uiConfig.fontSize).toBe('tiny');
    });

    test('should handle very large screens', () => {
      mockWindow.innerWidth = 3840;
      mockWindow.innerHeight = 2160;
      
      const uiConfig = service.getUIConfig();
      
      expect(uiConfig.scale).toBeGreaterThan(1.5);
      expect(uiConfig.fontSize).toBe('xlarge');
    });
  });

  describe('Error Handling', () => {
    test('should handle platform detection errors', () => {
      mockNavigator.userAgent = '';
      mockNavigator.platform = '';
      
      const platform = service.detectPlatform();
      
      expect(platform.type).toBe('unknown');
      expect(platform.isMobile).toBe(false);
    });

    test('should handle capability detection errors', () => {
      mockCanvas.getContext.mockImplementation(() => {
        throw new Error('WebGL not supported');
      });
      
      const capabilities = service.detectCapabilities();
      
      expect(capabilities.webGL.supported).toBe(false);
      expect(capabilities.webGL.error).toBeDefined();
    });
  });
});
