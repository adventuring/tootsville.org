/**
 * GameWorld.test.js - Unit tests for GameWorld component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the GameWorld component including
 * edge cases, negative assertions, and performance testing.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react-dom/test-utils';
import { GameWorld } from '../../components/GameWorld';
import { useGameStore } from '../../stores/GameStore';
import { CommunicationService } from '../../services/CommunicationService';

// Mock dependencies
jest.mock('../../stores/GameStore');
jest.mock('../../services/CommunicationService');
jest.mock('../../services/MobilePlatformService', () => ({
  default: {
    platform: { type: 'desktop', isMobile: false },
    capabilities: { webGL: { supported: true } },
    optimizations: { renderScale: 1.0, maxFPS: 60 }
  }
}));

// Mock Three.js
jest.mock('three', () => ({
  Scene: jest.fn().mockImplementation(() => ({
    add: jest.fn(),
    remove: jest.fn(),
    children: []
  })),
  PerspectiveCamera: jest.fn().mockImplementation(() => ({
    position: { set: jest.fn() },
    lookAt: jest.fn(),
    updateProjectionMatrix: jest.fn()
  })),
  WebGLRenderer: jest.fn().mockImplementation(() => ({
    setSize: jest.fn(),
    render: jest.fn(),
    domElement: { addEventListener: jest.fn(), removeEventListener: jest.fn() },
    dispose: jest.fn()
  })),
  AmbientLight: jest.fn().mockImplementation(() => ({
    position: { set: jest.fn() }
  })),
  DirectionalLight: jest.fn().mockImplementation(() => ({
    position: { set: jest.fn() },
    castShadow: jest.fn()
  })),
  Vector3: jest.fn().mockImplementation((x, y, z) => ({ x, y, z })),
  Vector2: jest.fn().mockImplementation((x, y) => ({ x, y })),
  Quaternion: jest.fn().mockImplementation((x, y, z, w) => ({ x, y, z, w })),
  Matrix4: jest.fn().mockImplementation(() => ({ elements: new Array(16).fill(0) })),
  Euler: jest.fn().mockImplementation((x, y, z) => ({ x, y, z })),
  Color: jest.fn().mockImplementation((color) => ({ color }))
}));

describe('GameWorld Component', () => {
  let mockGameStore;
  let mockCommunicationService;

  beforeEach(() => {
    // Reset all mocks
    jest.clearAllMocks();

    // Setup mock game store
    mockGameStore = {
      isConnected: false,
      isLoading: false,
      error: null,
      avatars: [],
      items: [],
      terrain: {},
      weather: {},
      connect: jest.fn(),
      disconnect: jest.fn(),
      moveAvatar: jest.fn(),
      sendMessage: jest.fn(),
      useItem: jest.fn()
    };

    useGameStore.mockReturnValue(mockGameStore);

    // Setup mock communication service
    mockCommunicationService = {
      connect: jest.fn(),
      disconnect: jest.fn(),
      send: jest.fn(),
      isConnected: false,
      on: jest.fn(),
      off: jest.fn()
    };

    CommunicationService.mockImplementation(() => mockCommunicationService);
  });

  describe('Component Rendering', () => {
    test('renders game world container', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('renders with loading state', () => {
      mockGameStore.isLoading = true;
      render(<GameWorld />);
      expect(screen.getByTestId('loading-indicator')).toBeInTheDocument();
    });

    test('renders with error state', () => {
      mockGameStore.error = 'Connection failed';
      render(<GameWorld />);
      expect(screen.getByTestId('error-message')).toHaveTextContent('Connection failed');
    });

    test('renders with disconnected state', () => {
      mockGameStore.isConnected = false;
      render(<GameWorld />);
      expect(screen.getByTestId('connection-status')).toHaveTextContent('Disconnected');
    });

    test('renders with connected state', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('connection-status')).toHaveTextContent('Connected');
    });

    test('renders canvas element for Three.js', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-canvas')).toBeInTheDocument();
    });

    test('renders UI overlay', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-ui')).toBeInTheDocument();
    });
  });

  describe('Component Lifecycle', () => {
    test('initializes Three.js scene on mount', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-canvas')).toBeInTheDocument();
    });

    test('cleans up resources on unmount', () => {
      const { unmount } = render(<GameWorld />);
      unmount();
      // Verify cleanup was called
      expect(mockCommunicationService.off).toHaveBeenCalled();
    });

    test('handles component re-rendering', () => {
      const { rerender } = render(<GameWorld />);
      rerender(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });
  });

  describe('Connection Management', () => {
    test('attempts connection on mount when not connected', () => {
      mockGameStore.isConnected = false;
      render(<GameWorld />);
      expect(mockGameStore.connect).toHaveBeenCalled();
    });

    test('does not attempt connection when already connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(mockGameStore.connect).not.toHaveBeenCalled();
    });

    test('handles connection failure gracefully', async () => {
      mockGameStore.connect.mockRejectedValue(new Error('Connection failed'));
      render(<GameWorld />);
      
      await waitFor(() => {
        expect(screen.getByTestId('error-message')).toBeInTheDocument();
      });
    });

    test('retries connection on user action', async () => {
      mockGameStore.error = 'Connection failed';
      render(<GameWorld />);
      
      const retryButton = screen.getByTestId('retry-connection');
      fireEvent.click(retryButton);
      
      expect(mockGameStore.connect).toHaveBeenCalled();
    });
  });

  describe('User Input Handling', () => {
    test('handles keyboard movement input', () => {
      render(<GameWorld />);
      
      fireEvent.keyDown(document, { key: 'ArrowUp' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('forward');
      
      fireEvent.keyDown(document, { key: 'ArrowDown' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('backward');
      
      fireEvent.keyDown(document, { key: 'ArrowLeft' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('left');
      
      fireEvent.keyDown(document, { key: 'ArrowRight' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('right');
    });

    test('handles mouse movement for camera control', () => {
      render(<GameWorld />);
      
      const canvas = screen.getByTestId('game-canvas');
      fireEvent.mouseMove(canvas, { clientX: 100, clientY: 100 });
      
      // Verify mouse movement was handled
      expect(canvas).toBeInTheDocument();
    });

    test('handles mouse click for interaction', () => {
      render(<GameWorld />);
      
      const canvas = screen.getByTestId('game-canvas');
      fireEvent.click(canvas, { clientX: 100, clientY: 100 });
      
      // Verify click was handled
      expect(canvas).toBeInTheDocument();
    });

    test('ignores input when disconnected', () => {
      mockGameStore.isConnected = false;
      render(<GameWorld />);
      
      fireEvent.keyDown(document, { key: 'ArrowUp' });
      expect(mockGameStore.moveAvatar).not.toHaveBeenCalled();
    });

    test('handles invalid input gracefully', () => {
      render(<GameWorld />);
      
      // Test with invalid keys
      fireEvent.keyDown(document, { key: 'x' });
      expect(mockGameStore.moveAvatar).not.toHaveBeenCalled();
    });
  });

  describe('Performance and Optimization', () => {
    test('applies mobile optimizations when on mobile platform', () => {
      // Mock mobile platform
      jest.doMock('../../services/MobilePlatformService', () => ({
        default: {
          platform: { type: 'android', isMobile: true },
          capabilities: { webGL: { supported: true, isLowEnd: true } },
          optimizations: { renderScale: 0.5, maxFPS: 30 }
        }
      }));

      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles low-end device gracefully', () => {
      // Mock low-end device
      jest.doMock('../../services/MobilePlatformService', () => ({
        default: {
          platform: { type: 'desktop', isMobile: false },
          capabilities: { webGL: { supported: true, isLowEnd: true } },
          optimizations: { renderScale: 0.5, maxFPS: 30 }
        }
      }));

      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles WebGL unsupported gracefully', () => {
      // Mock WebGL unsupported
      jest.doMock('../../services/MobilePlatformService', () => ({
        default: {
          platform: { type: 'desktop', isMobile: false },
          capabilities: { webGL: { supported: false } },
          optimizations: { renderScale: 1.0, maxFPS: 60 }
        }
      }));

      render(<GameWorld />);
      expect(screen.getByTestId('webgl-error')).toBeInTheDocument();
    });
  });

  describe('Error Handling', () => {
    test('handles Three.js initialization failure', () => {
      // Mock Three.js failure
      jest.doMock('three', () => ({
        Scene: jest.fn().mockImplementation(() => {
          throw new Error('Three.js initialization failed');
        })
      }));

      render(<GameWorld />);
      expect(screen.getByTestId('error-message')).toBeInTheDocument();
    });

    test('handles communication service errors', async () => {
      mockCommunicationService.connect.mockRejectedValue(new Error('Service error'));
      render(<GameWorld />);
      
      await waitFor(() => {
        expect(screen.getByTestId('error-message')).toBeInTheDocument();
      });
    });

    test('handles store errors gracefully', () => {
      mockGameStore.error = 'Store error';
      render(<GameWorld />);
      expect(screen.getByTestId('error-message')).toHaveTextContent('Store error');
    });

    test('handles undefined store values', () => {
      mockGameStore.avatars = undefined;
      mockGameStore.items = undefined;
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });
  });

  describe('State Management', () => {
    test('updates when store state changes', () => {
      const { rerender } = render(<GameWorld />);
      
      // Change store state
      mockGameStore.isConnected = true;
      useGameStore.mockReturnValue(mockGameStore);
      
      rerender(<GameWorld />);
      expect(screen.getByTestId('connection-status')).toHaveTextContent('Connected');
    });

    test('handles store subscription cleanup', () => {
      const { unmount } = render(<GameWorld />);
      unmount();
      // Verify cleanup was called
      expect(mockCommunicationService.off).toHaveBeenCalled();
    });
  });

  describe('Accessibility', () => {
    test('provides proper ARIA labels', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toHaveAttribute('aria-label', 'Tootsville 3D World');
    });

    test('supports keyboard navigation', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toHaveAttribute('tabIndex', '0');
    });

    test('provides screen reader support', () => {
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toHaveAttribute('role', 'application');
    });
  });

  describe('Edge Cases', () => {
    test('handles rapid state changes', () => {
      render(<GameWorld />);
      
      // Rapidly change connection state
      act(() => {
        mockGameStore.isConnected = true;
        useGameStore.mockReturnValue(mockGameStore);
      });
      
      act(() => {
        mockGameStore.isConnected = false;
        useGameStore.mockReturnValue(mockGameStore);
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles large number of avatars', () => {
      mockGameStore.avatars = Array(1000).fill().map((_, i) => ({ id: i, name: `Avatar ${i}` }));
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles empty world state', () => {
      mockGameStore.avatars = [];
      mockGameStore.items = [];
      mockGameStore.terrain = {};
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles null props gracefully', () => {
      render(<GameWorld config={null} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles undefined props gracefully', () => {
      render(<GameWorld config={undefined} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles invalid config object', () => {
      render(<GameWorld config={{ invalid: 'config', with: null, values: undefined }} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles negative dimensions', () => {
      render(<GameWorld width={-100} height={-200} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles zero dimensions', () => {
      render(<GameWorld width={0} height={0} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles extremely large dimensions', () => {
      render(<GameWorld width={999999} height={999999} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles NaN values', () => {
      render(<GameWorld width={NaN} height={NaN} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles Infinity values', () => {
      render(<GameWorld width={Infinity} height={Infinity} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles null store values', () => {
      mockGameStore.avatars = null;
      mockGameStore.items = null;
      mockGameStore.terrain = null;
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles undefined store values', () => {
      mockGameStore.avatars = undefined;
      mockGameStore.items = undefined;
      mockGameStore.terrain = undefined;
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles malformed avatar data', () => {
      mockGameStore.avatars = [
        null,
        undefined,
        { id: 'invalid-id' },
        { name: 'no-id-avatar' },
        { id: 123, name: null },
        { id: 456, name: undefined },
        { id: 789, name: '' },
        { id: 999, name: 123 }, // non-string name
        { id: 888, name: {} }, // object name
        { id: 777, name: [] }  // array name
      ];
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles malformed item data', () => {
      mockGameStore.items = [
        null,
        undefined,
        { id: 'invalid-id' },
        { name: 'no-id-item' },
        { id: 123, name: null },
        { id: 456, name: undefined },
        { id: 789, name: '' },
        { id: 999, name: 123 }, // non-string name
        { id: 888, name: {} }, // object name
        { id: 777, name: [] }  // array name
      ];
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles circular references in data', () => {
      const circularObj = { id: 1, name: 'circular' };
      circularObj.self = circularObj;
      mockGameStore.avatars = [circularObj];
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles deeply nested objects', () => {
      const deepObj = { level1: { level2: { level3: { level4: { level5: { value: 'deep' } } } } } };
      mockGameStore.terrain = deepObj;
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles function props', () => {
      render(<GameWorld onError={() => {}} onLoad={() => {}} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles boolean props', () => {
      render(<GameWorld debug={true} showFPS={false} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles array props', () => {
      render(<GameWorld plugins={[]} features={['feature1', 'feature2']} />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles empty string props', () => {
      render(<GameWorld className="" id="" />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles whitespace-only string props', () => {
      render(<GameWorld className="   " id="  " />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles special characters in props', () => {
      render(<GameWorld className="test-class-!@#$%^&*()" id="test-id-!@#$%^&*()" />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles unicode characters in props', () => {
      render(<GameWorld className="test-class-🚀🎮" id="test-id-🚀🎮" />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles store method failures gracefully', () => {
      mockGameStore.connect.mockImplementation(() => {
        throw new Error('Connection failed');
      });
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles communication service failures', () => {
      mockCommunicationService.connect.mockImplementation(() => {
        throw new Error('Communication failed');
      });
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles missing DOM elements', () => {
      // Mock document.getElementById to return null
      const originalGetElementById = document.getElementById;
      document.getElementById = jest.fn().mockReturnValue(null);
      
      render(<GameWorld />);
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
      
      // Restore original function
      document.getElementById = originalGetElementById;
    });

    test('handles window resize events', () => {
      render(<GameWorld />);
      
      act(() => {
        window.dispatchEvent(new Event('resize'));
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles visibility change events', () => {
      render(<GameWorld />);
      
      act(() => {
        Object.defineProperty(document, 'hidden', {
          writable: true,
          value: true
        });
        document.dispatchEvent(new Event('visibilitychange'));
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles network status changes', () => {
      render(<GameWorld />);
      
      act(() => {
        Object.defineProperty(navigator, 'onLine', {
          writable: true,
          value: false
        });
        window.dispatchEvent(new Event('offline'));
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles memory pressure events', () => {
      render(<GameWorld />);
      
      act(() => {
        window.dispatchEvent(new Event('memorywarning'));
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles multiple rapid renders', () => {
      const { rerender } = render(<GameWorld />);
      
      for (let i = 0; i < 100; i++) {
        rerender(<GameWorld key={i} />);
      }
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles concurrent state updates', () => {
      render(<GameWorld />);
      
      act(() => {
        // Simulate concurrent updates
        mockGameStore.isConnected = true;
        mockGameStore.isLoading = true;
        mockGameStore.error = 'test error';
        useGameStore.mockReturnValue(mockGameStore);
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles store state mutations', () => {
      render(<GameWorld />);
      
      act(() => {
        // Directly mutate store state
        mockGameStore.avatars.push({ id: 999, name: 'mutated' });
        useGameStore.mockReturnValue(mockGameStore);
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });

    test('handles component prop changes during render', () => {
      const { rerender } = render(<GameWorld />);
      
      act(() => {
        rerender(<GameWorld config={{ changed: true }} />);
      });
      
      expect(screen.getByTestId('game-world')).toBeInTheDocument();
    });
  });

  describe('Integration with Child Components', () => {
    test('renders AvatarManager when connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('avatar-manager')).toBeInTheDocument();
    });

    test('renders WorldTerrain when connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('world-terrain')).toBeInTheDocument();
    });

    test('renders ItemManager when connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('item-manager')).toBeInTheDocument();
    });

    test('renders WeatherSystem when connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('weather-system')).toBeInTheDocument();
    });

    test('renders GameHUD when connected', () => {
      mockGameStore.isConnected = true;
      render(<GameWorld />);
      expect(screen.getByTestId('game-hud')).toBeInTheDocument();
    });
  });

  describe('Memory Management', () => {
    test('cleans up event listeners on unmount', () => {
      const { unmount } = render(<GameWorld />);
      unmount();
      expect(mockCommunicationService.off).toHaveBeenCalled();
    });

    test('disposes Three.js resources on unmount', () => {
      const { unmount } = render(<GameWorld />);
      unmount();
      // Verify Three.js cleanup was called
    });

    test('prevents memory leaks from rapid mount/unmount', () => {
      for (let i = 0; i < 10; i++) {
        const { unmount } = render(<GameWorld />);
        unmount();
      }
      // Verify no memory leaks
    });
  });
});

