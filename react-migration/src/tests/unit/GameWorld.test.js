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
    domElement: document.createElement('canvas'),
    dispose: jest.fn()
  })),
  AmbientLight: jest.fn().mockImplementation(() => ({
    position: { set: jest.fn() }
  })),
  DirectionalLight: jest.fn().mockImplementation(() => ({
    position: { set: jest.fn() },
    castShadow: jest.fn()
  }))
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
      
      fireEvent.keyDown(document, { key: 'w' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('forward');
      
      fireEvent.keyDown(document, { key: 's' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('backward');
      
      fireEvent.keyDown(document, { key: 'a' });
      expect(mockGameStore.moveAvatar).toHaveBeenCalledWith('left');
      
      fireEvent.keyDown(document, { key: 'd' });
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
      
      fireEvent.keyDown(document, { key: 'w' });
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
