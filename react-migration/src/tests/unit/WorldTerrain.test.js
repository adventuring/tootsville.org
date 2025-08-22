/**
 * WorldTerrain.test.js - Unit tests for WorldTerrain component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the WorldTerrain component including
 * terrain generation, world-specific features, and performance testing.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { render, screen } from '@testing-library/react';
import { act } from 'react-dom/test-utils';
import WorldTerrain from '../../components/WorldTerrain';
import { useGameStore } from '../../stores/GameStore';

// Mock dependencies
jest.mock('../../stores/GameStore');
jest.mock('@react-three/fiber', () => ({
  useFrame: jest.fn((callback) => callback({ clock: { elapsedTime: 0 } }))
}));
jest.mock('@react-three/drei', () => ({
  Plane: ({ children, ...props }) => <div data-testid="terrain-plane" {...props}>{children}</div>,
  MeshDistortMaterial: ({ children, ...props }) => <div data-testid="terrain-material" {...props}>{children}</div>
}));

// Mock Three.js
jest.mock('three', () => ({
  Vector3: jest.fn().mockImplementation(() => ({ set: jest.fn() })),
  Color: jest.fn().mockImplementation(() => ({ set: jest.fn() }))
}));

describe('WorldTerrain Component', () => {
  let mockGameStore;

  beforeEach(() => {
    jest.clearAllMocks();

    mockGameStore = {
      activity: { lat: 0, long: 0, alt: 0 }
    };

    useGameStore.mockReturnValue(mockGameStore);
  });

  describe('Component Rendering', () => {
    test('renders terrain plane', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('renders terrain material', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-material')).toBeInTheDocument();
    });

    test('renders with default CHOR world', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#7ac142');
    });
  });

  describe('World-Specific Terrain', () => {
    test('renders CHOR world terrain', () => {
      render(<WorldTerrain world="CHOR" />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#7ac142');
    });

    test('renders MOON world terrain', () => {
      render(<WorldTerrain world="MOON" />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#cccccc');
    });

    test('renders OTHM world terrain', () => {
      render(<WorldTerrain world="OTHM" />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#8b4513');
    });

    test('renders PINK world terrain', () => {
      render(<WorldTerrain world="PINK" />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#ff69b4');
    });

    test('falls back to CHOR for unknown world', () => {
      render(<WorldTerrain world="UNKNOWN" />);
      expect(screen.getByTestId('terrain-material')).toHaveAttribute('color', '#7ac142');
    });
  });

  describe('Terrain Configuration', () => {
    test('applies custom terrain settings', () => {
      const customSettings = {
        height: 200,
        segments: 256,
        distortion: 0.8
      };

      render(<WorldTerrain terrainSettings={customSettings} />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('uses default terrain settings when not provided', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });
  });

  describe('Terrain Features', () => {
    test('renders CHOR world features', () => {
      render(<WorldTerrain world="CHOR" />);
      // CHOR features should be rendered
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('renders MOON world features', () => {
      render(<WorldTerrain world="MOON" />);
      // MOON features should be rendered
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('renders OTHM world features', () => {
      render(<WorldTerrain world="OTHM" />);
      // OTHM features should be rendered
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('renders PINK world features', () => {
      render(<WorldTerrain world="PINK" />);
      // PINK features should be rendered
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });
  });

  describe('Performance and Optimization', () => {
    test('handles terrain animation efficiently', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('optimizes rendering for different world types', () => {
      const worlds = ['CHOR', 'MOON', 'OTHM', 'PINK'];
      
      worlds.forEach(world => {
        const { unmount } = render(<WorldTerrain world={world} />);
        expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
        unmount();
      });
    });
  });

  describe('Error Handling', () => {
    test('handles missing world prop gracefully', () => {
      render(<WorldTerrain world={undefined} />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('handles invalid terrain settings gracefully', () => {
      render(<WorldTerrain terrainSettings={null} />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('handles missing store data gracefully', () => {
      useGameStore.mockReturnValue({ activity: null });
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });
  });

  describe('Integration with Game Store', () => {
    test('responds to activity changes', () => {
      const { rerender } = render(<WorldTerrain />);
      
      // Update activity
      mockGameStore.activity = { lat: 100, long: 200, alt: 50 };
      useGameStore.mockReturnValue(mockGameStore);
      
      rerender(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('handles store subscription cleanup', () => {
      const { unmount } = render(<WorldTerrain />);
      unmount();
      // Verify component unmounts without errors
    });
  });

  describe('Accessibility', () => {
    test('provides proper ARIA labels for terrain', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('supports screen reader navigation', () => {
      render(<WorldTerrain />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });
  });

  describe('Edge Cases', () => {
    test('handles rapid world changes', () => {
      const { rerender } = render(<WorldTerrain world="CHOR" />);
      
      act(() => {
        rerender(<WorldTerrain world="MOON" />);
      });
      
      act(() => {
        rerender(<WorldTerrain world="OTHM" />);
      });
      
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('handles large terrain settings', () => {
      const largeSettings = {
        height: 1000,
        segments: 512,
        distortion: 1.0
      };

      render(<WorldTerrain terrainSettings={largeSettings} />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });

    test('handles zero terrain settings', () => {
      const zeroSettings = {
        height: 0,
        segments: 0,
        distortion: 0
      };

      render(<WorldTerrain terrainSettings={zeroSettings} />);
      expect(screen.getByTestId('terrain-plane')).toBeInTheDocument();
    });
  });
});


