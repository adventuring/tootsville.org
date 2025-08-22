/**
 * AnimationManager.test.js - Unit tests for animation state management
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Tests for proper animation state transitions between walking, idle, and other states.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import { renderHook, act } from '@testing-library/react';
import { useAnimationManager, ANIMATION_STATES } from '../../components/AnimationManager';
import { Vector3 } from 'three';

// Mock Three.js animations
const mockActions = {
  idle: {
    play: jest.fn(),
    stop: jest.fn(),
    reset: jest.fn(),
    fadeIn: jest.fn().mockReturnThis(),
    fadeOut: jest.fn().mockReturnThis(),
    isRunning: jest.fn(() => false),
    setLoop: jest.fn(),
    clampWhenFinished: false,
    enabled: true
  },
  walk: {
    play: jest.fn(),
    stop: jest.fn(),
    reset: jest.fn(),
    fadeIn: jest.fn().mockReturnThis(),
    fadeOut: jest.fn().mockReturnThis(),
    isRunning: jest.fn(() => false),
    setLoop: jest.fn(),
    clampWhenFinished: false,
    enabled: true
  },
  run: {
    play: jest.fn(),
    stop: jest.fn(),
    reset: jest.fn(),
    fadeIn: jest.fn().mockReturnThis(),
    fadeOut: jest.fn().mockReturnThis(),
    isRunning: jest.fn(() => false),
    setLoop: jest.fn(),
    clampWhenFinished: false,
    enabled: true
  },
  sit: {
    play: jest.fn(),
    stop: jest.fn(),
    reset: jest.fn(),
    fadeIn: jest.fn().mockReturnThis(),
    fadeOut: jest.fn().mockReturnThis(),
    isRunning: jest.fn(() => false),
    setLoop: jest.fn(),
    clampWhenFinished: false,
    enabled: true
  }
};

// Mock useAnimations hook
jest.mock('@react-three/drei', () => ({
  useAnimations: () => ({ actions: mockActions })
}));

describe('AnimationManager', () => {
  let mockAvatar;
  let mockCharacter;

  beforeEach(() => {
    // Reset all mocks
    jest.clearAllMocks();
    
    mockAvatar = {
      animations: ['idle', 'walk', 'run', 'sit'],
      scene: {}
    };
    
    mockCharacter = {
      id: 'test-character',
      name: 'Test Character',
      position: { x: 0, y: 0, z: 0 },
      isSitting: false,
      isJumping: false,
      isUsingItem: false,
      isTalking: false,
      isEmoting: false
    };
  });

  describe('Initial State', () => {
    it('should initialize with idle state', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
    });

    it('should set up animation actions correctly', () => {
      renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      Object.values(mockActions).forEach(action => {
        expect(action.setLoop).toHaveBeenCalled();
        expect(action.clampWhenFinished).toBe(true);
        expect(action.enabled).toBe(true);
      });
    });
  });

  describe('Movement Detection', () => {
    it('should detect movement when position changes', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      const initialPosition = new Vector3(0, 0, 0);
      const movedPosition = new Vector3(1, 0, 1);
      
      expect(result.current.isMoving(initialPosition)).toBe(false);
      expect(result.current.isMoving(movedPosition)).toBe(true);
    });

    it('should not detect movement for small position changes', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      const initialPosition = new Vector3(0, 0, 0);
      const smallMovePosition = new Vector3(0.005, 0, 0.005); // Below threshold
      
      expect(result.current.isMoving(smallMovePosition)).toBe(false);
    });
  });

  describe('Animation State Transitions', () => {
    it('should transition from idle to walking when moving', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      // Simulate movement
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.WALKING);
      expect(mockActions.walk.play).toHaveBeenCalled();
    });

    it('should transition from walking to idle when stopped', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      // Start with movement
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016);
      });
      
      // Stop movement
      const stoppedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(mockCharacter, stoppedPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
      expect(mockActions.idle.play).toHaveBeenCalled();
    });

    it('should transition to running for high velocity', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      // Simulate high velocity movement
      const fastPosition = new Vector3(10, 0, 10);
      act(() => {
        result.current.updateAnimation(mockCharacter, fastPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.RUNNING);
      expect(mockActions.run.play).toHaveBeenCalled();
    });
  });

  describe('Special Animation States', () => {
    it('should prioritize sitting over movement', () => {
      const sittingCharacter = { ...mockCharacter, isSitting: true };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, sittingCharacter));
      
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(sittingCharacter, movedPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SITTING);
      expect(mockActions.sit.play).toHaveBeenCalled();
    });

    it('should prioritize jumping over other states', () => {
      const jumpingCharacter = { ...mockCharacter, isJumping: true };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, jumpingCharacter));
      
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(jumpingCharacter, movedPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.JUMPING);
    });

    it('should handle item usage animation', () => {
      const usingItemCharacter = { ...mockCharacter, isUsingItem: true };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, usingItemCharacter));
      
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.updateAnimation(usingItemCharacter, movedPosition, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.USING_ITEM);
    });
  });

  describe('Animation Transitions', () => {
    it('should use crossfade for smooth transitions', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      // Trigger a transition
      const movedPosition = new Vector3(1, 0, 1);
      act(() => {
        result.current.playAnimation(ANIMATION_STATES.WALKING, true);
      });
      
      expect(mockActions.idle.fadeOut).toHaveBeenCalled();
      expect(mockActions.walk.fadeIn).toHaveBeenCalled();
    });

    it('should handle missing animations gracefully', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      const consoleSpy = jest.spyOn(console, 'warn').mockImplementation();
      
      act(() => {
        result.current.playAnimation('nonexistent_animation');
      });
      
      expect(consoleSpy).toHaveBeenCalledWith('Animation "nonexistent_animation" not found');
      consoleSpy.mockRestore();
    });
  });

  describe('Velocity Calculation', () => {
    it('should calculate velocity correctly', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      const initialPosition = new Vector3(0, 0, 0);
      const finalPosition = new Vector3(1, 0, 1);
      const deltaTime = 0.016; // 60fps
      
      // First frame
      act(() => {
        result.current.updateAnimation(mockCharacter, initialPosition, deltaTime);
      });
      
      // Second frame with movement
      act(() => {
        result.current.updateAnimation(mockCharacter, finalPosition, deltaTime);
      });
      
      const animationState = result.current.getAnimationState();
      expect(animationState.movementVelocity.length()).toBeGreaterThan(0);
    });
  });

  describe('State Persistence', () => {
    it('should maintain state between updates', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      const position = new Vector3(0, 0, 0);
      
      // Multiple updates
      act(() => {
        result.current.updateAnimation(mockCharacter, position, 0.016);
      });
      
      act(() => {
        result.current.updateAnimation(mockCharacter, position, 0.016);
      });
      
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
    });
  });

  describe('Cleanup', () => {
    it('should clean up animations on unmount', () => {
      const { unmount } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));
      
      unmount();
      
      Object.values(mockActions).forEach(action => {
        expect(action.stop).toHaveBeenCalled();
      });
    });
  });
});
