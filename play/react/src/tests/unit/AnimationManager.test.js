/**
 * AnimationManager.test.js - Unit tests for AnimationManager
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { renderHook, act } from '@testing-library/react';
import { useAnimationManager, ANIMATION_STATES } from '../../components/AnimationManager';

// Mock @react-three/drei's useAnimations
jest.mock('@react-three/drei', () => ({
  useAnimations: jest.fn()
}));

// Mock @react-three/fiber's useFrame
jest.mock('@react-three/fiber', () => ({
  useFrame: jest.fn((callback) => {
    // Simulate frame updates
    setInterval(() => callback({}, 0.016), 16);
  })
}));

describe('AnimationManager', () => {
  let mockActions;
  let mockAvatar;
  let mockCharacter;

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();

    // Mock animation actions
    mockActions = {
      idle: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      walk: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      run: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      jump: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      swim: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      fly: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      },
      sit: {
        play: jest.fn(),
        stop: jest.fn(),
        reset: jest.fn(),
        fadeIn: jest.fn().mockReturnThis(),
        fadeOut: jest.fn(),
        isRunning: jest.fn(() => false),
        setLoop: jest.fn(),
        clampWhenFinished: true,
        enabled: true
      }
    };

    // Mock useAnimations hook
    const { useAnimations } = require('@react-three/drei');
    useAnimations.mockReturnValue({ actions: mockActions });

    // Mock avatar and character data
    mockAvatar = {
      animations: ['idle', 'walk', 'run', 'jump', 'swim', 'fly', 'sit'],
      scene: {}
    };

    mockCharacter = {
      id: 'test-character',
      name: 'TestToot',
      avatar: 'toot',
      world: 'CHOR',
      isMoving: false,
      isRunning: false,
      isJumping: false,
      isSitting: false,
      isUsingItem: false,
      isTalking: false,
      isEmoting: false,
      isFlying: false,
      position: { x: 0, y: 0, z: 0 },
      altitude: 0
    };
  });

  describe('Character Capabilities', () => {
    it('should prevent elephants (Toots) from jumping', () => {
      const elephantCharacter = { 
        ...mockCharacter, 
        name: 'ElephantToot', 
        avatar: 'UltraToot',
        avatarClass: { title: 'UltraToot', filename: 'UltraToot' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, elephantCharacter));

      act(() => {
        const jumpingCharacter = { ...elephantCharacter, isJumping: true };
        result.current.updateAnimation(jumpingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should not play jump animation for elephants
      expect(mockActions.jump.play).not.toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
    });

    it('should allow manatees to swim', () => {
      const manateeCharacter = { 
        ...mockCharacter, 
        name: 'Manatee', 
        avatar: 'manatee', 
        type: 'manatee',
        avatarClass: { title: 'Manatee', filename: 'manatee' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, manateeCharacter));

      act(() => {
        const swimmingCharacter = { ...manateeCharacter, isMoving: true };
        result.current.updateAnimation(swimmingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'OCEAN' });
      });

      // Should play swim animation for manatees in water
      expect(mockActions.swim.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SWIMMING);
    });

    it('should allow birds to fly', () => {
      const birdCharacter = { 
        ...mockCharacter, 
        name: 'Katootel', 
        avatar: 'bird', 
        type: 'bird',
        avatarClass: { title: 'Bird', filename: 'bird' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, birdCharacter));

      act(() => {
        const flyingCharacter = { ...birdCharacter, isFlying: true };
        result.current.updateAnimation(flyingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should play fly animation for birds
      expect(mockActions.fly.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.FLYING);
    });

    it('should allow birds to jump', () => {
      const birdCharacter = { 
        ...mockCharacter, 
        name: 'Katootel', 
        avatar: 'bird', 
        type: 'bird',
        avatarClass: { title: 'Bird', filename: 'bird' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, birdCharacter));

      act(() => {
        const jumpingCharacter = { ...birdCharacter, isJumping: true };
        result.current.updateAnimation(jumpingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should play jump animation for birds
      expect(mockActions.jump.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.JUMPING);
    });

    it('should prevent manatees from walking', () => {
      const manateeCharacter = { 
        ...mockCharacter, 
        name: 'Manatee', 
        avatar: 'manatee', 
        type: 'manatee',
        avatarClass: { title: 'Manatee', filename: 'manatee' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, manateeCharacter));

      act(() => {
        const movingCharacter = { ...manateeCharacter, isMoving: true };
        result.current.updateAnimation(movingCharacter, { x: 1, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should not play walk animation for manatees
      expect(mockActions.walk.play).not.toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
    });

    it('should allow unknown character types to jump by default', () => {
      const unknownCharacter = { 
        ...mockCharacter, 
        name: 'UnknownCharacter', 
        avatar: 'mystery',
        avatarClass: { title: 'Mystery', filename: 'mystery' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, unknownCharacter));

      act(() => {
        const jumpingCharacter = { ...unknownCharacter, isJumping: true };
        result.current.updateAnimation(jumpingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should allow jumping for unknown character types (DEFAULT capabilities)
      expect(mockActions.jump.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.JUMPING);
    });

    it('should not treat non-Toot characters as elephants', () => {
      const nonTootCharacter = { 
        ...mockCharacter, 
        name: 'RegularCharacter', 
        avatar: 'regular',
        avatarClass: { title: 'Regular', filename: 'regular' }
      };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, nonTootCharacter));

      act(() => {
        const jumpingCharacter = { ...nonTootCharacter, isJumping: true };
        result.current.updateAnimation(jumpingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should allow jumping for non-Toot characters (DEFAULT capabilities)
      expect(mockActions.jump.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.JUMPING);
    });
  });

  describe('Water Detection', () => {
    it('should detect swimming in ocean world', () => {
      const tootCharacter = { ...mockCharacter, name: 'SwimmingToot', avatar: 'toot' };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, tootCharacter));

      act(() => {
        const movingCharacter = { ...tootCharacter, isMoving: true };
        result.current.updateAnimation(movingCharacter, { x: 1, y: 0, z: 0 }, 0.016, { name: 'OCEAN' });
      });

      // Should play swim animation in ocean
      expect(mockActions.swim.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SWIMMING);
    });

    it('should detect swimming when altitude is below 0', () => {
      const tootCharacter = { ...mockCharacter, name: 'UnderwaterToot', avatar: 'toot', altitude: -5 };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, tootCharacter));

      act(() => {
        const movingCharacter = { ...tootCharacter, isMoving: true };
        result.current.updateAnimation(movingCharacter, { x: 1, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should play swim animation when underwater
      expect(mockActions.swim.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SWIMMING);
    });
  });

  describe('Basic Animation States', () => {
    it('should start with idle state', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
      expect(mockActions.idle.play).toHaveBeenCalled();
    });

    it('should detect movement and transition to walking', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 0.1, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      expect(mockActions.walk.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.WALKING);
    });

    it('should detect high velocity and transition to running', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 1.0, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      expect(mockActions.run.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.RUNNING);
    });

    it('should prioritize sitting over other states', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const sittingCharacter = { ...mockCharacter, isSitting: true, isMoving: true };
        result.current.updateAnimation(sittingCharacter, { x: 1, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      expect(mockActions.sit.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SITTING);
    });

    it('should prioritize jumping over movement states for capable characters', () => {
      const birdCharacter = { ...mockCharacter, name: 'Bird', avatar: 'bird', type: 'bird' };
      const { result } = renderHook(() => useAnimationManager(mockAvatar, birdCharacter));

      act(() => {
        const jumpingCharacter = { ...birdCharacter, isJumping: true, isMoving: true };
        result.current.updateAnimation(jumpingCharacter, { x: 1, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      expect(mockActions.jump.play).toHaveBeenCalled();
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.JUMPING);
    });

    it('should handle missing animations gracefully', () => {
      const missingAnimations = { ...mockActions };
      delete missingAnimations.walk;
      
      const { useAnimations } = require('@react-three/drei');
      useAnimations.mockReturnValue({ actions: missingAnimations });

      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 0.1, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      // Should not crash and should stay in idle
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.IDLE);
    });
  });

  describe('Animation Transitions', () => {
    it('should handle crossfade transitions', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 0.1, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      // Should fade out idle and fade in walk
      expect(mockActions.idle.fadeOut).toHaveBeenCalled();
      expect(mockActions.walk.fadeIn).toHaveBeenCalled();
    });

    it('should handle transition completion', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 0.1, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      // Wait for transition to complete
      act(() => {
        jest.advanceTimersByTime(200);
      });

      const animationState = result.current.getAnimationState();
      expect(animationState.isTransitioning).toBe(false);
    });
  });

  describe('Velocity Calculation', () => {
    it('should calculate movement velocity correctly', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const movedPosition = { x: 0.5, y: 0, z: 0 };
        result.current.updateAnimation(mockCharacter, movedPosition, 0.016, { name: 'CHOR' });
      });

      const animationState = result.current.getAnimationState();
      expect(animationState.movementVelocity.length()).toBeGreaterThan(0);
    });

    it('should smooth velocity over time', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      // Multiple movement updates
      act(() => {
        result.current.updateAnimation(mockCharacter, { x: 0.1, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
        result.current.updateAnimation(mockCharacter, { x: 0.2, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
        result.current.updateAnimation(mockCharacter, { x: 0.3, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      const animationState = result.current.getAnimationState();
      expect(animationState.movementVelocity.length()).toBeGreaterThan(0);
    });
  });

  describe('State Persistence', () => {
    it('should maintain state between updates', () => {
      const { result } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      act(() => {
        const sittingCharacter = { ...mockCharacter, isSitting: true };
        result.current.updateAnimation(sittingCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SITTING);

      act(() => {
        result.current.updateAnimation(mockCharacter, { x: 0, y: 0, z: 0 }, 0.016, { name: 'CHOR' });
      });

      // Should maintain sitting state
      expect(result.current.getCurrentState()).toBe(ANIMATION_STATES.SITTING);
    });
  });

  describe('Cleanup', () => {
    it('should clean up animations on unmount', () => {
      const { unmount } = renderHook(() => useAnimationManager(mockAvatar, mockCharacter));

      unmount();

      // Should stop all animations
      Object.values(mockActions).forEach(action => {
        expect(action.stop).toHaveBeenCalled();
      });
    });
  });
});
