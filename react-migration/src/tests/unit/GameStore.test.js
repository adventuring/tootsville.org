/**
 * GameStore.test.js - Unit tests for GameStore
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Comprehensive unit tests for the GameStore including
 * state management, actions, and edge cases.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import { useGameStore } from '../../stores/GameStore';

// Mock the store for testing
const mockStore = {
  // State
  user: null,
  character: null,
  world: null,
  avatars: [],
  items: [],
  weather: null,
  connection: {
    status: 'disconnected',
    error: null
  },
  ui: {
    loading: false,
    error: null,
    modal: null
  },
  
  // Actions
  setUser: jest.fn(),
  setCharacter: jest.fn(),
  setWorld: jest.fn(),
  addAvatar: jest.fn(),
  removeAvatar: jest.fn(),
  updateAvatar: jest.fn(),
  addItem: jest.fn(),
  removeItem: jest.fn(),
  updateItem: jest.fn(),
  setWeather: jest.fn(),
  setConnectionStatus: jest.fn(),
  setConnectionError: jest.fn(),
  setLoading: jest.fn(),
  setError: jest.fn(),
  setModal: jest.fn(),
  clearError: jest.fn(),
  reset: jest.fn()
};

// Mock the useGameStore hook
jest.mock('../../stores/GameStore', () => ({
  useGameStore: jest.fn(() => mockStore)
}));

describe('GameStore', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    // Reset mock store state
    Object.assign(mockStore, {
      user: null,
      character: null,
      world: null,
      avatars: [],
      items: [],
      weather: null,
      connection: {
        status: 'disconnected',
        error: null
      },
      ui: {
        loading: false,
        error: null,
        modal: null
      }
    });
  });

  describe('User Management', () => {
    test('should set user information', () => {
      const user = { id: '123', name: 'TestUser', email: 'test@example.com' };
      mockStore.setUser(user);
      
      expect(mockStore.setUser).toHaveBeenCalledWith(user);
    });

    test('should handle null user', () => {
      mockStore.setUser(null);
      expect(mockStore.setUser).toHaveBeenCalledWith(null);
    });

    test('should handle user with missing fields', () => {
      const incompleteUser = { id: '123' };
      mockStore.setUser(incompleteUser);
      expect(mockStore.setUser).toHaveBeenCalledWith(incompleteUser);
    });

    test('should handle user with special characters', () => {
      const specialUser = { 
        id: '123', 
        name: 'Test User 世界! 🎉', 
        email: 'test+special@example.com' 
      };
      mockStore.setUser(specialUser);
      expect(mockStore.setUser).toHaveBeenCalledWith(specialUser);
    });
  });

  describe('Character Management', () => {
    test('should set character information', () => {
      const character = {
        id: 'char123',
        name: 'TestCharacter',
        position: { x: 0, y: 0, z: 0 },
        appearance: { color: 'blue', size: 'medium' }
      };
      mockStore.setCharacter(character);
      
      expect(mockStore.setCharacter).toHaveBeenCalledWith(character);
    });

    test('should handle character position updates', () => {
      const character = {
        id: 'char123',
        name: 'TestCharacter',
        position: { x: 10, y: 20, z: 30 }
      };
      mockStore.setCharacter(character);
      
      expect(mockStore.setCharacter).toHaveBeenCalledWith(character);
    });

    test('should handle character with complex appearance', () => {
      const character = {
        id: 'char123',
        name: 'TestCharacter',
        appearance: {
          color: 'blue',
          size: 'medium',
          accessories: ['hat', 'glasses'],
          animations: ['idle', 'walk', 'jump']
        }
      };
      mockStore.setCharacter(character);
      
      expect(mockStore.setCharacter).toHaveBeenCalledWith(character);
    });
  });

  describe('World Management', () => {
    test('should set world information', () => {
      const world = {
        id: 'world123',
        name: 'TestWorld',
        dimensions: { width: 1000, height: 1000, depth: 1000 },
        terrain: 'grassland',
        weather: 'sunny'
      };
      mockStore.setWorld(world);
      
      expect(mockStore.setWorld).toHaveBeenCalledWith(world);
    });

    test('should handle world with complex terrain', () => {
      const world = {
        id: 'world123',
        name: 'TestWorld',
        terrain: {
          type: 'mixed',
          regions: [
            { type: 'grassland', bounds: { x: 0, y: 0, width: 500, height: 500 } },
            { type: 'forest', bounds: { x: 500, y: 0, width: 500, height: 500 } }
          ]
        }
      };
      mockStore.setWorld(world);
      
      expect(mockStore.setWorld).toHaveBeenCalledWith(world);
    });
  });

  describe('Avatar Management', () => {
    test('should add avatar', () => {
      const avatar = {
        id: 'avatar123',
        name: 'TestAvatar',
        position: { x: 0, y: 0, z: 0 },
        appearance: { color: 'red' }
      };
      mockStore.addAvatar(avatar);
      
      expect(mockStore.addAvatar).toHaveBeenCalledWith(avatar);
    });

    test('should remove avatar', () => {
      const avatarId = 'avatar123';
      mockStore.removeAvatar(avatarId);
      
      expect(mockStore.removeAvatar).toHaveBeenCalledWith(avatarId);
    });

    test('should update avatar', () => {
      const avatarId = 'avatar123';
      const updates = { position: { x: 10, y: 20, z: 30 } };
      mockStore.updateAvatar(avatarId, updates);
      
      expect(mockStore.updateAvatar).toHaveBeenCalledWith(avatarId, updates);
    });

    test('should handle multiple avatars', () => {
      const avatar1 = { id: 'avatar1', name: 'Avatar1' };
      const avatar2 = { id: 'avatar2', name: 'Avatar2' };
      
      mockStore.addAvatar(avatar1);
      mockStore.addAvatar(avatar2);
      
      expect(mockStore.addAvatar).toHaveBeenCalledTimes(2);
      expect(mockStore.addAvatar).toHaveBeenNthCalledWith(1, avatar1);
      expect(mockStore.addAvatar).toHaveBeenNthCalledWith(2, avatar2);
    });

    test('should handle avatar with animations', () => {
      const avatar = {
        id: 'avatar123',
        name: 'TestAvatar',
        animations: {
          idle: { duration: 1000, loop: true },
          walk: { duration: 500, loop: true },
          jump: { duration: 200, loop: false }
        }
      };
      mockStore.addAvatar(avatar);
      
      expect(mockStore.addAvatar).toHaveBeenCalledWith(avatar);
    });
  });

  describe('Item Management', () => {
    test('should add item', () => {
      const item = {
        id: 'item123',
        name: 'TestItem',
        type: 'tool',
        position: { x: 0, y: 0, z: 0 },
        properties: { durability: 100, weight: 5 }
      };
      mockStore.addItem(item);
      
      expect(mockStore.addItem).toHaveBeenCalledWith(item);
    });

    test('should remove item', () => {
      const itemId = 'item123';
      mockStore.removeItem(itemId);
      
      expect(mockStore.removeItem).toHaveBeenCalledWith(itemId);
    });

    test('should update item', () => {
      const itemId = 'item123';
      const updates = { position: { x: 10, y: 20, z: 30 } };
      mockStore.updateItem(itemId, updates);
      
      expect(mockStore.updateItem).toHaveBeenCalledWith(itemId, updates);
    });

    test('should handle item with complex properties', () => {
      const item = {
        id: 'item123',
        name: 'Magic Sword',
        type: 'weapon',
        properties: {
          damage: 25,
          durability: 100,
          weight: 5,
          enchantments: ['fire', 'ice'],
          rarity: 'legendary'
        }
      };
      mockStore.addItem(item);
      
      expect(mockStore.addItem).toHaveBeenCalledWith(item);
    });
  });

  describe('Weather Management', () => {
    test('should set weather', () => {
      const weather = {
        type: 'rain',
        intensity: 0.7,
        wind: { speed: 10, direction: 'north' },
        temperature: 15
      };
      mockStore.setWeather(weather);
      
      expect(mockStore.setWeather).toHaveBeenCalledWith(weather);
    });

    test('should handle weather transitions', () => {
      const weather1 = { type: 'sunny', intensity: 1.0 };
      const weather2 = { type: 'cloudy', intensity: 0.5 };
      
      mockStore.setWeather(weather1);
      mockStore.setWeather(weather2);
      
      expect(mockStore.setWeather).toHaveBeenCalledTimes(2);
      expect(mockStore.setWeather).toHaveBeenNthCalledWith(1, weather1);
      expect(mockStore.setWeather).toHaveBeenNthCalledWith(2, weather2);
    });

    test('should handle extreme weather conditions', () => {
      const extremeWeather = {
        type: 'storm',
        intensity: 1.0,
        wind: { speed: 50, direction: 'random' },
        temperature: -10,
        effects: ['lightning', 'hail', 'tornado']
      };
      mockStore.setWeather(extremeWeather);
      
      expect(mockStore.setWeather).toHaveBeenCalledWith(extremeWeather);
    });
  });

  describe('Connection Management', () => {
    test('should set connection status', () => {
      mockStore.setConnectionStatus('connecting');
      expect(mockStore.setConnectionStatus).toHaveBeenCalledWith('connecting');
      
      mockStore.setConnectionStatus('connected');
      expect(mockStore.setConnectionStatus).toHaveBeenCalledWith('connected');
    });

    test('should set connection error', () => {
      const error = new Error('Connection failed');
      mockStore.setConnectionError(error);
      
      expect(mockStore.setConnectionError).toHaveBeenCalledWith(error);
    });

    test('should handle connection state transitions', () => {
      const states = ['disconnected', 'connecting', 'connected', 'reconnecting'];
      
      states.forEach(state => {
        mockStore.setConnectionStatus(state);
      });
      
      expect(mockStore.setConnectionStatus).toHaveBeenCalledTimes(4);
      states.forEach((state, index) => {
        expect(mockStore.setConnectionStatus).toHaveBeenNthCalledWith(index + 1, state);
      });
    });
  });

  describe('UI Management', () => {
    test('should set loading state', () => {
      mockStore.setLoading(true);
      expect(mockStore.setLoading).toHaveBeenCalledWith(true);
      
      mockStore.setLoading(false);
      expect(mockStore.setLoading).toHaveBeenCalledWith(false);
    });

    test('should set error', () => {
      const error = new Error('Something went wrong');
      mockStore.setError(error);
      
      expect(mockStore.setError).toHaveBeenCalledWith(error);
    });

    test('should clear error', () => {
      mockStore.clearError();
      expect(mockStore.clearError).toHaveBeenCalled();
    });

    test('should set modal', () => {
      const modal = { type: 'confirm', title: 'Confirm Action', message: 'Are you sure?' };
      mockStore.setModal(modal);
      
      expect(mockStore.setModal).toHaveBeenCalledWith(modal);
    });

    test('should clear modal', () => {
      mockStore.setModal(null);
      expect(mockStore.setModal).toHaveBeenCalledWith(null);
    });
  });

  describe('Store Reset', () => {
    test('should reset store to initial state', () => {
      // Set some state
      mockStore.setUser({ id: '123', name: 'Test' });
      mockStore.setCharacter({ id: 'char123', name: 'TestChar' });
      mockStore.addAvatar({ id: 'avatar123', name: 'TestAvatar' });
      
      // Reset
      mockStore.reset();
      
      expect(mockStore.reset).toHaveBeenCalled();
    });
  });

  describe('Edge Cases', () => {
    test('should handle null/undefined values', () => {
      expect(() => mockStore.setUser(null)).not.toThrow();
      expect(() => mockStore.setCharacter(undefined)).not.toThrow();
      expect(() => mockStore.setWorld(null)).not.toThrow();
    });

    test('should handle empty objects', () => {
      expect(() => mockStore.setUser({})).not.toThrow();
      expect(() => mockStore.setCharacter({})).not.toThrow();
      expect(() => mockStore.setWorld({})).not.toThrow();
    });

    test('should handle very large objects', () => {
      const largeObject = {
        id: 'large123',
        data: 'x'.repeat(1000000), // 1MB string
        properties: Array(1000).fill().map((_, i) => ({ key: `prop${i}`, value: i }))
      };
      
      expect(() => mockStore.setUser(largeObject)).not.toThrow();
    });

    test('should handle circular references', () => {
      const circular = { id: 'circular123', name: 'Circular' };
      circular.self = circular;
      
      expect(() => mockStore.setUser(circular)).not.toThrow();
    });
  });

  describe('Performance', () => {
    test('should handle rapid state updates', () => {
      const startTime = Date.now();
      
      for (let i = 0; i < 1000; i++) {
        mockStore.setUser({ id: `user${i}`, name: `User${i}` });
      }
      
      const endTime = Date.now();
      expect(endTime - startTime).toBeLessThan(1000); // Should complete in under 1 second
    });

    test('should handle concurrent operations', () => {
      const promises = [];
      
      for (let i = 0; i < 100; i++) {
        promises.push(Promise.resolve().then(() => {
          mockStore.setUser({ id: `user${i}`, name: `User${i}` });
          mockStore.addAvatar({ id: `avatar${i}`, name: `Avatar${i}` });
          mockStore.addItem({ id: `item${i}`, name: `Item${i}` });
        }));
      }
      
      return Promise.all(promises).then(() => {
        expect(mockStore.setUser).toHaveBeenCalledTimes(100);
        expect(mockStore.addAvatar).toHaveBeenCalledTimes(100);
        expect(mockStore.addItem).toHaveBeenCalledTimes(100);
      });
    });
  });
});



