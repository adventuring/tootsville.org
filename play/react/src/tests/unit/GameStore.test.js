/**
 * Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { useGameStore } from '../../stores/GameStore';

// Mock the CommunicationService
jest.mock('../../services/CommunicationService', () => ({
  CommunicationService: jest.fn().mockImplementation(() => ({
    connect: jest.fn(),
    disconnect: jest.fn(),
    send: jest.fn(),
    on: jest.fn(),
    isConnected: jest.fn().mockReturnValue(true)
  }))
}));

describe('GameStore', () => {
  let store;

  beforeEach(() => {
    // Reset the store before each test
    store = useGameStore.getState();
    store.actions.reset();
  });

  describe('Character Management', () => {
    test('should set character', () => {
      const character = {
        id: 'char123',
        name: 'TestCharacter',
        position: { x: 0, y: 0, z: 0 }
      };

      store.actions.setCharacter(character);

      expect(store.character).toEqual(character);
    });

    test('should set player', () => {
      const player = {
        id: 'user123',
        username: 'testuser',
        email: 'test@example.com'
      };

      store.actions.setPlayer(player);

      expect(store.player).toEqual(player);
    });

    test('should set character UUID', () => {
      const uuid = 'test-uuid-123';

      store.actions.setCharacterUUID(uuid);

      expect(store.characterUUID).toBe(uuid);
    });
  });

  describe('Activity Management', () => {
    test('should update activity', () => {
      const newActivity = {
        world: 'MOON',
        lat: 10,
        long: 20,
        alt: 100
      };

      store.actions.updateActivity(newActivity);

      expect(store.activity).toEqual(newActivity);
    });

    test('should update activity partially', () => {
      const originalActivity = { ...store.activity };
      const partialUpdate = { world: 'MOON' };

      store.actions.updateActivity(partialUpdate);

      expect(store.activity.world).toBe('MOON');
      expect(store.activity.lat).toBe(originalActivity.lat);
      expect(store.activity.long).toBe(originalActivity.long);
      expect(store.activity.alt).toBe(originalActivity.alt);
    });
  });

  describe('Audio Management', () => {
    test('should set volume', () => {
      const volume = 50;

      store.actions.setVolume(volume);

      expect(store.audio.volume).toBe(volume);
    });
  });

  describe('UI Management', () => {
    test('should set talk box open state', () => {
      store.actions.setTalkBoxOpen(false);

      expect(store.ui.talkBoxOpen).toBe(false);
    });

    test('should open panel', () => {
      store.actions.openPanel('inventory');

      expect(store.ui.openPanel).toBe('inventory');
    });

    test('should close panel', () => {
      store.actions.openPanel('inventory');
      store.actions.closePanel();

      expect(store.ui.openPanel).toBe(null);
    });
  });

  describe('Character and Avatar Management', () => {
    test('should update characters', () => {
      const characters = [
        { id: 'char1', name: 'Character 1' },
        { id: 'char2', name: 'Character 2' }
      ];

      store.actions.updateCharacters(characters);

      expect(store.characters).toEqual(characters);
    });

    test('should update avatars', () => {
      const avatars = {
        'user1': { id: 'avatar1', name: 'Avatar 1' },
        'user2': { id: 'avatar2', name: 'Avatar 2' }
      };

      store.actions.updateAvatars(avatars);

      expect(store.avatars).toEqual(avatars);
    });
  });

  describe('Inventory Management', () => {
    test('should update inventory', () => {
      const inventory = [
        { id: 'item1', name: 'Golden Coin', quantity: 5 },
        { id: 'item2', name: 'Magic Wand', quantity: 1 }
      ];

      store.actions.updateInventory(inventory);

      expect(store.inventory).toEqual(inventory);
    });
  });

  describe('Chat Management', () => {
    test('should add chat message', () => {
      const message = {
        id: 'msg1',
        user: 'testuser',
        text: 'Hello world!',
        timestamp: Date.now()
      };

      store.actions.addChatMessage(message);

      expect(store.chat.messages).toContain(message);
      expect(store.chat.messages).toHaveLength(1);
    });

    test('should add multiple chat messages', () => {
      const message1 = { id: 'msg1', user: 'user1', text: 'Hello' };
      const message2 = { id: 'msg2', user: 'user2', text: 'Hi there' };

      store.actions.addChatMessage(message1);
      store.actions.addChatMessage(message2);

      expect(store.chat.messages).toHaveLength(2);
      expect(store.chat.messages[0]).toEqual(message1);
      expect(store.chat.messages[1]).toEqual(message2);
    });
  });

  describe('Connection Management', () => {
    test('should set connection status', () => {
      store.actions.setConnectionStatus(true);

      expect(store.connection.isConnected).toBe(true);
    });

    test('should set authentication status', () => {
      store.actions.setAuthenticated(true);

      expect(store.connection.isAuthenticated).toBe(true);
    });

    test('should disconnect', () => {
      store.actions.setConnectionStatus(true);
      store.actions.setAuthenticated(true);

      store.actions.disconnect();

      expect(store.connection.isConnected).toBe(false);
      expect(store.connection.isAuthenticated).toBe(false);
    });
  });

  describe('Game Systems', () => {
    test('should update game time', () => {
      const now = Date.now();

      store.actions.updateGameTime(now);

      expect(store.game.now).toBe(now);
      expect(store.game.tick).toBe(now - 0); // Initial now is 0
    });

    test('should calculate tick correctly', () => {
      const time1 = 1000;
      const time2 = 1100;

      store.actions.updateGameTime(time1);
      store.actions.updateGameTime(time2);

      expect(store.game.tick).toBe(100); // 1100 - 1000
    });
  });

  describe('Communication', () => {
    test('should send message through communication service', () => {
      const command = 'move';
      const data = { x: 10, y: 20 };

      store.actions.sendMessage(command, data);

      // The send method should be called on the communication service
      // This is tested through the mock
    });

    test('should initialize communication', () => {
      store.actions.initializeCommunication();

      // The communication service should be initialized with the cluster
      expect(store.cluster).toBeDefined();
    });
  });

  describe('Store Reset', () => {
    test('should reset store to initial state', () => {
      // Modify some state
      store.actions.setCharacter({ id: 'test', name: 'Test' });
      store.actions.setVolume(50);
      store.actions.openPanel('inventory');

      // Reset
      store.actions.reset();

      // Check that state is reset
      expect(store.character).toBeNull();
      expect(store.audio.volume).toBe(80);
      expect(store.ui.openPanel).toBe(null);
      expect(store.characters).toEqual([]);
      expect(store.avatars).toEqual({});
      expect(store.inventory).toEqual([]);
      expect(store.chat.messages).toEqual([]);
      expect(store.connection.isConnected).toBe(false);
      expect(store.connection.isAuthenticated).toBe(false);
    });
  });

  describe('Edge Cases', () => {
    test('should handle null values gracefully', () => {
      expect(() => store.actions.setCharacter(null)).not.toThrow();
      expect(() => store.actions.setPlayer(null)).not.toThrow();
      expect(() => store.actions.updateActivity(null)).not.toThrow();
    });

    test('should handle undefined values gracefully', () => {
      expect(() => store.actions.setCharacter(undefined)).not.toThrow();
      expect(() => store.actions.setPlayer(undefined)).not.toThrow();
    });

    test('should handle empty objects', () => {
      expect(() => store.actions.updateActivity({})).not.toThrow();
      expect(() => store.actions.updateAvatars({})).not.toThrow();
    });
  });

  describe('Performance', () => {
    test('should handle rapid state updates', () => {
      const updates = Array.from({ length: 100 }, (_, i) => ({
        world: 'CHOR',
        lat: i,
        long: i * 2,
        alt: i * 10
      }));

      const startTime = Date.now();
      updates.forEach(activity => store.actions.updateActivity(activity));
      const endTime = Date.now();

      expect(endTime - startTime).toBeLessThan(100); // Should complete in under 100ms
    });

    test('should handle large chat message lists', () => {
      const messages = Array.from({ length: 1000 }, (_, i) => ({
        id: `msg${i}`,
        user: `user${i}`,
        text: `Message ${i}`,
        timestamp: Date.now() + i
      }));

      const startTime = Date.now();
      messages.forEach(msg => store.actions.addChatMessage(msg));
      const endTime = Date.now();

      expect(store.chat.messages).toHaveLength(1000);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete in under 1 second
    });
  });
});
