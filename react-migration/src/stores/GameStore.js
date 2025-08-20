/**
 * GameStore - Central state management for Tootsville game
 * Preserves existing data structures while providing reactive state management
 * 
 * Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA
 */

import { create } from 'zustand';
import { subscribeWithSelector } from 'zustand/middleware';
import communicationService from '../services/CommunicationService';

const useGameStore = create(
  subscribeWithSelector((set, get) => ({
    // Core game state - preserving original structure
    cluster: document.location.host.split('.').splice(1).join('.'),
    character: null,
    player: null,
    activity: { 
      world: 'CHOR', 
      lat: 0, 
      long: 0, 
      alt: 0 
    },
    characterUUID: null,
    privateKey: null,
    
    // Audio settings
    audio: { 
      volume: 80,
      savedVolume: 80 
    },
    
    // World configurations
    worlds: {
      CHOR: { Gravity: 9.81 },
      MOON: { Gravity: 0.32 },
      OTHM: { Gravity: 0.12 },
      PINK: { Gravity: 0.08 }
    },
    
    // Game systems
    game: {
      lag: 50,
      now: 0,
      tick: 0
    },
    
    // UI state
    ui: {
      talkBoxOpen: true,
      openPanel: null,
      seen: {}
    },
    
    // Character and avatar data
    characters: [],
    avatars: {},
    inventory: [],
    
    // Chat and communication
    chat: {
      messages: [],
      history: []
    },
    
    // Connection state
    connection: {
      isConnected: false,
      isAuthenticated: false,
      reconnectAttempts: 0
    },
    
    // Actions
    actions: {
      // Authentication
      setCharacter: (character) => set({ character }),
      setPlayer: (player) => set({ player }),
      setCharacterUUID: (uuid) => set({ characterUUID: uuid }),
      
      // Position and activity
      updateActivity: (activity) => set((state) => ({
        activity: { ...state.activity, ...activity }
      })),
      
      // Audio
      setVolume: (volume) => set((state) => ({
        audio: { ...state.audio, volume }
      })),
      
      // UI
      setTalkBoxOpen: (open) => set((state) => ({
        ui: { ...state.ui, talkBoxOpen: open }
      })),
      
      openPanel: (panelId) => set((state) => ({
        ui: { ...state.ui, openPanel: panelId }
      })),
      
      closePanel: () => set((state) => ({
        ui: { ...state.ui, openPanel: null }
      })),
      
      // Characters and avatars
      updateCharacters: (characters) => set({ characters }),
      updateAvatars: (avatars) => set({ avatars }),
      
      // Inventory
      updateInventory: (inventory) => set({ inventory }),
      
      // Chat
      addChatMessage: (message) => set((state) => ({
        chat: {
          ...state.chat,
          messages: [...state.chat.messages, message]
        }
      })),
      
      // Connection
      setConnectionStatus: (isConnected) => set((state) => ({
        connection: { ...state.connection, isConnected }
      })),
      
      setAuthenticated: (isAuthenticated) => set((state) => ({
        connection: { ...state.connection, isAuthenticated }
      })),
      
      // Game systems
      updateGameTime: (now) => set((state) => ({
        game: { 
          ...state.game, 
          now,
          tick: now - state.game.now
        }
      })),
      
      // Communication actions
      sendMessage: (command, data) => {
        communicationService.send(command, data);
      },
      
      // Initialize communication
      initializeCommunication: () => {
        const { cluster } = get();
        
        communicationService.on('connected', () => {
          get().actions.setConnectionStatus(true);
        });
        
        communicationService.on('disconnected', () => {
          get().actions.setConnectionStatus(false);
        });
        
        communicationService.on('loginSuccess', (data) => {
          get().actions.setAuthenticated(true);
        });
        
        communicationService.on('avatarsUpdate', (avatars) => {
          get().actions.updateAvatars(avatars);
        });
        
        communicationService.on('publicMessage', (message) => {
          get().actions.addChatMessage(message);
        });
        
        communicationService.on('inventoryUpdate', (inventory) => {
          get().actions.updateInventory(inventory);
        });
        
        communicationService.connect(cluster);
      },
      
      // Disconnect
      disconnect: () => {
        communicationService.disconnect();
        set((state) => ({
          connection: { 
            ...state.connection, 
            isConnected: false, 
            isAuthenticated: false 
          }
        }));
      }
    }
  }))
);

export default useGameStore;
