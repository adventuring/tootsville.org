/**
 * Game Store - Pinia store for game state management
 * 
 * Vue 3 / TypeScript version replacing Zustand GameStore
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { defineStore } from 'pinia'
import { ref, reactive, computed } from 'vue'

// Types
export interface GameState {
  now: number
  tick: number
  lastUpdate: number
}

export interface CharacterState {
  id: string
  name: string
  avatar: string
  position: { x: number; y: number; z: number }
  rotation: { x: number; y: number; z: number }
  scale: { x: number; y: number; z: number }
  animation: string
  isMoving: boolean
  isSitting: boolean
  isJumping: boolean
  isSwimming: boolean
  isFlying: boolean
  health: number
  energy: number
  level: number
  experience: number
}

export interface AvatarState {
  id: string
  name: string
  type: string
  position: { x: number; y: number; z: number }
  rotation: { x: number; y: number; z: number }
  scale: { x: number; y: number; z: number }
  animation: string
  isMoving: boolean
  isSitting: boolean
  isJumping: boolean
  isSwimming: boolean
  isFlying: boolean
  lastUpdate: number
}

export interface InventoryState {
  items: any[]
  capacity: number
  weight: number
  maxWeight: number
}

export interface ChatState {
  messages: any[]
  isOpen: boolean
  currentChannel: string
  unreadCount: number
}

export interface ConnectionState {
  isConnected: boolean
  protocol: string | null
  cluster: string
  latency: number
  lastPing: number
}

export interface UIState {
  isLoading: boolean
  currentView: string
  modals: string[]
  notifications: any[]
  settings: any
}

/**
 * Game Store - Centralized state management for the game
 */
export const useGameStore = defineStore('game', () => {
  // Game state
  const game = reactive<GameState>({
    now: 0,
    tick: 0,
    lastUpdate: 0
  })

  // Character state
  const character = reactive<CharacterState>({
    id: '',
    name: '',
    avatar: '',
    position: { x: 0, y: 0, z: 0 },
    rotation: { x: 0, y: 0, z: 0 },
    scale: { x: 1, y: 1, z: 1 },
    animation: 'idle',
    isMoving: false,
    isSitting: false,
    isJumping: false,
    isSwimming: false,
    isFlying: false,
    health: 100,
    energy: 100,
    level: 1,
    experience: 0
  })

  // Avatars state (other players)
  const avatars = reactive<Map<string, AvatarState>>(new Map())

  // Inventory state
  const inventory = reactive<InventoryState>({
    items: [],
    capacity: 50,
    weight: 0,
    maxWeight: 100
  })

  // Chat state
  const chat = reactive<ChatState>({
    messages: [],
    isOpen: false,
    currentChannel: 'public',
    unreadCount: 0
  })

  // Connection state
  const connection = reactive<ConnectionState>({
    isConnected: false,
    protocol: null,
    cluster: 'test',
    latency: 0,
    lastPing: 0
  })

  // UI state
  const ui = reactive<UIState>({
    isLoading: false,
    currentView: 'game',
    modals: [],
    notifications: [],
    settings: {}
  })

  // Computed properties
  const isGameRunning = computed(() => game.now > 0)
  const isCharacterLoaded = computed(() => character.id !== '')
  const isConnected = computed(() => connection.isConnected)
  const avatarCount = computed(() => avatars.size)
  const hasUnreadMessages = computed(() => chat.unreadCount > 0)

  // Actions
  function updateGameTime(now: number) {
    const previousNow = game.now
    game.now = now
    game.lastUpdate = Date.now()
    
    // Calculate tick only after game.now has been initialized
    game.tick = previousNow > 0 ? now - previousNow : 0
  }

  function setCharacter(charData: Partial<CharacterState>) {
    Object.assign(character, charData)
  }

  function updateCharacterPosition(position: { x: number; y: number; z: number }) {
    character.position = position
  }

  function updateCharacterRotation(rotation: { x: number; y: number; z: number }) {
    character.rotation = rotation
  }

  function setCharacterAnimation(animation: string) {
    character.animation = animation
    
    // Update movement flags based on animation
    character.isMoving = ['walk', 'run'].includes(animation)
    character.isSitting = animation === 'sit'
    character.isJumping = animation === 'jump'
    character.isSwimming = animation === 'swim'
    character.isFlying = animation === 'fly'
  }

  function addAvatar(avatarData: AvatarState) {
    avatars.set(avatarData.id, avatarData)
  }

  function updateAvatar(id: string, updates: Partial<AvatarState>) {
    const avatar = avatars.get(id)
    if (avatar) {
      Object.assign(avatar, updates)
    }
  }

  function removeAvatar(id: string) {
    avatars.delete(id)
  }

  function clearAvatars() {
    avatars.clear()
  }

  function addInventoryItem(item: any) {
    inventory.items.push(item)
    inventory.weight += item.weight || 0
  }

  function removeInventoryItem(itemId: string) {
    const index = inventory.items.findIndex(item => item.id === itemId)
    if (index !== -1) {
      const item = inventory.items[index]
      inventory.weight -= item.weight || 0
      inventory.items.splice(index, 1)
    }
  }

  function addChatMessage(message: any) {
    chat.messages.push(message)
    if (!chat.isOpen) {
      chat.unreadCount++
    }
  }

  function clearChatMessages() {
    chat.messages = []
    chat.unreadCount = 0
  }

  function openChat() {
    chat.isOpen = true
    chat.unreadCount = 0
  }

  function closeChat() {
    chat.isOpen = false
  }

  function setConnectionStatus(status: Partial<ConnectionState>) {
    Object.assign(connection, status)
  }

  function setUIState(updates: Partial<UIState>) {
    Object.assign(ui, updates)
  }

  function showModal(modalName: string) {
    if (!ui.modals.includes(modalName)) {
      ui.modals.push(modalName)
    }
  }

  function hideModal(modalName: string) {
    const index = ui.modals.indexOf(modalName)
    if (index !== -1) {
      ui.modals.splice(index, 1)
    }
  }

  function addNotification(notification: any) {
    ui.notifications.push(notification)
  }

  function removeNotification(id: string) {
    const index = ui.notifications.findIndex(n => n.id === id)
    if (index !== -1) {
      ui.notifications.splice(index, 1)
    }
  }

  function resetGameState() {
    // Reset game state
    game.now = 0
    game.tick = 0
    game.lastUpdate = 0

    // Reset character state
    Object.assign(character, {
      id: '',
      name: '',
      avatar: '',
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0 },
      scale: { x: 1, y: 1, z: 1 },
      animation: 'idle',
      isMoving: false,
      isSitting: false,
      isJumping: false,
      isSwimming: false,
      isFlying: false,
      health: 100,
      energy: 100,
      level: 1,
      experience: 0
    })

    // Reset other states
    avatars.clear()
    inventory.items = []
    inventory.weight = 0
    chat.messages = []
    chat.unreadCount = 0
    connection.isConnected = false
    connection.protocol = null
    ui.isLoading = false
    ui.modals = []
    ui.notifications = []
  }

  return {
    // State
    game,
    character,
    avatars,
    inventory,
    chat,
    connection,
    ui,

    // Computed
    isGameRunning,
    isCharacterLoaded,
    isConnected,
    avatarCount,
    hasUnreadMessages,

    // Actions
    updateGameTime,
    setCharacter,
    updateCharacterPosition,
    updateCharacterRotation,
    setCharacterAnimation,
    addAvatar,
    updateAvatar,
    removeAvatar,
    clearAvatars,
    addInventoryItem,
    removeInventoryItem,
    addChatMessage,
    clearChatMessages,
    openChat,
    closeChat,
    setConnectionStatus,
    setUIState,
    showModal,
    hideModal,
    addNotification,
    removeNotification,
    resetGameState
  }
})
