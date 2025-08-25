/**
 * useAnimationManager.ts - Vue composable for animation management
 * 
 * Provides reactive integration of AnimationManager with Vue components
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { computed, onUnmounted } from 'vue'
import { animationManager, type CharacterData, type AnimationStateType } from '@/services/AnimationManager'

/**
 * Vue composable for animation management
 */
export function useAnimationManager() {
  // Reactive computed properties
  const currentAnimation = computed(() => animationManager.currentAnimation.value)
  const isTransitioning = computed(() => animationManager.isTransitioning.value)
  const isMoving = computed(() => animationManager.isMoving.value)
  const capabilities = computed(() => animationManager.capabilities.value)

  // Methods
  const updateCharacter = (character: CharacterData) => {
    animationManager.updateCharacter(character)
  }

  const updatePosition = (position: any, deltaTime: number) => {
    animationManager.updatePosition(position, deltaTime)
  }

  const setAnimation = (animation: AnimationStateType, immediate = false) => {
    animationManager.setAnimation(animation, immediate)
  }

  const sit = () => {
    animationManager.sit()
  }

  const stand = () => {
    animationManager.stand()
  }

  const jump = () => {
    animationManager.jump()
  }

  const useItem = () => {
    animationManager.useItem()
  }

  const talk = () => {
    animationManager.talk()
  }

  const emote = () => {
    animationManager.emote()
  }

  const getCurrentAnimation = () => {
    return animationManager.getCurrentAnimation()
  }

  const getAnimationState = () => {
    return animationManager.getAnimationState()
  }

  const getMovementState = () => {
    return animationManager.getMovementState()
  }

  const getCapabilities = () => {
    return animationManager.getCapabilities()
  }

  const updateSettings = (settings: any) => {
    animationManager.updateSettings(settings)
  }

  const getDebugInfo = () => {
    return animationManager.getDebugInfo()
  }

  const reset = () => {
    animationManager.reset()
  }

  return {
    // Reactive state
    currentAnimation,
    isTransitioning,
    isMoving,
    capabilities,

    // Methods
    updateCharacter,
    updatePosition,
    setAnimation,
    sit,
    stand,
    jump,
    useItem,
    talk,
    emote,
    getCurrentAnimation,
    getAnimationState,
    getMovementState,
    getCapabilities,
    updateSettings,
    getDebugInfo,
    reset
  }
}

/**
 * Vue composable with automatic cleanup
 */
export function useAnimationManagerWithCleanup() {
  const animationManager = useAnimationManager()

  onUnmounted(() => {
    animationManager.reset()
  })

  return animationManager
}

/**
 * Vue composable with character data integration
 */
export function useAnimationManagerWithCharacter(character: CharacterData) {
  const animationManager = useAnimationManager()

  // Update character on mount
  animationManager.updateCharacter(character)

  onUnmounted(() => {
    animationManager.reset()
  })

  return {
    ...animationManager,
    // Additional methods for character-specific operations
    updateCharacterData: (newCharacter: CharacterData) => {
      animationManager.updateCharacter(newCharacter)
    }
  }
}
