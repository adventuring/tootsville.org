/**
 * useAnimationManager - Vue composable for AnimationManager integration
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { computed, ref, onUnmounted } from 'vue'
import { AnimationManager } from '@/services/AnimationManager'
import type { CharacterData, AnimationState, CharacterCapabilities } from '@/services/AnimationManager'

/**
 * Vue composable for AnimationManager integration
 * 
 * Provides reactive integration of AnimationManager with Vue components.
 * Manages character animation state and provides animation controls.
 * 
 * @returns {Object} Animation manager composable with reactive state and methods
 * 
 * @example
 * const { currentAnimation, isMoving, capabilities, updateCharacter, sit, stand } = useAnimationManager()
 */
export function useAnimationManager() {
  const animationManager = new AnimationManager()
  
  // Reactive state
  const currentAnimation = computed(() => animationManager.getCurrentAnimation().current)
  const isTransitioning = computed(() => animationManager.isTransitioning())
  const isMoving = computed(() => animationManager.isMoving())
  const capabilities = computed(() => animationManager.getCharacterCapabilities())

  // Methods
  const updateCharacter = (character: CharacterData) => {
    animationManager.updateCharacterData(character)
  }

  const updatePosition = (position: { x: number; y: number; z: number }, deltaTime: number) => {
    // Handle position updates
    animationManager.updateCharacterData({
      position,
      deltaTime
    } as CharacterData)
  }

  const setAnimation = (animation: string) => {
    animationManager.setAnimation(animation)
  }

  const sit = () => {
    animationManager.setAnimation('sit')
  }

  const stand = () => {
    animationManager.setAnimation('idle')
  }

  const jump = () => {
    animationManager.setAnimation('jump')
  }

  const useItem = () => {
    animationManager.setAnimation('use_item')
  }

  const talk = () => {
    animationManager.setAnimation('talk')
  }

  const emote = () => {
    animationManager.setAnimation('emote')
  }

  const reset = () => {
    animationManager.reset()
  }

  // Getters
  const getAnimationState = (): AnimationState => {
    return animationManager.animationState
  }

  const getCapabilities = (): CharacterCapabilities | null => {
    return animationManager.getCharacterCapabilities()
  }

  const getDebugInfo = () => {
    return {
      currentAnimation: animationManager.getCurrentAnimation(),
      isTransitioning: animationManager.isTransitioning(),
      isMoving: animationManager.isMoving(),
      capabilities: animationManager.getCharacterCapabilities(),
      characterData: animationManager.getCharacterData()
    }
  }

  // Cleanup
  onUnmounted(() => {
    // Cleanup if needed
  })

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
    reset,

    // Getters
    getAnimationState,
    getCapabilities,
    getDebugInfo
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
