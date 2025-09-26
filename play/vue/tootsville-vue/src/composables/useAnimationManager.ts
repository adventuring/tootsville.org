/**
 * useAnimationManager - Vue composable for AnimationManager integration
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { computed, ref, onUnmounted, onMounted } from 'vue'
import { animationManager } from '@/services/AnimationManager'
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
  // Use singleton service
  const service = animationManager

  // Reactive state - access computed properties directly
  const currentAnimation = computed(() => service.animationState.value.current)
  const isTransitioning = computed(() => service.animationState.value.isTransitioning)
  const isMoving = computed(() => service.movementState.value.isMoving)
  const capabilities = computed(() => service.characterCapabilities.value)

  // Methods
  const updateCharacterData = (character: CharacterData) => {
    service.updateCharacterData(character)
  }

  const updateMovementState = (position: { x: number; y: number; z: number }, timestamp: number) => {
    // Convert position object to Vector3
    const vector3Pos = { x: position.x, y: position.y, z: position.z } as any
    service.updateMovementState(vector3Pos, timestamp)
  }

  const setAnimation = (animation: string, force: boolean = false) => {
    service.setAnimation(animation, force)
  }

  const canPerformAction = (action: string) => {
    return service.canPerformAction.value(action)
  }

  const sit = () => {
    service.setAnimation('sit')
  }

  const stand = () => {
    service.setAnimation('idle')
  }

  const jump = () => {
    if (service.canPerformAction.value('jump')) {
      service.setAnimation('jump')
    }
  }

  const useItem = () => {
    service.setAnimation('use_item')
  }

  const talk = () => {
    service.setAnimation('talk')
  }

  const emote = () => {
    service.setAnimation('emote')
  }

  const reset = () => {
    service.reset()
  }

  const updateSettings = (settings: Partial<{ fadeDuration: number; crossfadeDuration: number; minMovementThreshold: number; movementCheckInterval: number }>) => {
    service.updateSettings(settings)
  }

  // Getters
  const getAnimationState = (): AnimationState => {
    return service.getCurrentAnimation()
  }

  const getCharacterData = () => {
    return service.characterData.value
  }

  const getMovementState = () => {
    return service.getMovementState()
  }

  const getSettings = () => {
    return service.getSettings()
  }

  const getCharacterType = () => {
    return service.getCharacterType()
  }

  const getCharacterName = () => {
    return service.getCharacterName()
  }

  const getAvailableAnimations = () => {
    return service.getAvailableAnimations()
  }

  const isValidAnimation = (animation: string) => {
    return service.isValidAnimation(animation)
  }

  const getDebugInfo = () => {
    return {
      animationState: service.animationState.value,
      movementState: service.movementState.value,
      characterData: service.characterData.value,
      capabilities: service.characterCapabilities.value,
      settings: service.settings.value,
      isTransitioning: service.isTransitioning(),
      transitionProgress: service.getTransitionProgress(),
      availableAnimations: service.getAvailableAnimations()
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
    updateCharacterData,
    updateMovementState,
    setAnimation,
    canPerformAction,
    sit,
    stand,
    jump,
    useItem,
    talk,
    emote,
    reset,
    updateSettings,

    // Getters
    getAnimationState,
    getCharacterData,
    getMovementState,
    getSettings,
    getCharacterType,
    getCharacterName,
    getAvailableAnimations,
    isValidAnimation,
    getDebugInfo
  }
}

/**
 * Vue composable with automatic cleanup
 */
export function useAnimationManagerWithCleanup() {
  const animManager = useAnimationManager()

  onUnmounted(() => {
    animManager.reset()
  })

  return animManager
}

/**
 * Vue composable with character data integration
 */
export function useAnimationManagerWithCharacter(character: CharacterData) {
  const animManager = useAnimationManager()

  // Update character on mount
  onMounted(() => {
    animManager.updateCharacterData(character)
  })

  onUnmounted(() => {
    animManager.reset()
  })

  return {
    ...animManager,
    // Additional methods for character-specific operations
    updateCharacter: (newCharacter: CharacterData) => {
      animManager.updateCharacterData(newCharacter)
    }
  }
}
