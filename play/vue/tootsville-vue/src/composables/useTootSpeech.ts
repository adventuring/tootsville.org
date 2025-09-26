/**
 * useTootSpeech - Vue composable for TootSpeechService integration
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { computed, ref, onUnmounted, onMounted } from 'vue'
import { tootSpeechService } from '@/services/TootSpeechService'
import type { SpeechParameters, SpeechQueueItem } from '@/services/TootSpeechService'

/**
 * Vue composable for TootSpeechService integration
 * 
 * Provides reactive integration of TootSpeechService with Vue components.
 * Manages speech synthesis and audio playback for character voices.
 * 
 * @returns {Object} Toot speech composable with reactive state and methods
 * 
 * @example
 * const { isSpeaking, queueLength, speak, stop, pause } = useTootSpeech()
 */
export function useTootSpeech() {
  // Use singleton service
  const service = tootSpeechService

  // Reactive state - access computed properties directly
  const speechStatus = computed(() => service.speechStatus)
  const isSpeaking = computed(() => service.isSpeaking)
  const queueLength = computed(() => service.queueLength)
  const isPaused = ref(false)

  // Methods
  const speak = async (text: string, params?: Partial<SpeechParameters>) => {
    try {
      return await service.speak(text, params)
    } catch (error) {
      console.error('Speech error:', error)
      return false
    }
  }

  const queueSpeech = (text: string, params?: Partial<SpeechParameters>, priority: number = 0) => {
    try {
      return service.queueSpeech(text, params, priority)
    } catch (error) {
      console.error('Queue speech error:', error)
      return -1
    }
  }

  const stopSpeaking = () => {
    service.stopSpeaking()
  }

  const clearQueue = () => {
    return service.clearQueue()
  }

  const setVolume = (volume: number) => {
    service.setSpeechVolume(volume)
  }

  const setMasterVolume = (volume: number) => {
    service.setMasterVolume(volume)
  }

  const getSpeechStatus = () => {
    return service.getSpeechStatus()
  }

  const getMasterVolume = () => {
    return service.getMasterVolume()
  }

  const getSpeechVolume = () => {
    return service.getSpeechVolume()
  }

  const isInitialized = () => {
    return service.isInitialized()
  }

  const getError = () => {
    return service.getError()
  }

  const clearError = () => {
    service.clearError()
  }

  const getDebugInfo = () => {
    return {
      speechStatus: speechStatus.value,
      isSpeaking: isSpeaking.value,
      queueLength: queueLength.value,
      isInitialized: service.isInitialized(),
      error: service.getError(),
      masterVolume: service.getMasterVolume(),
      speechVolume: service.getSpeechVolume()
    }
  }

  // Cleanup
  onUnmounted(() => {
    service.stopSpeaking()
    service.clearQueue()
  })

  return {
    // Reactive state
    speechStatus,
    isSpeaking,
    queueLength,
    isPaused,

    // Methods
    speak,
    queueSpeech,
    stopSpeaking,
    clearQueue,
    setVolume,
    setMasterVolume,
    getSpeechStatus,
    getMasterVolume,
    getSpeechVolume,
    isInitialized,
    getError,
    clearError,
    getDebugInfo,

    // Computed properties for component use
    error: computed(() => service.getError()),
    canSpeak: computed(() => service.isInitialized() && !service.isSpeaking.value),
    clearSpeechQueue: clearQueue
  }
}

/**
 * Vue composable with automatic cleanup
 */
export function useTootSpeechWithCleanup() {
  const speech = useTootSpeech()

  onUnmounted(() => {
    tootSpeechService.dispose()
  })

  return speech
}

/**
 * Vue composable with volume control
 */
export function useTootSpeechWithVolume(initialVolume: number = 0.5) {
  const speech = useTootSpeech()
  const volume = ref(initialVolume)

  const setVolume = (newVolume: number) => {
    volume.value = newVolume
    speech.setVolume(newVolume)
  }

  const setMasterVolume = (newVolume: number) => {
    speech.setMasterVolume(newVolume)
  }

  onMounted(() => {
    speech.setVolume(volume.value)
  })

  return {
    ...speech,
    volume,
    setVolume,
    setMasterVolume
  }
}

