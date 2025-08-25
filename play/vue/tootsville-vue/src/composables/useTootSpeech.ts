/**
 * useTootSpeech.ts - Vue composable for Toot Speech Service
 * 
 * Vue 3 / TypeScript version replacing React hook
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, computed, onMounted, onUnmounted } from 'vue'
import { tootSpeechService, type SpeechParameters, type SpeechStatus } from '@/services/TootSpeechService'

/**
 * Vue composable for Toot Speech Service
 * 
 * Provides reactive access to speech synthesis functionality
 */
export function useTootSpeech() {
  // Reactive state
  const isSpeaking = ref(false)
  const speechStatus = ref<SpeechStatus>({
    isSpeaking: false,
    currentText: null,
    queueLength: 0,
    isInitialized: false,
    error: null
  })
  const isInitialized = ref(false)
  const error = ref<string | null>(null)

  // Computed properties
  const canSpeak = computed(() => isInitialized.value && !error.value)
  const queueLength = computed(() => speechStatus.value.queueLength)

  // Event handlers
  const handleSpeechStarted = (data: { text: string; parameters: SpeechParameters }) => {
    isSpeaking.value = true
    speechStatus.value.isSpeaking = true
    speechStatus.value.currentText = data.text
  }

  const handleSpeechEnded = (data: { text: string; parameters: SpeechParameters }) => {
    isSpeaking.value = false
    speechStatus.value.isSpeaking = false
    speechStatus.value.currentText = null
  }

  const handleSpeechQueued = (queueItem: any) => {
    speechStatus.value.queueLength = tootSpeechService.queueLength.value
  }

  const handleQueueCleared = () => {
    speechStatus.value.queueLength = 0
  }

  const handleInitialized = () => {
    isInitialized.value = true
    error.value = null
  }

  const handleError = (err: any) => {
    error.value = err.message || 'Speech service error'
  }

  const handleVolumeChanged = (data: { type: string; volume: number }) => {
    // Update local state if needed
  }

  // Methods
  const speak = async (text: string, parameters?: Partial<SpeechParameters>): Promise<boolean> => {
    try {
      return await tootSpeechService.speak(text, parameters)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Failed to speak'
      return false
    }
  }

  const stopSpeaking = (): void => {
    tootSpeechService.stopSpeaking()
  }

  const queueSpeech = (text: string, parameters?: Partial<SpeechParameters>, priority?: number): void => {
    tootSpeechService.queueSpeech(text, parameters, priority)
  }

  const clearSpeechQueue = (): void => {
    tootSpeechService.clearSpeechQueue()
  }

  const setVolume = (volume: number): void => {
    tootSpeechService.setSpeechVolume(volume)
  }

  const setMasterVolume = (volume: number): void => {
    tootSpeechService.setMasterVolume(volume)
  }

  const resume = async (): Promise<void> => {
    await tootSpeechService.resume()
  }

  const suspend = async (): Promise<void> => {
    await tootSpeechService.suspend()
  }

  const initialize = async (): Promise<boolean> => {
    try {
      const success = await tootSpeechService.initialize()
      if (success) {
        isInitialized.value = true
        error.value = null
      }
      return success
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Failed to initialize'
      return false
    }
  }

  const dispose = (): void => {
    tootSpeechService.dispose()
    isInitialized.value = false
    isSpeaking.value = false
    speechStatus.value = {
      isSpeaking: false,
      currentText: null,
      queueLength: 0,
      isInitialized: false,
      error: null
    }
  }

  // Setup event listeners
  const setupEventListeners = () => {
    tootSpeechService.on('speechStarted', handleSpeechStarted)
    tootSpeechService.on('speechEnded', handleSpeechEnded)
    tootSpeechService.on('speechQueued', handleSpeechQueued)
    tootSpeechService.on('queueCleared', handleQueueCleared)
    tootSpeechService.on('initialized', handleInitialized)
    tootSpeechService.on('error', handleError)
    tootSpeechService.on('volumeChanged', handleVolumeChanged)
  }

  const cleanupEventListeners = () => {
    tootSpeechService.off('speechStarted', handleSpeechStarted)
    tootSpeechService.off('speechEnded', handleSpeechEnded)
    tootSpeechService.off('speechQueued', handleSpeechQueued)
    tootSpeechService.off('queueCleared', handleQueueCleared)
    tootSpeechService.off('initialized', handleInitialized)
    tootSpeechService.off('error', handleError)
    tootSpeechService.off('volumeChanged', handleVolumeChanged)
  }

  // Lifecycle
  onMounted(() => {
    setupEventListeners()
    // Auto-initialize if not already initialized
    if (!tootSpeechService.speechStatus.value.isInitialized) {
      initialize()
    } else {
      isInitialized.value = true
      speechStatus.value = tootSpeechService.speechStatus.value
    }
  })

  onUnmounted(() => {
    cleanupEventListeners()
  })

  return {
    // State
    isSpeaking,
    speechStatus,
    isInitialized,
    error,

    // Computed
    canSpeak,
    queueLength,

    // Methods
    speak,
    stopSpeaking,
    queueSpeech,
    clearSpeechQueue,
    setVolume,
    setMasterVolume,
    resume,
    suspend,
    initialize,
    dispose
  }
}

/**
 * Vue composable with automatic cleanup
 */
export function useTootSpeechWithCleanup() {
  const speech = useTootSpeech()

  onUnmounted(() => {
    speech.dispose()
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
