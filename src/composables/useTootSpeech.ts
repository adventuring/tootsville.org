import { ref, computed, onMounted, onUnmounted } from 'vue'
import { TootSpeechService } from '@/services/TootSpeechService'
import type { SpeechParameters, SpeechQueueItem } from '@/services/TootSpeechService'

// Create a singleton instance
const tootSpeechService = new TootSpeechService()

export function useTootSpeech() {
  // Reactive state
  const isPaused = ref(false)
  const volume = ref(1.0)
  const isInitialized = ref(false)
  const error = ref<string | null>(null)

  // Computed properties that match the service API
  const isSpeaking = computed(() => tootSpeechService.isSpeaking)
  const queueLength = computed(() => tootSpeechService.queueLength)
  const currentSpeech = computed(() => tootSpeechService.currentSpeech)
  const availableVoices = computed(() => tootSpeechService.availableVoices)
  const speechHistory = computed(() => tootSpeechService.speechHistory)
  const queue = computed(() => tootSpeechService.queue)
  const speechStatus = computed(() => tootSpeechService.speechStatus)
  const canSpeak = computed(() => tootSpeechService.canSpeak)

  // Speech control methods
  const speak = async (text: string, params?: SpeechParameters) => {
    try {
      error.value = null
      const result = await tootSpeechService.speak(text, params)
      return result
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
      throw err
    }
  }

  const pause = () => {
    try {
      tootSpeechService.pause()
      isPaused.value = true
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const resume = async () => {
    try {
      await tootSpeechService.resume()
      isPaused.value = false
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const stop = () => {
    try {
      tootSpeechService.stop()
      isPaused.value = false
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const setVoice = (voice: string) => {
    try {
      tootSpeechService.setVoice(voice)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const setRate = (rate: number) => {
    try {
      tootSpeechService.setRate(rate)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const setPitch = (pitch: number) => {
    try {
      tootSpeechService.setPitch(pitch)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const setVolume = (newVolume: number) => {
    try {
      volume.value = Math.max(0, Math.min(1, newVolume))
      tootSpeechService.setVolume(volume.value)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const getAvailableVoices = () => {
    return tootSpeechService.availableVoices
  }

  const getSpeechHistory = () => {
    return tootSpeechService.speechHistory
  }

  const getQueue = () => {
    return tootSpeechService.queue
  }

  const removeFromQueue = (index: number) => {
    try {
      tootSpeechService.removeFromQueue(index)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const moveInQueue = (fromIndex: number, toIndex: number) => {
    try {
      tootSpeechService.moveInQueue(fromIndex, toIndex)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  const getSpeechStatus = () => {
    return tootSpeechService.speechStatus
  }

  const resetStats = () => {
    try {
      tootSpeechService.resetStats()
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  }

  // Return the composable interface
  const speech = {
    // State
    isPaused,
    volume,
    isInitialized,
    error,
    
    // Computed properties
    isSpeaking,
    queueLength,
    currentSpeech,
    availableVoices,
    speechHistory,
    queue,
    speechStatus,
    canSpeak,
    
    // Methods
    speak,
    pause,
    resume,
    stop,
    setVoice,
    setRate,
    setPitch,
    setVolume,
    getAvailableVoices,
    getSpeechHistory,
    getQueue,
    removeFromQueue,
    moveInQueue,
    getSpeechStatus,
    resetStats,
    
    // Utility methods
    getDebugInfo: () => ({
      isPaused: isPaused.value,
      volume: volume.value,
      isInitialized: isInitialized.value,
      error: error.value,
      serviceStatus: tootSpeechService.getDebugInfo()
    })
  }

  // Cleanup function
  const cleanup = () => {
    try {
      tootSpeechService.stop()
    } catch (err) {
      console.warn('Error during cleanup:', err)
    }
  }

  // Set volume on mount
  onMounted(() => {
    try {
      isInitialized.value = true
      setVolume(volume.value)
    } catch (err) {
      error.value = err instanceof Error ? err.message : 'Unknown error'
    }
  })

  // Cleanup on unmount
  onUnmounted(() => {
    cleanup()
  })

  return speech
}
