/**
 * useTootSpeech - Vue composable for TootSpeechService integration
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { computed, ref, onUnmounted } from 'vue'
import { TootSpeechService } from '@/services/TootSpeechService'
import type { SpeechParams, SpeechQueueItem } from '@/services/TootSpeechService'

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
  const tootSpeechService = new TootSpeechService()
  
  // Reactive state
  const isSpeaking = computed(() => tootSpeechService.isSpeaking())
  const queueLength = computed(() => tootSpeechService.getQueueLength())
  const currentSpeech = computed(() => tootSpeechService.getCurrentSpeech())
  const isPaused = ref(false)

  // Methods
  const speak = async (text: string, params?: Partial<SpeechParams>) => {
    try {
      await tootSpeechService.speak(text, params)
    } catch (error) {
      console.error('Speech error:', error)
    }
  }

  const queueSpeech = async (text: string, params?: Partial<SpeechParams>) => {
    try {
      await tootSpeechService.queueSpeech(text, params)
    } catch (error) {
      console.error('Queue speech error:', error)
    }
  }

  const stop = () => {
    tootSpeechService.stop()
  }

  const pause = () => {
    tootSpeechService.pause()
    isPaused.value = true
  }

  const resume = async () => {
    try {
      await tootSpeechService.resume()
      isPaused.value = false
    } catch (error) {
      console.error('Resume error:', error)
    }
  }

  const clearQueue = () => {
    tootSpeechService.clearQueue()
  }

  const setVoice = (voice: string) => {
    tootSpeechService.setVoice(voice)
  }

  const setRate = (rate: number) => {
    tootSpeechService.setRate(rate)
  }

  const setPitch = (pitch: number) => {
    tootSpeechService.setPitch(pitch)
  }

  const setVolume = (volume: number) => {
    tootSpeechService.setVolume(volume)
  }

  const getAvailableVoices = () => {
    return tootSpeechService.getAvailableVoices()
  }

  const getSpeechHistory = () => {
    return tootSpeechService.getSpeechHistory()
  }

  const getQueue = (): SpeechQueueItem[] => {
    return tootSpeechService.getQueue()
  }

  const removeFromQueue = (index: number) => {
    tootSpeechService.removeFromQueue(index)
  }

  const moveInQueue = (fromIndex: number, toIndex: number) => {
    tootSpeechService.moveInQueue(fromIndex, toIndex)
  }

  const getSpeechStats = () => {
    return tootSpeechService.getSpeechStats()
  }

  const resetStats = () => {
    tootSpeechService.resetStats()
  }

  const getDebugInfo = () => {
    return {
      isSpeaking: tootSpeechService.isSpeaking(),
      queueLength: tootSpeechService.getQueueLength(),
      currentSpeech: tootSpeechService.getCurrentSpeech(),
      availableVoices: tootSpeechService.getAvailableVoices(),
      speechHistory: tootSpeechService.getSpeechHistory(),
      queue: tootSpeechService.getQueue(),
      stats: tootSpeechService.getSpeechStats()
    }
  }

  // Cleanup
  onUnmounted(() => {
    tootSpeechService.stop()
    tootSpeechService.clearQueue()
  })

  return {
    // Reactive state
    isSpeaking,
    queueLength,
    currentSpeech,
    isPaused,

    // Methods
    speak,
    queueSpeech,
    stop,
    pause,
    resume,
    clearQueue,
    setVoice,
    setRate,
    setPitch,
    setVolume,
    getAvailableVoices,
    getSpeechHistory,
    getQueue,
    removeFromQueue,
    moveInQueue,
    getSpeechStats,
    resetStats,
    getDebugInfo
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
