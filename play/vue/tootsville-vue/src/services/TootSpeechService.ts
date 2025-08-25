/**
 * TootSpeechService.ts - Toot Speech Synthesis Service
 * 
 * Vue 3 / TypeScript version for audio processing with pitch and duration shifting
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'
import { EventEmitter } from 'events'

// Types
export interface SpeechParameters {
  pitch: number
  duration: number
  volume: number
  speed: number
}

export interface SpeechQueueItem {
  text: string
  parameters?: Partial<SpeechParameters>
  priority?: number
  timestamp: number
}

export interface SpeechStatus {
  isSpeaking: boolean
  currentText: string | null
  queueLength: number
  isInitialized: boolean
  error: string | null
}

export interface AudioConfig {
  baseVolume: number
  speechVolume: number
  pitchRange: { min: number; max: number }
  durationRange: { min: number; max: number }
  bufferCache: Map<string, AudioBuffer>
}

/**
 * Toot Speech Synthesis Service
 * 
 * Handles audio processing with pitch and duration shifting for Toot speech
 * using Web Audio API with Vue 3 reactive patterns.
 */
export class TootSpeechService extends EventEmitter {
  // Audio context and nodes
  private audioContext: AudioContext | null = null
  private masterGain: GainNode | null = null
  private speechGain: GainNode | null = null

  // Configuration
  private config: AudioConfig = {
    baseVolume: 0.7,
    speechVolume: 0.5,
    pitchRange: { min: 0.5, max: 2.0 },
    durationRange: { min: 0.5, max: 2.0 },
    bufferCache: new Map()
  }

  // Reactive state
  private _isSpeaking = ref<boolean>(false)
  private _currentText = ref<string | null>(null)
  private _speechQueue = ref<SpeechQueueItem[]>([])
  private _isInitialized = ref<boolean>(false)
  private _error = ref<string | null>(null)
  private _masterVolume = ref<number>(0.7)
  private _speechVolume = ref<number>(0.5)

  // Computed properties
  public readonly speechStatus = computed<SpeechStatus>(() => ({
    isSpeaking: this._isSpeaking.value,
    currentText: this._currentText.value,
    queueLength: this._speechQueue.value.length,
    isInitialized: this._isInitialized.value,
    error: this._error.value
  }))

  public readonly isSpeaking = computed(() => this._isSpeaking.value)
  public readonly queueLength = computed(() => this._speechQueue.value.length)

  constructor() {
    super()
  }

  /**
   * Initialize the audio context and gain nodes
   */
  async initialize(): Promise<boolean> {
    try {
      // Create audio context
      this.audioContext = new (window.AudioContext || (window as any).webkitAudioContext)()
      
      // Create gain nodes
      this.masterGain = this.audioContext.createGain()
      this.speechGain = this.audioContext.createGain()
      
      // Set initial volumes
      this.masterGain.gain.value = this._masterVolume.value
      this.speechGain.gain.value = this._speechVolume.value
      
      // Connect nodes
      this.speechGain.connect(this.masterGain)
      this.masterGain.connect(this.audioContext.destination)
      
      this._isInitialized.value = true
      this._error.value = null
      
      this.emit('initialized')
      return true
    } catch (error) {
      this._error.value = `Failed to initialize audio: ${error}`
      this.emit('error', error)
      return false
    }
  }

  /**
   * Set master volume
   */
  setMasterVolume(volume: number): void {
    this._masterVolume.value = Math.max(0, Math.min(1, volume))
    if (this.masterGain) {
      this.masterGain.gain.value = this._masterVolume.value
    }
    this.emit('volumeChanged', { type: 'master', volume: this._masterVolume.value })
  }

  /**
   * Set speech volume
   */
  setSpeechVolume(volume: number): void {
    this._speechVolume.value = Math.max(0, Math.min(1, volume))
    if (this.speechGain) {
      this.speechGain.gain.value = this._speechVolume.value
    }
    this.emit('volumeChanged', { type: 'speech', volume: this._speechVolume.value })
  }

  /**
   * Load audio buffer with caching
   */
  async loadAudioBuffer(url: string): Promise<AudioBuffer | null> {
    // Check cache first
    if (this.config.bufferCache.has(url)) {
      return this.config.bufferCache.get(url)!
    }

    try {
      if (!this.audioContext) {
        throw new Error('Audio context not initialized')
      }

      const response = await fetch(url)
      const arrayBuffer = await response.arrayBuffer()
      const audioBuffer = await this.audioContext.decodeAudioData(arrayBuffer)
      
      // Cache the buffer
      this.config.bufferCache.set(url, audioBuffer)
      
      return audioBuffer
    } catch (error) {
      this._error.value = `Failed to load audio: ${error}`
      this.emit('error', error)
      return null
    }
  }

  /**
   * Create audio source with pitch and duration control
   */
  createAudioSource(buffer: AudioBuffer, parameters: SpeechParameters): AudioBufferSourceNode | null {
    if (!this.audioContext) {
      return null
    }

    try {
      const source = this.audioContext.createBufferSource()
      source.buffer = buffer
      
      // Apply pitch shifting via playbackRate
      source.playbackRate.value = parameters.pitch
      
      // Apply duration control
      const originalDuration = buffer.duration
      const targetDuration = parameters.duration
      const rateMultiplier = originalDuration / targetDuration
      source.playbackRate.value *= rateMultiplier
      
      // Connect to speech gain node
      source.connect(this.speechGain!)
      
      return source
    } catch (error) {
      this._error.value = `Failed to create audio source: ${error}`
      this.emit('error', error)
      return null
    }
  }

  /**
   * Play a sound with specified parameters
   */
  async playSound(url: string, parameters: SpeechParameters): Promise<boolean> {
    try {
      const buffer = await this.loadAudioBuffer(url)
      if (!buffer) return false

      const source = this.createAudioSource(buffer, parameters)
      if (!source) return false

      source.start()
      
      source.onended = () => {
        this.emit('soundEnded', { url, parameters })
      }

      return true
    } catch (error) {
      this._error.value = `Failed to play sound: ${error}`
      this.emit('error', error)
      return false
    }
  }

  /**
   * Calculate speech parameters based on text analysis
   */
  calculateSpeechParameters(text: string): SpeechParameters {
    // Analyze text for pitch and duration
    const words = text.toLowerCase().split(/\s+/)
    const wordCount = words.length
    
    // Base parameters
    let pitch = 1.0
    let duration = 1.0
    let speed = 1.0
    
    // Analyze for exclamations (higher pitch)
    const exclamationCount = (text.match(/!/g) || []).length
    if (exclamationCount > 0) {
      pitch += exclamationCount * 0.1
    }
    
    // Analyze for questions (slight pitch increase)
    const questionCount = (text.match(/\?/g) || []).length
    if (questionCount > 0) {
      pitch += questionCount * 0.05
    }
    
    // Analyze for emphasis (CAPS)
    const capsRatio = (text.match(/[A-Z]/g) || []).length / text.length
    if (capsRatio > 0.3) {
      pitch += 0.2
      speed += 0.1
    }
    
    // Analyze for long sentences (slower)
    if (wordCount > 10) {
      speed -= 0.1
    }
    
    // Analyze for short sentences (faster)
    if (wordCount < 3) {
      speed += 0.1
    }
    
    // Clamp values to ranges
    pitch = Math.max(this.config.pitchRange.min, Math.min(this.config.pitchRange.max, pitch))
    speed = Math.max(0.5, Math.min(2.0, speed))
    duration = 1.0 / speed
    
    return {
      pitch,
      duration,
      volume: this._speechVolume.value,
      speed
    }
  }

  /**
   * Speak text with automatic parameter calculation
   */
  async speak(text: string, customParameters?: Partial<SpeechParameters>): Promise<boolean> {
    try {
      if (!this._isInitialized.value) {
        await this.initialize()
      }

      this._isSpeaking.value = true
      this._currentText.value = text
      
      // Calculate parameters
      const baseParameters = this.calculateSpeechParameters(text)
      const parameters = { ...baseParameters, ...customParameters }
      
      // Generate audio URL (this would be replaced with actual Toot speech synthesis)
      const audioUrl = this.generateAudioUrl(text)
      
      // Play the sound
      const success = await this.playSound(audioUrl, parameters)
      
      if (success) {
        this.emit('speechStarted', { text, parameters })
        
        // Simulate speech duration (in real implementation, this would be based on actual audio)
        setTimeout(() => {
          this._isSpeaking.value = false
          this._currentText.value = null
          this.emit('speechEnded', { text, parameters })
        }, parameters.duration * 1000)
      } else {
        this._isSpeaking.value = false
        this._currentText.value = null
      }
      
      return success
    } catch (error) {
      this._error.value = `Failed to speak: ${error}`
      this.emit('error', error)
      this._isSpeaking.value = false
      this._currentText.value = null
      return false
    }
  }

  /**
   * Stop current speech
   */
  stopSpeaking(): void {
    this._isSpeaking.value = false
    this._currentText.value = null
    this.emit('speechStopped')
  }

  /**
   * Queue speech for later processing
   */
  queueSpeech(text: string, parameters?: Partial<SpeechParameters>, priority: number = 0): void {
    const queueItem: SpeechQueueItem = {
      text,
      parameters,
      priority,
      timestamp: Date.now()
    }
    
    this._speechQueue.value.push(queueItem)
    
    // Sort by priority (higher priority first)
    this._speechQueue.value.sort((a, b) => (b.priority || 0) - (a.priority || 0))
    
    this.emit('speechQueued', queueItem)
    
    // Process queue if not currently speaking
    if (!this._isSpeaking.value) {
      this.processSpeechQueue()
    }
  }

  /**
   * Process the speech queue
   */
  private async processSpeechQueue(): Promise<void> {
    if (this._speechQueue.value.length === 0 || this._isSpeaking.value) {
      return
    }

    const nextItem = this._speechQueue.value.shift()!
    await this.speak(nextItem.text, nextItem.parameters)
    
    // Process next item if available
    if (this._speechQueue.value.length > 0) {
      setTimeout(() => this.processSpeechQueue(), 100)
    }
  }

  /**
   * Clear the speech queue
   */
  clearSpeechQueue(): void {
    this._speechQueue.value = []
    this.emit('queueCleared')
  }

  /**
   * Get current speech status
   */
  getSpeechStatus(): SpeechStatus {
    return this.speechStatus.value
  }

  /**
   * Dispose of audio resources
   */
  dispose(): void {
    if (this.audioContext) {
      this.audioContext.close()
      this.audioContext = null
    }
    
    this.masterGain = null
    this.speechGain = null
    this.config.bufferCache.clear()
    
    this._isInitialized.value = false
    this._isSpeaking.value = false
    this._currentText.value = null
    this._speechQueue.value = []
    
    this.emit('disposed')
  }

  /**
   * Resume audio context (for autoplay policy)
   */
  async resume(): Promise<void> {
    if (this.audioContext && this.audioContext.state === 'suspended') {
      await this.audioContext.resume()
      this.emit('resumed')
    }
  }

  /**
   * Suspend audio context
   */
  async suspend(): Promise<void> {
    if (this.audioContext && this.audioContext.state === 'running') {
      await this.audioContext.suspend()
      this.emit('suspended')
    }
  }

  /**
   * Generate audio URL for text (placeholder implementation)
   */
  private generateAudioUrl(text: string): string {
    // This would be replaced with actual Toot speech synthesis
    // For now, return a placeholder URL
    return `/api/speech/synthesize?text=${encodeURIComponent(text)}`
  }

  /**
   * Get configuration
   */
  getConfig(): AudioConfig {
    return { ...this.config }
  }

  /**
   * Update configuration
   */
  updateConfig(updates: Partial<AudioConfig>): void {
    this.config = { ...this.config, ...updates }
    this.emit('configUpdated', this.config)
  }
}

// Export singleton instance
export const tootSpeechService = new TootSpeechService()
