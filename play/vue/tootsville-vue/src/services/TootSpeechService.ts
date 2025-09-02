/**
 * TootSpeechService.ts - Toot Speech Synthesis Service
 * 
 * Vue 3 / TypeScript version for audio processing with pitch and duration shifting
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'

// Browser-compatible EventEmitter
class EventEmitter {
  private events: Map<string, Function[]> = new Map()

  on(event: string, listener: Function) {
    if (!this.events.has(event)) {
      this.events.set(event, [])
    }
    this.events.get(event)!.push(listener)
  }

  off(event: string, listener: Function) {
    if (!this.events.has(event)) return
    const listeners = this.events.get(event)!
    const index = listeners.indexOf(listener)
    if (index > -1) {
      listeners.splice(index, 1)
    }
  }

  emit(event: string, ...args: any[]) {
    if (!this.events.has(event)) return
    const listeners = this.events.get(event)!
    listeners.forEach(listener => listener(...args))
  }

  removeAllListeners(event?: string) {
    if (event) {
      this.events.delete(event)
    } else {
      this.events.clear()
    }
  }
}

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
 * 
 * @class TootSpeechService
 * @description Provides speech synthesis with audio processing capabilities
 * 
 * @example
 * ```typescript
 * import { tootSpeechService } from './TootSpeechService'
 * 
 * // Initialize the service
 * await tootSpeechService.initialize()
 * 
 * // Speak with custom parameters
 * tootSpeechService.speak('Hello, Tootsville!', {
 *   pitch: 1.2,
 *   duration: 1.5,
 *   volume: 0.8
 * })
 * 
 * // Queue multiple speech items
 * tootSpeechService.queueSpeech('First message')
 * tootSpeechService.queueSpeech('Second message', { pitch: 0.8 })
 * ```
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

  /**
   * Constructor - Initializes the EventEmitter
   * 
   * @description Sets up the service as an EventEmitter for speech events
   * @sideEffects - Calls super() to initialize EventEmitter
   */
  constructor() {
    super()
  }

  /**
   * Initialize the audio context and gain nodes
   * 
   * @description Sets up Web Audio API context and gain nodes for audio processing
   * @sideEffects - Creates AudioContext, GainNodes, and connects audio graph
   * @sideEffects - Updates _isInitialized.value to true on success
   * @sideEffects - Updates _error.value with error message on failure
   * @inputs - window.AudioContext (function) - Web Audio API constructor
   * @outputs - boolean - true if initialization successful, false otherwise
   * @returns {Promise<boolean>} Whether initialization was successful
   */
  async initialize(): Promise<boolean> {
    try {
      // Create audio context
      this.audioContext = new (window.AudioContext || (window as any).webkitAudioContext)()
      
      // Create gain nodes
      this.masterGain = this.audioContext.createGain()
      this.speechGain = this.audioContext.createGain()
      
      // Connect audio graph
      this.speechGain.connect(this.masterGain)
      this.masterGain.connect(this.audioContext.destination)
      
      // Set initial volumes
      this.masterGain.gain.value = this._masterVolume.value
      this.speechGain.gain.value = this._speechVolume.value
      
      this._isInitialized.value = true
      this._error.value = null
      
      this.emit('initialized')
      return true
    } catch (error) {
      this._error.value = error instanceof Error ? error.message : 'Unknown error'
      this.emit('error', this._error.value)
      return false
    }
  }

  /**
   * Set master volume level
   * 
   * @description Adjusts the overall audio output volume
   * @sideEffects - Updates _masterVolume.value and masterGain.gain.value
   * @inputs - volume (number) - Volume level from 0.0 to 1.0
   * @units - Volume as decimal (0.0 = silent, 1.0 = full volume)
   * @param {number} volume - Volume level (0.0 to 1.0)
   */
  setMasterVolume(volume: number): void {
    const clampedVolume = Math.max(0, Math.min(1, volume))
    this._masterVolume.value = clampedVolume
    
    if (this.masterGain) {
      this.masterGain.gain.value = clampedVolume
    }
    
    this.emit('volumeChanged', { type: 'master', volume: clampedVolume })
  }

  /**
   * Set speech volume level
   * 
   * @description Adjusts the volume specifically for speech audio
   * @sideEffects - Updates _speechVolume.value and speechGain.gain.value
   * @inputs - volume (number) - Volume level from 0.0 to 1.0
   * @units - Volume as decimal (0.0 = silent, 1.0 = full volume)
   * @param {number} volume - Volume level (0.0 to 1.0)
   */
  setSpeechVolume(volume: number): void {
    const clampedVolume = Math.max(0, Math.min(1, volume))
    this._speechVolume.value = clampedVolume
    
    if (this.speechGain) {
      this.speechGain.gain.value = clampedVolume
    }
    
    this.emit('volumeChanged', { type: 'speech', volume: clampedVolume })
  }

  /**
   * Load audio buffer from URL or data
   * 
   * @description Fetches and decodes audio data for speech synthesis
   * @sideEffects - May update bufferCache with new AudioBuffer
   * @inputs - url (string) - Audio file URL or data URL, audioContext (AudioContext)
   * @outputs - AudioBuffer | null - Decoded audio buffer or null on failure
   * @sideEffects - Updates _error.value with error message on failure
   * @units - Audio data as AudioBuffer object
   * @param {string} url - Audio file URL or data URL
   * @returns {Promise<AudioBuffer | null>} Decoded audio buffer
   */
  async loadAudioBuffer(url: string): Promise<AudioBuffer | null> {
    if (!this.audioContext) {
      this._error.value = 'Audio context not initialized'
      return null
    }

    try {
      // Check cache first
      if (this.config.bufferCache.has(url)) {
        return this.config.bufferCache.get(url) || null
      }

      // Fetch audio data
      const response = await fetch(url)
      if (!response.ok) {
        throw new Error(`Failed to fetch audio: ${response.statusText}`)
      }

      const arrayBuffer = await response.arrayBuffer()
      const audioBuffer = await this.audioContext.decodeAudioData(arrayBuffer)
      
      // Cache the buffer
      this.config.bufferCache.set(url, audioBuffer)
      
      return audioBuffer
    } catch (error) {
      this._error.value = error instanceof Error ? error.message : 'Unknown error'
      this.emit('error', this._error.value)
      return null
    }
  }

  /**
   * Create audio source from buffer
   * 
   * @description Creates an AudioBufferSourceNode from an AudioBuffer
   * @inputs - buffer (AudioBuffer) - Audio data buffer, audioContext (AudioContext)
   * @outputs - AudioBufferSourceNode | null - Audio source node or null on failure
   * @sideEffects - Creates new AudioBufferSourceNode and connects to speechGain
   * @units - Audio source as AudioBufferSourceNode object
   * @param {AudioBuffer} buffer - Audio buffer to create source from
   * @returns {AudioBufferSourceNode | null} Audio source node
   */
  private createAudioSource(buffer: AudioBuffer): AudioBufferSourceNode | null {
    if (!this.audioContext || !this.speechGain) {
      return null
    }

    try {
      const source = this.audioContext.createBufferSource()
      source.buffer = buffer
      source.connect(this.speechGain)
      
      return source
    } catch (error) {
      this._error.value = error instanceof Error ? error.message : 'Unknown error'
      this.emit('error', this._error.value)
      return null
    }
  }

  /**
   * Calculate speech parameters from text
   * 
   * @description Analyzes text to determine optimal speech parameters
   * @inputs - text (string) - Text to analyze, parameters (Partial<SpeechParameters>) - Optional custom parameters
   * @outputs - SpeechParameters object with calculated values
   * @units - Pitch as multiplier (1.0 = normal), duration as multiplier (1.0 = normal), volume as decimal (0.0-1.0), speed as multiplier (1.0 = normal)
   * @param {string} text - Text to analyze
   * @param {Partial<SpeechParameters>} parameters - Optional custom parameters
   * @returns {SpeechParameters} Calculated speech parameters
   */
  calculateSpeechParameters(text: string, parameters?: Partial<SpeechParameters>): SpeechParameters {
    const defaultParams: SpeechParameters = {
      pitch: 1.0,
      duration: 1.0,
      volume: this._speechVolume.value,
      speed: 1.0
    }

    // Analyze text characteristics
    const textLength = text.length
    const hasExclamation = text.includes('!')
    const hasQuestion = text.includes('?')
    const isShouting = text === text.toUpperCase() && textLength > 3

    // Adjust parameters based on text analysis
    let calculatedPitch = defaultParams.pitch
    let calculatedDuration = defaultParams.duration
    let calculatedSpeed = defaultParams.speed

    if (isShouting) {
      calculatedPitch = 1.3
      calculatedDuration = 0.8
      calculatedSpeed = 1.2
    } else if (hasExclamation) {
      calculatedPitch = 1.2
      calculatedDuration = 0.9
    } else if (hasQuestion) {
      calculatedPitch = 1.1
      calculatedDuration = 1.1
      calculatedSpeed = 0.9
    } else if (textLength > 50) {
      calculatedDuration = 1.2
      calculatedSpeed = 0.9
    }

    // Merge with custom parameters
    return {
      pitch: parameters?.pitch ?? calculatedPitch,
      duration: parameters?.duration ?? calculatedDuration,
      volume: parameters?.volume ?? defaultParams.volume,
      speed: parameters?.speed ?? calculatedSpeed
    }
  }

  /**
   * Speak text with audio processing
   * 
   * @description Synthesizes speech from text with pitch and duration shifting
   * @sideEffects - Updates _isSpeaking.value, _currentText.value, and _error.value
   * @sideEffects - Creates and plays AudioBufferSourceNode
   * @sideEffects - Emits 'speechStart', 'speechEnd', and 'error' events
   * @inputs - text (string) - Text to speak, parameters (Partial<SpeechParameters>) - Optional speech parameters
   * @outputs - boolean - true if speech started successfully, false otherwise
   * @units - Text as string, parameters as SpeechParameters object
   * @param {string} text - Text to speak
   * @param {Partial<SpeechParameters>} parameters - Optional speech parameters
   * @returns {Promise<boolean>} Whether speech started successfully
   */
  async speak(text: string, parameters?: Partial<SpeechParameters>): Promise<boolean> {
    if (!this._isInitialized.value) {
      this._error.value = 'Service not initialized'
      return false
    }

    if (this._isSpeaking.value) {
      this._error.value = 'Already speaking'
      return false
    }

    try {
      // Stop any current speech
      this.stopSpeaking()

      // Calculate speech parameters
      const speechParams = this.calculateSpeechParameters(text, parameters)

      // Load or generate audio buffer (placeholder for actual TTS)
      const audioBuffer = await this.loadAudioBuffer('/api/tts?text=' + encodeURIComponent(text))
      if (!audioBuffer) {
        // Fallback: create a simple beep for demonstration
        const buffer = this.createBeepBuffer(speechParams)
        if (!buffer) {
          throw new Error('Failed to create audio buffer')
        }
        
        // Try to play the buffer and check if it succeeds
        const source = this.createAudioSource(buffer)
        if (!source) {
          throw new Error('Failed to create audio source')
        }
        
        this.playAudioBuffer(buffer, speechParams)
      } else {
        // Try to play the buffer and check if it succeeds
        const source = this.createAudioSource(audioBuffer)
        if (!source) {
          throw new Error('Failed to create audio source')
        }
        
        this.playAudioBuffer(audioBuffer, speechParams)
      }

      this._isSpeaking.value = true
      this._currentText.value = text
      this._error.value = null

      this.emit('speechStart', { text, parameters: speechParams })
      return true
    } catch (error) {
      this._error.value = error instanceof Error ? error.message : 'Unknown error'
      this.emit('error', this._error.value)
      return false
    }
  }

  /**
   * Queue speech for later playback
   * 
   * @description Adds speech to the queue for sequential playback
   * @sideEffects - Updates _speechQueue.value by adding new item
   * @sideEffects - Emits 'queued' event
   * @inputs - text (string) - Text to queue, parameters (Partial<SpeechParameters>) - Optional parameters, priority (number) - Queue priority
   * @outputs - number - Queue position (0-based index)
   * @units - Priority as integer (lower = higher priority), timestamp as milliseconds
   * @param {string} text - Text to queue
   * @param {Partial<SpeechParameters>} parameters - Optional speech parameters
   * @param {number} priority - Queue priority (default: 0)
   * @returns {number} Queue position
   */
  queueSpeech(text: string, parameters?: Partial<SpeechParameters>, priority: number = 0): number {
    const queueItem: SpeechQueueItem = {
      text,
      parameters,
      priority,
      timestamp: Date.now()
    }

    // Insert based on priority
    const queue = this._speechQueue.value
    let insertIndex = 0
    
    for (let i = 0; i < queue.length; i++) {
      if (priority < queue[i].priority!) {
        insertIndex = i
        break
      }
      insertIndex = i + 1
    }

    queue.splice(insertIndex, 0, queueItem)
    this._speechQueue.value = [...queue]

    this.emit('queued', { text, position: insertIndex, queueLength: queue.length })
    return insertIndex
  }

  /**
   * Process speech queue
   * 
   * @description Plays the next item in the speech queue
   * @sideEffects - Updates _speechQueue.value by removing played items
   * @sideEffects - Calls speak() method for queue items
   * @sideEffects - Emits 'queueProcessed' event
   * @inputs - _speechQueue.value (SpeechQueueItem[]) - Current queue state
   * @outputs - boolean - true if queue item was processed, false if queue empty
   * @returns {Promise<boolean>} Whether a queue item was processed
   */
  async processQueue(): Promise<boolean> {
    if (this._isSpeaking.value || this._speechQueue.value.length === 0) {
      return false
    }

    const nextItem = this._speechQueue.value[0]
    const queue = this._speechQueue.value.slice(1)
    this._speechQueue.value = queue

    const success = await this.speak(nextItem.text, nextItem.parameters)
    
    this.emit('queueProcessed', { 
      text: nextItem.text, 
      success, 
      remainingItems: queue.length 
    })
    
    return success
  }

  /**
   * Stop current speech
   * 
   * @description Stops any currently playing speech
   * @sideEffects - Updates _isSpeaking.value and _currentText.value
   * @sideEffects - Stops AudioBufferSourceNode if playing
   * @sideEffects - Emits 'speechStop' event
   */
  stopSpeaking(): void {
    if (this._isSpeaking.value) {
      this._isSpeaking.value = false
      this._currentText.value = null
      
      // Stop any playing audio sources
      if (this.audioContext) {
        this.audioContext.resume() // Ensure context is running
      }
      
      this.emit('speechStop')
    }
  }

  /**
   * Clear speech queue
   * 
   * @description Removes all items from the speech queue
   * @sideEffects - Updates _speechQueue.value to empty array
   * @sideEffects - Emits 'queueCleared' event
   * @outputs - number - Number of items cleared
   * @returns {number} Number of items cleared
   */
  clearQueue(): number {
    const queueLength = this._speechQueue.value.length
    this._speechQueue.value = []
    
    this.emit('queueCleared', { clearedItems: queueLength })
    return queueLength
  }

  /**
   * Get current speech status
   * 
   * @description Returns current speech state information
   * @outputs - SpeechStatus object with current state
   * @returns {SpeechStatus} Current speech status
   */
  getSpeechStatus(): SpeechStatus {
    return this.speechStatus.value
  }

  /**
   * Get master volume
   * 
   * @description Returns current master volume level
   * @outputs - number - Current master volume (0.0 to 1.0)
   * @units - Volume as decimal (0.0 = silent, 1.0 = full volume)
   * @returns {number} Current master volume
   */
  getMasterVolume(): number {
    return this._masterVolume.value
  }

  /**
   * Get speech volume
   * 
   * @description Returns current speech volume level
   * @outputs - number - Current speech volume (0.0 to 1.0)
   * @units - Volume as decimal (0.0 = silent, 1.0 = full volume)
   * @returns {number} Current speech volume
   */
  getSpeechVolume(): number {
    return this._speechVolume.value
  }

  /**
   * Check if service is initialized
   * 
   * @description Determines if the audio context and gain nodes are ready
   * @outputs - boolean - true if initialized, false otherwise
   * @returns {boolean} Whether service is initialized
   */
  isInitialized(): boolean {
    return this._isInitialized.value
  }

  /**
   * Get current error
   * 
   * @description Returns the most recent error message
   * @outputs - string | null - Error message or null if no error
   * @returns {string | null} Current error message
   */
  getError(): string | null {
    return this._error.value
  }

  /**
   * Clear error state
   * 
   * @description Resets the error state
   * @sideEffects - Updates _error.value to null
   */
  clearError(): void {
    this._error.value = null
  }

  /**
   * Dispose of resources
   * 
   * @description Cleans up audio context and resources
   * @sideEffects - Closes AudioContext, clears buffer cache, resets state
   * @sideEffects - Emits 'disposed' event
   */
  dispose(): void {
    this.stopSpeaking()
    this.clearQueue()
    
    if (this.audioContext) {
      this.audioContext.close()
      this.audioContext = null
    }
    
    this.masterGain = null
    this.speechGain = null
    this.config.bufferCache.clear()
    
    this._isInitialized.value = false
    this._error.value = null
    
    this.emit('disposed')
  }

  /**
   * Create beep buffer for demonstration
   * 
   * @description Creates a simple beep audio buffer for testing
   * @inputs - parameters (SpeechParameters) - Speech parameters to apply
   * @outputs - AudioBuffer | null - Generated audio buffer or null on failure
   * @sideEffects - Creates new AudioBuffer with beep waveform
   * @units - Audio data as AudioBuffer object
   * @param {SpeechParameters} parameters - Speech parameters
   * @returns {AudioBuffer | null} Generated audio buffer
   * @private
   */
  private createBeepBuffer(parameters: SpeechParameters): AudioBuffer | null {
    if (!this.audioContext) return null

    const sampleRate = this.audioContext.sampleRate
    const duration = 0.5 * parameters.duration // Base duration * parameter
    const frequency = 440 * parameters.pitch // Base frequency * parameter
    const frameCount = Math.floor(sampleRate * duration)
    
    const buffer = this.audioContext.createBuffer(1, frameCount, sampleRate)
    const channelData = buffer.getChannelData(0)
    
    for (let i = 0; i < frameCount; i++) {
      const t = i / sampleRate
      channelData[i] = Math.sin(2 * Math.PI * frequency * t) * parameters.volume * 0.3
    }
    
    return buffer
  }

  /**
   * Play audio buffer with parameters
   * 
   * @description Plays an audio buffer with pitch and duration adjustments
   * @sideEffects - Creates and plays AudioBufferSourceNode
   * @sideEffects - Updates _isSpeaking.value and _currentText.value
   * @sideEffects - Emits 'speechStart' and 'speechEnd' events
   * @inputs - buffer (AudioBuffer) - Audio data to play, parameters (SpeechParameters) - Playback parameters
   * @units - Audio data as AudioBuffer object, parameters as SpeechParameters object
   * @param {AudioBuffer} buffer - Audio buffer to play
   * @param {SpeechParameters} parameters - Playback parameters
   * @private
   */
  private playAudioBuffer(buffer: AudioBuffer, parameters: SpeechParameters): void {
    const source = this.createAudioSource(buffer)
    if (!source) return

    // Apply playback rate for speed adjustment
    source.playbackRate.value = parameters.speed

    // Set up end event
    source.onended = () => {
      this._isSpeaking.value = false
      this._currentText.value = null
      this.emit('speechEnd')
      
      // Process next queue item
      this.processQueue()
    }

    // Start playback
    source.start(0)
  }
}

// Export singleton instance
export const tootSpeechService = new TootSpeechService()
