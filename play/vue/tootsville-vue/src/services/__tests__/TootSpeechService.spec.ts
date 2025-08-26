import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { TootSpeechService } from '../TootSpeechService'

// Mock Web Audio API
const mockAudioContext = {
  createGain: vi.fn(() => ({
    connect: vi.fn(),
    gain: { value: 0.5 }
  })),
  createBufferSource: vi.fn(() => ({
    buffer: null,
    connect: vi.fn(),
    start: vi.fn(),
    onended: null,
    playbackRate: { value: 1.0 }
  })),
  createBuffer: vi.fn(() => ({
    duration: 0.5,
    length: 22050,
    numberOfChannels: 1,
    sampleRate: 44100,
    getChannelData: vi.fn(() => new Float32Array(22050))
  })),
  decodeAudioData: vi.fn(),
  close: vi.fn(),
  resume: vi.fn(),
  destination: {},
  sampleRate: 44100
}

const mockGainNode = {
  connect: vi.fn(),
  gain: { value: 0.5 }
}

const mockAudioBuffer = {
  duration: 1.0,
  length: 44100,
  numberOfChannels: 1,
  sampleRate: 44100,
  getChannelData: vi.fn(() => new Float32Array(44100))
}

const mockAudioSource = {
  buffer: null,
  connect: vi.fn(),
  start: vi.fn(),
  onended: null,
  playbackRate: { value: 1.0 }
}

// Mock fetch
global.fetch = vi.fn()

// Mock window.AudioContext
Object.defineProperty(window, 'AudioContext', {
  value: vi.fn(() => mockAudioContext),
  writable: true
})

describe('TootSpeechService', () => {
  let service: TootSpeechService

  beforeEach(() => {
    service = new TootSpeechService()
    vi.clearAllMocks()
    
    // Reset mock implementations
    mockAudioContext.createGain.mockReturnValue(mockGainNode)
    mockAudioContext.createBufferSource.mockReturnValue(mockAudioSource)
    mockAudioContext.decodeAudioData.mockResolvedValue(mockAudioBuffer)
    mockAudioContext.createBuffer.mockReturnValue(mockAudioBuffer)
    ;(global.fetch as any).mockResolvedValue({
      ok: true,
      arrayBuffer: () => Promise.resolve(new ArrayBuffer(1024))
    })
  })

  afterEach(() => {
    service.dispose()
  })

  describe('Constructor', () => {
    it('should initialize as EventEmitter', () => {
      expect(service).toBeInstanceOf(TootSpeechService)
      expect(typeof service.on).toBe('function')
      expect(typeof service.emit).toBe('function')
    })

    it('should have initial reactive state', () => {
      expect(service.speechStatus.value.isSpeaking).toBe(false)
      expect(service.speechStatus.value.currentText).toBe(null)
      expect(service.speechStatus.value.queueLength).toBe(0)
      expect(service.speechStatus.value.isInitialized).toBe(false)
      expect(service.speechStatus.value.error).toBe(null)
    })
  })

  describe('Initialization', () => {
    it('should initialize audio context and gain nodes', async () => {
      const result = await service.initialize()
      
      expect(result).toBe(true)
      expect(service.isInitialized()).toBe(true)
      expect(service.getError()).toBe(null)
      expect(mockAudioContext.createGain).toHaveBeenCalledTimes(2)
      expect(mockGainNode.connect).toHaveBeenCalled()
    })

    it('should handle initialization errors', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      mockAudioContext.createGain.mockImplementation(() => {
        throw new Error('Audio context error')
      })

      const result = await service.initialize()
      
      expect(result).toBe(false)
      expect(service.isInitialized()).toBe(false)
      expect(service.getError()).toBe('Audio context error')
      expect(errorSpy).toHaveBeenCalledWith('Audio context error')
    })

    it('should emit initialized event on success', async () => {
      const eventSpy = vi.fn()
      service.on('initialized', eventSpy)
      
      await service.initialize()
      
      expect(eventSpy).toHaveBeenCalled()
    })

    it('should emit error event on failure', async () => {
      const eventSpy = vi.fn()
      service.on('error', eventSpy)
      
      mockAudioContext.createGain.mockImplementation(() => {
        throw new Error('Audio context error')
      })
      
      await service.initialize()
      
      expect(eventSpy).toHaveBeenCalledWith('Audio context error')
    })
  })

  describe('Volume Control', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should set master volume correctly', () => {
      service.setMasterVolume(0.8)
      
      expect(service.getMasterVolume()).toBe(0.8)
      expect(mockGainNode.gain.value).toBe(0.8)
    })

    it('should clamp master volume to valid range', () => {
      service.setMasterVolume(1.5) // Above max
      expect(service.getMasterVolume()).toBe(1.0)
      
      service.setMasterVolume(-0.5) // Below min
      expect(service.getMasterVolume()).toBe(0.0)
    })

    it('should set speech volume correctly', () => {
      service.setSpeechVolume(0.6)
      
      expect(service.getSpeechVolume()).toBe(0.6)
      expect(mockGainNode.gain.value).toBe(0.6)
    })

    it('should clamp speech volume to valid range', () => {
      service.setSpeechVolume(1.2) // Above max
      expect(service.getSpeechVolume()).toBe(1.0)
      
      service.setSpeechVolume(-0.1) // Below min
      expect(service.getSpeechVolume()).toBe(0.0)
    })

    it('should emit volumeChanged events', () => {
      const eventSpy = vi.fn()
      service.on('volumeChanged', eventSpy)
      
      service.setMasterVolume(0.7)
      expect(eventSpy).toHaveBeenCalledWith({ type: 'master', volume: 0.7 })
      
      service.setSpeechVolume(0.8)
      expect(eventSpy).toHaveBeenCalledWith({ type: 'speech', volume: 0.8 })
    })
  })

  describe('Audio Buffer Loading', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should load audio buffer from URL', async () => {
      const buffer = await service.loadAudioBuffer('/test/audio.wav')
      
      expect(buffer).toBe(mockAudioBuffer)
      expect(global.fetch).toHaveBeenCalledWith('/test/audio.wav')
      expect(mockAudioContext.decodeAudioData).toHaveBeenCalled()
    })

    it('should cache loaded buffers', async () => {
      const buffer1 = await service.loadAudioBuffer('/test/audio.wav')
      const buffer2 = await service.loadAudioBuffer('/test/audio.wav')
      
      expect(buffer1).toBe(buffer2)
      expect(global.fetch).toHaveBeenCalledTimes(1) // Only called once due to caching
    })

    it('should handle fetch errors', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      ;(global.fetch as any).mockRejectedValue(new Error('Network error'))
      
      const buffer = await service.loadAudioBuffer('/test/audio.wav')
      
      expect(buffer).toBe(null)
      expect(service.getError()).toBe('Network error')
      expect(errorSpy).toHaveBeenCalledWith('Network error')
    })

    it('should handle non-ok responses', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      ;(global.fetch as any).mockResolvedValue({
        ok: false,
        statusText: 'Not Found'
      })
      
      const buffer = await service.loadAudioBuffer('/test/audio.wav')
      
      expect(buffer).toBe(null)
      expect(service.getError()).toBe('Failed to fetch audio: Not Found')
      expect(errorSpy).toHaveBeenCalledWith('Failed to fetch audio: Not Found')
    })

    it('should handle decode errors', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      mockAudioContext.decodeAudioData.mockRejectedValue(new Error('Invalid audio data'))
      
      const buffer = await service.loadAudioBuffer('/test/audio.wav')
      
      expect(buffer).toBe(null)
      expect(service.getError()).toBe('Invalid audio data')
      expect(errorSpy).toHaveBeenCalledWith('Invalid audio data')
    })
  })

  describe('Speech Parameter Calculation', () => {
    it('should calculate parameters for normal text', () => {
      const params = service.calculateSpeechParameters('Hello world')
      
      expect(params.pitch).toBe(1.0)
      expect(params.duration).toBe(1.0)
      expect(params.speed).toBe(1.0)
      expect(params.volume).toBe(0.5) // Default speech volume
    })

    it('should adjust parameters for exclamations', () => {
      const params = service.calculateSpeechParameters('Hello world!')
      
      expect(params.pitch).toBe(1.2)
      expect(params.duration).toBe(0.9)
      expect(params.speed).toBe(1.0)
    })

    it('should adjust parameters for questions', () => {
      const params = service.calculateSpeechParameters('Hello world?')
      
      expect(params.pitch).toBe(1.1)
      expect(params.duration).toBe(1.1)
      expect(params.speed).toBe(0.9)
    })

    it('should adjust parameters for shouting', () => {
      const params = service.calculateSpeechParameters('HELLO WORLD')
      
      expect(params.pitch).toBe(1.3)
      expect(params.duration).toBe(0.8)
      expect(params.speed).toBe(1.2)
    })

    it('should adjust parameters for long text', () => {
      const longText = 'This is a very long text that should trigger different parameters for speech synthesis and duration calculation'
      const params = service.calculateSpeechParameters(longText)
      
      expect(params.duration).toBe(1.2)
      expect(params.speed).toBe(0.9)
    })

    it('should merge with custom parameters', () => {
      const customParams = { pitch: 1.5, volume: 0.8 }
      const params = service.calculateSpeechParameters('Hello world!', customParams)
      
      expect(params.pitch).toBe(1.5) // Custom overrides calculated
      expect(params.volume).toBe(0.8) // Custom overrides default
      expect(params.duration).toBe(0.9) // Calculated value
    })
  })

  describe('Speech Synthesis', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should speak text successfully', async () => {
      const result = await service.speak('Hello world')
      
      expect(result).toBe(true)
      expect(service.isSpeaking.value).toBe(true)
      expect(service.speechStatus.value.currentText).toBe('Hello world')
      expect(service.getError()).toBe(null)
    })

    it('should handle speech when not initialized', async () => {
      const uninitializedService = new TootSpeechService()
      const result = await uninitializedService.speak('Hello world')
      
      expect(result).toBe(false)
      expect(uninitializedService.getError()).toBe('Service not initialized')
    })

    it('should handle speech when already speaking', async () => {
      await service.speak('First message')
      const result = await service.speak('Second message')
      
      expect(result).toBe(false)
      expect(service.getError()).toBe('Already speaking')
    })

    it('should emit speech events', async () => {
      const startSpy = vi.fn()
      const endSpy = vi.fn()
      service.on('speechStart', startSpy)
      service.on('speechEnd', endSpy)
      
      await service.speak('Hello world')
      
      expect(startSpy).toHaveBeenCalledWith({
        text: 'Hello world',
        parameters: expect.objectContaining({
          pitch: 1.0,
          duration: 1.0,
          speed: 1.0
        })
      })
      
      // Wait for speech to end (simulated by onended callback)
      mockAudioSource.onended?.()
      
      expect(endSpy).toHaveBeenCalled()
    })

    it('should handle audio buffer creation errors', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      mockAudioContext.createBufferSource.mockReturnValue(null)
      
      const result = await service.speak('Hello world')
      
      expect(result).toBe(false)
      expect(service.getError()).toBe('Failed to create audio source')
      expect(errorSpy).toHaveBeenCalledWith('Failed to create audio source')
    })
  })

  describe('Speech Queue', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should queue speech items', () => {
      const position1 = service.queueSpeech('First message')
      const position2 = service.queueSpeech('Second message')
      
      expect(position1).toBe(0)
      expect(position2).toBe(1)
      expect(service.queueLength.value).toBe(2)
    })

    it('should handle priority in queue', () => {
      service.queueSpeech('Low priority', {}, 2)
      service.queueSpeech('High priority', {}, 0)
      service.queueSpeech('Medium priority', {}, 1)
      
      expect(service.queueLength.value).toBe(3)
    })

    it('should emit queued events', () => {
      const eventSpy = vi.fn()
      service.on('queued', eventSpy)
      
      service.queueSpeech('Test message')
      
      expect(eventSpy).toHaveBeenCalledWith({
        text: 'Test message',
        position: 0,
        queueLength: 1
      })
    })

    it('should process queue items', async () => {
      service.queueSpeech('First message')
      service.queueSpeech('Second message')
      
      const result1 = await service.processQueue()
      expect(result1).toBe(true)
      expect(service.queueLength.value).toBe(1)
      expect(service.isSpeaking.value).toBe(true)
      
      // The second item should not be processed until the first one ends
      const result2 = await service.processQueue()
      expect(result2).toBe(false) // Should return false because still speaking
      expect(service.queueLength.value).toBe(1) // Should still have 1 item
    })

    it('should not process queue when speaking', async () => {
      await service.speak('Current speech')
      service.queueSpeech('Queued message')
      
      const result = await service.processQueue()
      
      expect(result).toBe(false)
      expect(service.queueLength.value).toBe(1)
    })

    it('should not process empty queue', async () => {
      const result = await service.processQueue()
      
      expect(result).toBe(false)
    })

    it('should emit queueProcessed events', async () => {
      const eventSpy = vi.fn()
      service.on('queueProcessed', eventSpy)
      
      service.queueSpeech('Test message')
      await service.processQueue()
      
      expect(eventSpy).toHaveBeenCalledWith({
        text: 'Test message',
        success: true,
        remainingItems: 0
      })
    })

    it('should clear queue', () => {
      service.queueSpeech('Message 1')
      service.queueSpeech('Message 2')
      
      const clearedCount = service.clearQueue()
      
      expect(clearedCount).toBe(2)
      expect(service.queueLength.value).toBe(0)
    })

    it('should emit queueCleared events', () => {
      const eventSpy = vi.fn()
      service.on('queueCleared', eventSpy)
      
      service.queueSpeech('Message 1')
      service.queueSpeech('Message 2')
      service.clearQueue()
      
      expect(eventSpy).toHaveBeenCalledWith({ clearedItems: 2 })
    })
  })

  describe('Speech Control', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should stop speaking', async () => {
      await service.speak('Hello world')
      expect(service.isSpeaking.value).toBe(true)
      
      service.stopSpeaking()
      
      expect(service.isSpeaking.value).toBe(false)
      expect(service.speechStatus.value.currentText).toBe(null)
    })

    it('should emit speechStop events', async () => {
      const eventSpy = vi.fn()
      service.on('speechStop', eventSpy)
      
      await service.speak('Test message')
      service.stopSpeaking()
      
      expect(eventSpy).toHaveBeenCalled()
    })

    it('should not stop when not speaking', () => {
      const eventSpy = vi.fn()
      service.on('speechStop', eventSpy)
      
      service.stopSpeaking()
      
      expect(eventSpy).not.toHaveBeenCalled()
    })
  })

  describe('Status and Error Handling', () => {
    it('should return speech status', () => {
      const status = service.getSpeechStatus()
      
      expect(status).toEqual({
        isSpeaking: false,
        currentText: null,
        queueLength: 0,
        isInitialized: false,
        error: null
      })
    })

    it('should return current error', () => {
      expect(service.getError()).toBe(null)
    })

    it('should clear error state', () => {
      // Simulate an error
      service['_error'].value = 'Test error'
      
      service.clearError()
      
      expect(service.getError()).toBe(null)
    })

    it('should check initialization status', () => {
      expect(service.isInitialized()).toBe(false)
      
      // Simulate initialization
      service['_isInitialized'].value = true
      
      expect(service.isInitialized()).toBe(true)
    })
  })

  describe('Resource Management', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should dispose of resources', () => {
      const eventSpy = vi.fn()
      service.on('disposed', eventSpy)
      
      service.dispose()
      
      expect(mockAudioContext.close).toHaveBeenCalled()
      expect(service.isInitialized()).toBe(false)
      expect(service.getError()).toBe(null)
      expect(eventSpy).toHaveBeenCalled()
    })

    it('should stop speaking and clear queue on dispose', async () => {
      await service.speak('Test message')
      service.queueSpeech('Queued message')
      
      service.dispose()
      
      expect(service.isSpeaking.value).toBe(false)
      expect(service.queueLength.value).toBe(0)
    })
  })

  describe('Audio Buffer Creation', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should create beep buffer with parameters', () => {
      const params = { pitch: 1.2, duration: 1.5, volume: 0.8, speed: 1.0 }
      const buffer = service['createBeepBuffer'](params)
      
      expect(buffer).toBe(mockAudioBuffer)
      expect(buffer?.duration).toBe(1.0) // Mock buffer duration
    })

    it('should return null when audio context not available', () => {
      service['audioContext'] = null
      
      const params = { pitch: 1.0, duration: 1.0, volume: 0.5, speed: 1.0 }
      const buffer = service['createBeepBuffer'](params)
      
      expect(buffer).toBe(null)
    })
  })

  describe('Audio Buffer Playback', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should play audio buffer with parameters', () => {
      const params = { pitch: 1.2, duration: 1.0, volume: 0.8, speed: 1.5 }
      
      service['playAudioBuffer'](mockAudioBuffer, params)
      
      expect(mockAudioSource.playbackRate.value).toBe(1.5)
      expect(mockAudioSource.start).toHaveBeenCalledWith(0)
    })

    it('should handle audio source creation failure', () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      mockAudioContext.createBufferSource.mockImplementation(() => {
        throw new Error('Failed to create audio source')
      })
      
      const params = { pitch: 1.0, duration: 1.0, volume: 0.5, speed: 1.0 }
      
      // Should handle source creation failure gracefully
      service['playAudioBuffer'](mockAudioBuffer, params)
      
      // The method should return early without throwing when source creation fails
      expect(mockAudioSource.start).not.toHaveBeenCalled()
      expect(errorSpy).toHaveBeenCalledWith('Failed to create audio source')
    })

    it('should set up ended callback', () => {
      const params = { pitch: 1.0, duration: 1.0, volume: 0.5, speed: 1.0 }
      
      service['playAudioBuffer'](mockAudioBuffer, params)
      
      expect(mockAudioSource.onended).toBeInstanceOf(Function)
    })

    it('should process queue after speech ends', () => {
      const processSpy = vi.spyOn(service, 'processQueue')
      const params = { pitch: 1.0, duration: 1.0, volume: 0.5, speed: 1.0 }
      
      service['playAudioBuffer'](mockAudioBuffer, params)
      
      // Trigger the ended callback
      mockAudioSource.onended?.()
      
      expect(processSpy).toHaveBeenCalled()
    })
  })

  describe('Integration Tests', () => {
    beforeEach(async () => {
      await service.initialize()
    })

    it('should handle complete speech workflow', async () => {
      // Queue multiple messages
      service.queueSpeech('First message')
      service.queueSpeech('Second message', { pitch: 1.2 })
      service.queueSpeech('Third message', { volume: 0.8 }, 1)
      
      expect(service.queueLength.value).toBe(3)
      
      // Process queue
      const result1 = await service.processQueue()
      expect(result1).toBe(true)
      expect(service.isSpeaking.value).toBe(true)
      
      // Simulate speech ending
      mockAudioSource.onended?.()
      
      expect(service.isSpeaking.value).toBe(false)
      expect(service.queueLength.value).toBe(1) // Second message should be processed
    })

    it('should handle volume changes during speech', () => {
      service.setMasterVolume(0.6)
      service.setSpeechVolume(0.7)
      
      expect(service.getMasterVolume()).toBe(0.6)
      expect(service.getSpeechVolume()).toBe(0.7)
    })

    it('should handle errors gracefully', async () => {
      const errorSpy = vi.fn()
      service.on('error', errorSpy)
      
      // Test initialization error
      const uninitializedService = new TootSpeechService()
      const initResult = await uninitializedService.speak('Test')
      expect(initResult).toBe(false)
      
      // Test audio loading error
      ;(global.fetch as any).mockRejectedValue(new Error('Network error'))
      const loadResult = await service.loadAudioBuffer('/invalid/url')
      expect(loadResult).toBe(null)
      expect(service.getError()).toBe('Network error')
      expect(errorSpy).toHaveBeenCalledWith('Network error')
    })
  })
})
