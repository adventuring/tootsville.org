import { describe, it, expect, beforeEach, vi } from 'vitest'
import { mount } from '@vue/test-utils'
import { ref } from 'vue'
import TootSpeechPanel from '../TootSpeechPanel.vue'

// Mock the composable
vi.mock('@/composables/useTootSpeech', () => ({
  useTootSpeech: vi.fn(() => ({
    isSpeaking: ref(false),
    speechStatus: ref({
      isSpeaking: false,
      currentText: null,
      queueLength: 0,
      isInitialized: false,
      error: null
    }),
    isInitialized: ref(false),
    error: ref(null),
    canSpeak: ref(true),
    queueLength: ref(0),
    speak: vi.fn(),
    stopSpeaking: vi.fn(),
    queueSpeech: vi.fn(),
    clearSpeechQueue: vi.fn(),
    setVolume: vi.fn(),
    setMasterVolume: vi.fn(),
    resume: vi.fn(),
    suspend: vi.fn(),
    initialize: vi.fn(),
    dispose: vi.fn()
  })),
  useTootSpeechWithVolume: vi.fn(() => ({
    isSpeaking: ref(false),
    speechStatus: ref({
      isSpeaking: false,
      currentText: null,
      queueLength: 0,
      isInitialized: false,
      error: null
    }),
    isInitialized: ref(false),
    error: ref(null),
    canSpeak: ref(true),
    queueLength: ref(0),
    volume: ref(0.5),
    speak: vi.fn(),
    stopSpeaking: vi.fn(),
    queueSpeech: vi.fn(),
    clearSpeechQueue: vi.fn(),
    setVolume: vi.fn(),
    setMasterVolume: vi.fn(),
    resume: vi.fn(),
    suspend: vi.fn(),
    initialize: vi.fn(),
    dispose: vi.fn()
  }))
}))

describe('TootSpeechPanel', () => {
  const mockUseTootSpeechWithVolume = {
    speak: vi.fn(),
    stopSpeaking: vi.fn(),
    queueSpeech: vi.fn(),
    setVolume: vi.fn(),
    setMasterVolume: vi.fn(),
    resume: vi.fn(),
    suspend: vi.fn(),
    initialize: vi.fn(),
    dispose: vi.fn(),
    isSpeaking: { value: false },
    isInitialized: { value: true },
    volume: { value: 0.5 },
    error: { value: null },
    canSpeak: { value: true },
    queueLength: { value: 0 }
  }

  beforeEach(() => {
    vi.clearAllMocks()
    // The mock is already set up in the vi.mock above
  })

  describe('Component Rendering', () => {
    it('should render the speech panel with all controls', () => {
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.find('.toot-speech-panel').exists()).toBe(true)
      expect(wrapper.find('input[type="text"]').exists()).toBe(true)
      expect(wrapper.find('button').exists()).toBe(true)
      expect(wrapper.find('.volume-controls').exists()).toBe(true)
      expect(wrapper.find('.preset-phrases').exists()).toBe(true)
    })

    it('should display the component title', () => {
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Toot Speech Panel')
    })

    it('should show speech input field with placeholder', () => {
      const wrapper = mount(TootSpeechPanel)
      const input = wrapper.find('input[type="text"]')
      
      expect(input.attributes('placeholder')).toBe('Enter text to speak...')
    })
  })

  describe('Speech Controls', () => {
    it('should call speak when speak button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const input = wrapper.find('input[type="text"]')
      const speakButton = wrapper.find('button[data-testid="speak-button"]')
      
      await input.setValue('Hello Tootsville!')
      await speakButton.trigger('click')
      
      expect(mockUseTootSpeech.speak).toHaveBeenCalledWith('Hello Tootsville!')
    })

    it('should call stopSpeaking when stop button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const stopButton = wrapper.find('button[data-testid="stop-button"]')
      
      await stopButton.trigger('click')
      
      expect(mockUseTootSpeech.stopSpeaking).toHaveBeenCalled()
    })

    it('should call queueSpeech when queue button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const input = wrapper.find('input[type="text"]')
      const queueButton = wrapper.find('button[data-testid="queue-button"]')
      
      await input.setValue('Queued message')
      await queueButton.trigger('click')
      
      expect(mockUseTootSpeech.queueSpeech).toHaveBeenCalledWith('Queued message')
    })
  })

  describe('Volume Controls', () => {
    it('should call setMasterVolume when master volume slider changes', async () => {
      const wrapper = mount(TootSpeechPanel)
      const masterVolumeSlider = wrapper.find('input[data-testid="master-volume"]')
      
      await masterVolumeSlider.setValue(0.5)
      
      expect(mockUseTootSpeech.setMasterVolume).toHaveBeenCalledWith(0.5)
    })

    it('should call setVolume when speech volume slider changes', async () => {
      const wrapper = mount(TootSpeechPanel)
      const speechVolumeSlider = wrapper.find('input[data-testid="speech-volume"]')
      
      await speechVolumeSlider.setValue(0.7)
      
      expect(mockUseTootSpeech.setVolume).toHaveBeenCalledWith(0.7)
    })

    it('should display current volume values', () => {
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('80%') // masterVolume.value * 100
      expect(wrapper.text()).toContain('90%') // speechVolume.value * 100
    })
  })

  describe('Preset Phrases', () => {
    it('should render preset phrase buttons', () => {
      const wrapper = mount(TootSpeechPanel)
      const presetButtons = wrapper.findAll('button[data-testid^="preset-"]')
      
      expect(presetButtons.length).toBeGreaterThan(0)
    })

    it('should call speak with preset phrase when clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const presetButton = wrapper.find('button[data-testid="preset-hello"]')
      
      await presetButton.trigger('click')
      
      expect(mockUseTootSpeech.speak).toHaveBeenCalledWith('Hello!')
    })
  })

  describe('Speech History', () => {
    it('should display speech history when available', () => {
      mockUseTootSpeech.speechHistory.value = [
        { text: 'Hello!', timestamp: Date.now() },
        { text: 'Goodbye!', timestamp: Date.now() }
      ]
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Hello!')
      expect(wrapper.text()).toContain('Goodbye!')
    })

    it('should show "No speech history" when empty', () => {
      mockUseTootSpeech.speechHistory.value = []
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('No speech history')
    })
  })

  describe('Status Indicators', () => {
    it('should show speaking status when isSpeaking is true', () => {
      mockUseTootSpeech.isSpeaking.value = true
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Speaking')
    })

    it('should show not speaking status when isSpeaking is false', () => {
      mockUseTootSpeech.isSpeaking.value = false
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Not speaking')
    })

    it('should show initialization status', () => {
      mockUseTootSpeech.isInitialized.value = false
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Initializing')
    })

    it('should show error message when error exists', () => {
      mockUseTootSpeech.error.value = 'Audio context error'
      
      const wrapper = mount(TootSpeechPanel)
      
      expect(wrapper.text()).toContain('Audio context error')
    })
  })

  describe('Control Buttons', () => {
    it('should call resume when resume button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const resumeButton = wrapper.find('button[data-testid="resume-button"]')
      
      await resumeButton.trigger('click')
      
      expect(mockUseTootSpeech.resume).toHaveBeenCalled()
    })

    it('should call suspend when suspend button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const suspendButton = wrapper.find('button[data-testid="suspend-button"]')
      
      await suspendButton.trigger('click')
      
      expect(mockUseTootSpeech.suspend).toHaveBeenCalled()
    })

    it('should call initialize when initialize button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const initButton = wrapper.find('button[data-testid="init-button"]')
      
      await initButton.trigger('click')
      
      expect(mockUseTootSpeech.initialize).toHaveBeenCalled()
    })

    it('should call dispose when dispose button is clicked', async () => {
      const wrapper = mount(TootSpeechPanel)
      const disposeButton = wrapper.find('button[data-testid="dispose-button"]')
      
      await disposeButton.trigger('click')
      
      expect(mockUseTootSpeech.dispose).toHaveBeenCalled()
    })
  })

  describe('Input Validation', () => {
    it('should not call speak with empty input', async () => {
      const wrapper = mount(TootSpeechPanel)
      const speakButton = wrapper.find('button[data-testid="speak-button"]')
      
      await speakButton.trigger('click')
      
      expect(mockUseTootSpeech.speak).not.toHaveBeenCalled()
    })

    it('should not call queueSpeech with empty input', async () => {
      const wrapper = mount(TootSpeechPanel)
      const queueButton = wrapper.find('button[data-testid="queue-button"]')
      
      await queueButton.trigger('click')
      
      expect(mockUseTootSpeech.queueSpeech).not.toHaveBeenCalled()
    })
  })

  describe('Keyboard Shortcuts', () => {
    it('should call speak when Enter is pressed in input field', async () => {
      const wrapper = mount(TootSpeechPanel)
      const input = wrapper.find('input[type="text"]')
      
      await input.setValue('Hello!')
      await input.trigger('keydown.enter')
      
      expect(mockUseTootSpeech.speak).toHaveBeenCalledWith('Hello!')
    })

    it('should call stopSpeaking when Escape is pressed', async () => {
      const wrapper = mount(TootSpeechPanel)
      
      await wrapper.trigger('keydown.esc')
      
      expect(mockUseTootSpeech.stopSpeaking).toHaveBeenCalled()
    })
  })

  describe('Component Lifecycle', () => {
    it('should call initialize on mount', () => {
      mount(TootSpeechPanel)
      
      expect(mockUseTootSpeech.initialize).toHaveBeenCalled()
    })

    it('should call dispose on unmount', () => {
      const wrapper = mount(TootSpeechPanel)
      
      wrapper.unmount()
      
      expect(mockUseTootSpeech.dispose).toHaveBeenCalled()
    })
  })
})
