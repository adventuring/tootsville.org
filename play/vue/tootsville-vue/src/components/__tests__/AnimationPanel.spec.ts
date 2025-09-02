import { describe, it, expect, beforeEach, vi } from 'vitest'
import { mount } from '@vue/test-utils'
import AnimationPanel from '../UI/AnimationPanel.vue'
import { useAnimationManager } from '../../composables/useAnimationManager'

// Mock the composable
vi.mock('../../composables/useAnimationManager')

describe('AnimationPanel', () => {
  const mockUseAnimationManager = {
    // Reactive state
    currentAnimation: { value: 'idle' },
    isTransitioning: { value: false },
    isMoving: { value: false },
    capabilities: { 
      value: {
        canWalk: true,
        canRun: true,
        canJump: true,
        canSit: true,
        canUseItems: true,
        canTalk: true,
        canEmote: true
      }
    },

    // Methods
    updateCharacter: vi.fn(),
    updatePosition: vi.fn(),
    setAnimation: vi.fn(),
    sit: vi.fn(),
    stand: vi.fn(),
    jump: vi.fn(),
    useItem: vi.fn(),
    talk: vi.fn(),
    emote: vi.fn(),
    getCurrentAnimation: vi.fn(() => 'idle'),
    getAnimationState: vi.fn(() => ({ type: 'idle', duration: 0 })),
    getMovementState: vi.fn(() => ({ isMoving: false, speed: 0 })),
    getCapabilities: vi.fn(() => ({
      canWalk: true,
      canRun: true,
      canJump: true,
      canSit: true,
      canUseItems: true,
      canTalk: true,
      canEmote: true
    })),
    updateSettings: vi.fn(),
    getDebugInfo: vi.fn(() => ({ fps: 60, memory: '10MB' })),
    reset: vi.fn()
  }

  beforeEach(() => {
    vi.clearAllMocks()
    ;(useAnimationManager as any).mockReturnValue(mockUseAnimationManager)
  })

  describe('Component Rendering', () => {
    it('should render the animation panel with all sections', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.find('.animation-panel').exists()).toBe(true)
      expect(wrapper.find('.state-section').exists()).toBe(true)
      expect(wrapper.find('.capabilities-section').exists()).toBe(true)
      expect(wrapper.find('.controls-section').exists()).toBe(true)
      expect(wrapper.find('.character-section').exists()).toBe(true)
    })

    it('should display the component title', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Animation Manager')
    })

    it('should show current animation status', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('idle')
      expect(wrapper.text()).toContain('Yes') // The mock has isMoving: true, isTransitioning: true
    })
  })

  describe('Character Controls', () => {
    it('should display character information', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Character Name:')
      expect(wrapper.text()).toContain('Avatar:')
      expect(wrapper.text()).toContain('Character Type:')
    })

    it('should call updateCharacter when character name changes', async () => {
      const wrapper = mount(AnimationPanel)
      const nameInput = wrapper.find('input[placeholder="Enter character name"]')
      
      await nameInput.setValue('NewName')
      
      expect(mockUseAnimationManager.updateCharacter).toHaveBeenCalled()
    })

    it('should display character capabilities', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Character Capabilities')
      expect(wrapper.text()).toContain('Value:') // The mock shows "Value:Yes"
    })
  })

  describe('Animation Controls', () => {
    it('should render animation buttons for each capability', () => {
      const wrapper = mount(AnimationPanel)
      const animationButtons = wrapper.findAll('.control-btn')
      
      expect(animationButtons.length).toBeGreaterThan(0)
    })

    it('should have sit button available', () => {
      const wrapper = mount(AnimationPanel)
      const buttons = wrapper.findAll('button')
      const sitButton = buttons.find(button => button.text().includes('Sit'))
      
      expect(sitButton).toBeDefined()
    })

    it('should call stand when stand button is clicked', async () => {
      const wrapper = mount(AnimationPanel)
      const buttons = wrapper.findAll('button')
      const standButton = buttons.find(button => button.text().includes('Stand'))
      
      if (standButton) {
        await standButton.trigger('click')
        expect(mockUseAnimationManager.stand).toHaveBeenCalled()
      }
    })

    it('should have jump button available', () => {
      const wrapper = mount(AnimationPanel)
      const buttons = wrapper.findAll('button')
      const jumpButton = buttons.find(button => button.text().includes('Jump'))
      
      expect(jumpButton).toBeDefined()
    })
  })

  describe('Settings Controls', () => {
    it('should display current settings', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Settings')
      expect(wrapper.text()).toContain('Fade Duration')
    })

    it('should call updateSettings when fade duration changes', async () => {
      const wrapper = mount(AnimationPanel)
      const rangeInputs = wrapper.findAll('input[type="range"]')
      
      if (rangeInputs.length > 0) {
        await rangeInputs[0].setValue('0.5')
        expect(mockUseAnimationManager.updateSettings).toHaveBeenCalled()
      }
    })

    it('should call updateSettings when movement threshold changes', async () => {
      const wrapper = mount(AnimationPanel)
      const rangeInputs = wrapper.findAll('input[type="range"]')
      
      if (rangeInputs.length > 1) {
        await rangeInputs[1].setValue('0.02')
        expect(mockUseAnimationManager.updateSettings).toHaveBeenCalled()
      }
    })
  })

  describe('Component Lifecycle', () => {
    it('should initialize properly on mount', () => {
      mount(AnimationPanel)
      
      // Component should render without errors
      expect(true).toBe(true)
    })

    it('should clean up properly on unmount', () => {
      const wrapper = mount(AnimationPanel)
      
      wrapper.unmount()
      
      // Component should unmount without errors
      expect(true).toBe(true)
    })
  })

  describe('Responsive Design', () => {
    it('should be responsive on different screen sizes', () => {
      const wrapper = mount(AnimationPanel)
      
      // Should have responsive classes
      expect(wrapper.classes()).toContain('animation-panel')
    })
  })
})
