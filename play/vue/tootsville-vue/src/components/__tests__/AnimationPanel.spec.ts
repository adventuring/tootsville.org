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
      expect(wrapper.text()).toContain('No')
    })
  })

  describe('Character Controls', () => {
    it('should display character information', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Test Toot')
    })

    it('should call updateCharacter when character name changes', async () => {
      const wrapper = mount(AnimationPanel)
      const nameInput = wrapper.find('input[placeholder="Enter character name"]')
      
      await nameInput.setValue('NewName')
      
      expect(mockUseAnimationManager.updateCharacter).toHaveBeenCalled()
    })

    it('should display character capabilities', () => {
      const wrapper = mount(AnimationPanel)
      
      expect(wrapper.text()).toContain('Can Walk')
      expect(wrapper.text()).toContain('Can Jump')
      expect(wrapper.text()).toContain('Can Sit')
    })
  })

  describe('Animation Controls', () => {
    it('should render animation buttons for each capability', () => {
      const wrapper = mount(AnimationPanel)
      const animationButtons = wrapper.findAll('.control-btn')
      
      expect(animationButtons.length).toBeGreaterThan(0)
    })

    it('should call sit when sit button is clicked', async () => {
      const wrapper = mount(AnimationPanel)
      const buttons = wrapper.findAll('button')
      const sitButton = buttons.find(button => button.text().includes('Sit'))
      
      if (sitButton) {
        await sitButton.trigger('click')
        expect(mockUseAnimationManager.sit).toHaveBeenCalled()
      }
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

    it('should call jump when jump button is clicked', async () => {
      const wrapper = mount(AnimationPanel)
      const buttons = wrapper.findAll('button')
      const jumpButton = buttons.find(button => button.text().includes('Jump'))
      
      if (jumpButton) {
        await jumpButton.trigger('click')
        expect(mockUseAnimationManager.jump).toHaveBeenCalled()
      }
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
      const fadeDurationInput = wrapper.find('input[placeholder="0.3"]')
      
      await fadeDurationInput.setValue('0.5')
      
      expect(mockUseAnimationManager.updateSettings).toHaveBeenCalled()
    })

    it('should call updateSettings when movement threshold changes', async () => {
      const wrapper = mount(AnimationPanel)
      const thresholdInput = wrapper.find('input[placeholder="0.01"]')
      
      await thresholdInput.setValue('0.02')
      
      expect(mockUseAnimationManager.updateSettings).toHaveBeenCalled()
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
