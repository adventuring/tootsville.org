import { describe, it, expect, beforeEach, vi } from 'vitest'
import { AnimationManager, ANIMATION_STATES, CHARACTER_CAPABILITIES } from '../AnimationManager'
import { Vector3 } from 'three'

describe('AnimationManager', () => {
  let animationManager: AnimationManager

  beforeEach(() => {
    animationManager = new AnimationManager()
  })

  describe('Character Capabilities', () => {
    it('should detect Toot character capabilities', () => {
      const tootCharacter = {
        id: '1',
        name: 'Zap',
        avatar: 'zap',
        characterType: 'toot',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacter(tootCharacter)
      const capabilities = animationManager.getCapabilities()

      expect(capabilities.canJump).toBe(false)
      expect(capabilities.canSwim).toBe(true)
      expect(capabilities.canFly).toBe(false)
      expect(capabilities.canWalk).toBe(true)
      expect(capabilities.canRun).toBe(true)
      expect(capabilities.canSit).toBe(true)
    })

    it('should detect Manatee character capabilities', () => {
      const manateeCharacter = {
        id: '2',
        name: 'Sea Cow',
        avatar: 'manatee',
        characterType: 'manatee',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacter(manateeCharacter)
      const capabilities = animationManager.getCapabilities()

      expect(capabilities.canJump).toBe(false)
      expect(capabilities.canSwim).toBe(true)
      expect(capabilities.canFly).toBe(false)
      expect(capabilities.canWalk).toBe(false)
      expect(capabilities.canRun).toBe(false)
      expect(capabilities.canSit).toBe(false)
    })

    it('should detect Bird character capabilities', () => {
      const birdCharacter = {
        id: '3',
        name: 'Katootel',
        avatar: 'bird',
        characterType: 'bird',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacter(birdCharacter)
      const capabilities = animationManager.getCapabilities()

      expect(capabilities.canJump).toBe(true)
      expect(capabilities.canSwim).toBe(false)
      expect(capabilities.canFly).toBe(true)
      expect(capabilities.canWalk).toBe(true)
      expect(capabilities.canRun).toBe(true)
      expect(capabilities.canSit).toBe(true)
    })
  })

  describe('Animation States', () => {
    it('should start with idle animation', () => {
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.IDLE)
    })

    it('should set animation state', () => {
      animationManager.setAnimation(ANIMATION_STATES.WALKING)
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.WALKING)
    })

    it('should not allow invalid animations', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
      
      animationManager.setAnimation('invalid' as any)
      
      expect(consoleSpy).toHaveBeenCalledWith('Invalid animation state: invalid')
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.IDLE)
      
      consoleSpy.mockRestore()
    })

    it('should not allow animations that character cannot perform', () => {
      const tootCharacter = {
        id: '1',
        name: 'Zap',
        avatar: 'zap',
        characterType: 'toot',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacter(tootCharacter)
      
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
      
      animationManager.setAnimation(ANIMATION_STATES.JUMPING)
      
      expect(consoleSpy).toHaveBeenCalledWith('Character cannot perform animation: jump')
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.IDLE)
      
      consoleSpy.mockRestore()
    })
  })

  describe('Animation Controls', () => {
    beforeEach(() => {
      const character = {
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'default',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }
      animationManager.updateCharacter(character)
    })

    it('should handle sit command', () => {
      animationManager.sit()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.SITTING)
    })

    it('should handle jump command', () => {
      animationManager.jump()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.JUMPING)
    })

    it('should handle use item command', () => {
      animationManager.useItem()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.USING_ITEM)
    })

    it('should handle talk command', () => {
      animationManager.talk()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.TALKING)
    })

    it('should handle emote command', () => {
      animationManager.emote()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.EMOTING)
    })
  })

  describe('Movement Detection', () => {
    it('should detect movement', () => {
      const character = {
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'default',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }
      animationManager.updateCharacter(character)

      // Simulate movement
      const newPosition = new Vector3(1, 0, 0)
      animationManager.updatePosition(newPosition, 1.0)

      // Should trigger movement detection and update animation
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.WALKING)
    })
  })

  describe('Settings', () => {
    it('should update settings', () => {
      const newSettings = {
        fadeDuration: 0.5,
        minMovementThreshold: 0.02
      }

      animationManager.updateSettings(newSettings)
      const debugInfo = animationManager.getDebugInfo()

      expect(debugInfo).toBeDefined()
    })
  })

  describe('Reset', () => {
    it('should reset animation state', () => {
      animationManager.setAnimation(ANIMATION_STATES.WALKING)
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.WALKING)

      animationManager.reset()
      expect(animationManager.getCurrentAnimation()).toBe(ANIMATION_STATES.IDLE)
    })
  })
})
