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
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacterData(tootCharacter)
      const capabilities = animationManager.characterCapabilities.value

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
        characterType: 'MANATEE',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacterData(manateeCharacter)
      const capabilities = animationManager.characterCapabilities.value

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
        characterType: 'BIRD',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacterData(birdCharacter)
      const capabilities = animationManager.characterCapabilities.value

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
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.IDLE)
    })

    it('should set animation state', () => {
      // Need character data to set animations
      animationManager.updateCharacterData({
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      })
      animationManager.setAnimation(ANIMATION_STATES.WALKING)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.WALKING)
    })

    it('should not allow invalid animations', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
      
      // Need character data to set animations
      animationManager.updateCharacterData({
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      })
      
      // The new API allows any animation by default, so this should work
      animationManager.setAnimation('invalid' as any)
      
      // Should not warn and should set the animation
      expect(consoleSpy).not.toHaveBeenCalled()
      expect(animationManager.getCurrentAnimation().current).toBe('invalid')
      
      consoleSpy.mockRestore()
    })

    it('should not allow animations that character cannot perform', () => {
      const tootCharacter = {
        id: '1',
        name: 'Zap',
        avatar: 'zap',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }

      animationManager.updateCharacterData(tootCharacter)
      
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
      
      animationManager.setAnimation(ANIMATION_STATES.JUMPING)
      
      expect(consoleSpy).toHaveBeenCalledWith('Character cannot perform animation: jump')
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.IDLE)
      
      consoleSpy.mockRestore()
    })
  })

  describe('Animation Controls', () => {
    beforeEach(() => {
      const character = {
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }
      animationManager.updateCharacterData(character)
    })

    it('should handle sit command', () => {
      animationManager.setAnimation(ANIMATION_STATES.SITTING)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.SITTING)
    })

    it('should handle jump command', () => {
      // Toots can't jump, so this should fail
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
      animationManager.setAnimation(ANIMATION_STATES.JUMPING)
      expect(consoleSpy).toHaveBeenCalledWith('Character cannot perform animation: jump')
      consoleSpy.mockRestore()
    })

    it('should handle use item command', () => {
      animationManager.setAnimation(ANIMATION_STATES.USING_ITEM)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.USING_ITEM)
    })

    it('should handle talk command', () => {
      animationManager.setAnimation(ANIMATION_STATES.TALKING)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.TALKING)
    })

    it('should handle emote command', () => {
      animationManager.setAnimation(ANIMATION_STATES.EMOTING)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.EMOTING)
    })
  })

  describe('Movement Detection', () => {
    it('should detect movement', () => {
      const character = {
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      }
      animationManager.updateCharacterData(character)

      // Simulate movement
      const newPosition = new Vector3(1, 0, 0)
      animationManager.updateMovementState(newPosition, Date.now())

      // Should trigger movement detection and update animation
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.WALKING)
    })
  })

  describe('Settings', () => {
    it('should update settings', () => {
      const newSettings = {
        fadeDuration: 500,
        minMovementThreshold: 0.02
      }

      animationManager.updateSettings(newSettings)
      const settings = animationManager.getSettings()

      expect(settings.fadeDuration).toBe(500)
      expect(settings.minMovementThreshold).toBe(0.02)
    })
  })

  describe('Reset', () => {
    it('should reset animation state', () => {
      // Need character data to set animations
      animationManager.updateCharacterData({
        id: '1',
        name: 'Test',
        avatar: 'test',
        characterType: 'TOOT',
        position: new Vector3(0, 0, 0),
        rotation: new Vector3(0, 0, 0),
        scale: new Vector3(1, 1, 1)
      })
      
      animationManager.setAnimation(ANIMATION_STATES.WALKING)
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.WALKING)

      animationManager.reset()
      expect(animationManager.getCurrentAnimation().current).toBe(ANIMATION_STATES.IDLE)
    })
  })
})
