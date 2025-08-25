/**
 * AnimationManager.ts - Player Character Animation State Management
 * 
 * Vue 3 / TypeScript version for animation state management
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'
import { Vector3 } from 'three'

// Types
export interface CharacterCapabilities {
  canJump: boolean
  canSwim: boolean
  canFly: boolean
  canWalk: boolean
  canRun: boolean
  canSit: boolean
  defaultAnimations: string[]
}

export interface AnimationState {
  current: string
  previous: string
  isTransitioning: boolean
  transitionStartTime: number
  transitionDuration: number
}

export interface MovementState {
  isMoving: boolean
  velocity: Vector3
  lastPosition: Vector3
  movementThreshold: number
  lastMovementCheck: number
}

export interface AnimationSettings {
  fadeDuration: number
  crossfadeDuration: number
  minMovementThreshold: number
  movementCheckInterval: number
}

export interface CharacterData {
  id: string
  name: string
  avatar: string
  avatarClass?: {
    title?: string
    filename?: string
  }
  characterType?: string
  species?: string
  position: Vector3
  rotation: Vector3
  scale: Vector3
}

/**
 * Animation state constants
 */
export const ANIMATION_STATES = {
  IDLE: 'idle',
  WALKING: 'walk',
  RUNNING: 'run',
  SITTING: 'sit',
  JUMPING: 'jump',
  SWIMMING: 'swim',
  FLYING: 'fly',
  EMOTING: 'emote',
  USING_ITEM: 'use_item',
  TALKING: 'talk'
} as const

export type AnimationStateType = typeof ANIMATION_STATES[keyof typeof ANIMATION_STATES]

/**
 * Character type definitions with movement capabilities
 */
export const CHARACTER_CAPABILITIES: Record<string, CharacterCapabilities> = {
  // Toots (elephants) - can't jump but can walk, run, sit, swim
  TOOT: {
    canJump: false,
    canSwim: true,
    canFly: false,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'swim']
  },
  // Manatees - can swim, can't jump or fly
  MANATEE: {
    canJump: false,
    canSwim: true,
    canFly: false,
    canWalk: false,
    canRun: false,
    canSit: false,
    defaultAnimations: ['idle', 'swim']
  },
  // Birds/Katootels - can fly, jump, walk
  BIRD: {
    canJump: true,
    canSwim: false,
    canFly: true,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'jump', 'fly']
  },
  // Default for unknown character types
  DEFAULT: {
    canJump: true,
    canSwim: false,
    canFly: false,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'jump']
  }
}

/**
 * Animation transition settings
 */
export const TRANSITION_SETTINGS: AnimationSettings = {
  fadeDuration: 0.3,
  crossfadeDuration: 0.2,
  minMovementThreshold: 0.01,
  movementCheckInterval: 100 // milliseconds
}

/**
 * Animation Manager Service
 * 
 * Manages player character animation states with proper transitions between
 * walking, idle, and other states. Supports character-specific movement
 * capabilities (elephants can't jump, manatees can swim, etc.).
 */
export class AnimationManager {
  // Reactive state
  private _animationState = reactive<AnimationState>({
    current: ANIMATION_STATES.IDLE,
    previous: ANIMATION_STATES.IDLE,
    isTransitioning: false,
    transitionStartTime: 0,
    transitionDuration: TRANSITION_SETTINGS.fadeDuration
  })

  private _movementState = reactive<MovementState>({
    isMoving: false,
    velocity: new Vector3(),
    lastPosition: new Vector3(),
    movementThreshold: TRANSITION_SETTINGS.minMovementThreshold,
    lastMovementCheck: 0
  })

  private _characterData = ref<CharacterData | null>(null)
  private _capabilities = ref<CharacterCapabilities>(CHARACTER_CAPABILITIES.DEFAULT)
  private _settings = ref<AnimationSettings>({ ...TRANSITION_SETTINGS })

  // Computed properties
  public readonly currentAnimation = computed(() => this._animationState.current)
  public readonly isTransitioning = computed(() => this._animationState.isTransitioning)
  public readonly isMoving = computed(() => this._movementState.isMoving)
  public readonly capabilities = computed(() => this._capabilities.value)

  constructor() {
    this.setupMovementDetection()
  }

  /**
   * Get character capabilities based on character type
   */
  private getCharacterCapabilities(character: CharacterData | null): CharacterCapabilities {
    if (!character) return CHARACTER_CAPABILITIES.DEFAULT
    
    // Determine character type based on avatar or character properties
    const avatarName = character.avatar || character.name || ''
    const avatarClass = character.avatarClass || {}
    const characterType = character.characterType || character.species || ''
    
    // Check for Toot (elephant) characteristics
    if (this.isTootCharacter(avatarName, avatarClass, characterType)) {
      return CHARACTER_CAPABILITIES.TOOT
    }
    
    // Check for Manatee characteristics
    if (this.isManateeCharacter(avatarName, avatarClass, characterType)) {
      return CHARACTER_CAPABILITIES.MANATEE
    }
    
    // Check for Bird/Katootel characteristics
    if (this.isBirdCharacter(avatarName, avatarClass, characterType)) {
      return CHARACTER_CAPABILITIES.BIRD
    }
    
    // Default capabilities
    return CHARACTER_CAPABILITIES.DEFAULT
  }

  /**
   * Check if character is a Toot (elephant)
   */
  private isTootCharacter(avatarName: string, avatarClass: any, characterType: string): boolean {
    const tootKeywords = ['toot', 'elephant', 'ultra', 'zap', 'flora', 'sparkle', 'moo', 'superstar', 'lil mc', 'cupid', 'dottie']
    const name = avatarName.toLowerCase()
    const title = (avatarClass.title || '').toLowerCase()
    const filename = (avatarClass.filename || '').toLowerCase()
    const type = characterType.toLowerCase()
    
    return tootKeywords.some(keyword => 
      name.includes(keyword) || 
      title.includes(keyword) || 
      filename.includes(keyword) || 
      type.includes(keyword)
    )
  }

  /**
   * Check if character is a Manatee
   */
  private isManateeCharacter(avatarName: string, avatarClass: any, characterType: string): boolean {
    const manateeKeywords = ['manatee', 'sea cow', 'aquatic']
    const name = avatarName.toLowerCase()
    const title = (avatarClass.title || '').toLowerCase()
    const filename = (avatarClass.filename || '').toLowerCase()
    const type = characterType.toLowerCase()
    
    return manateeKeywords.some(keyword => 
      name.includes(keyword) || 
      title.includes(keyword) || 
      filename.includes(keyword) || 
      type.includes(keyword)
    )
  }

  /**
   * Check if character is a Bird/Katootel
   */
  private isBirdCharacter(avatarName: string, avatarClass: any, characterType: string): boolean {
    const birdKeywords = ['bird', 'katootel', 'wing', 'feather', 'fly']
    const name = avatarName.toLowerCase()
    const title = (avatarClass.title || '').toLowerCase()
    const filename = (avatarClass.filename || '').toLowerCase()
    const type = characterType.toLowerCase()
    
    return birdKeywords.some(keyword => 
      name.includes(keyword) || 
      title.includes(keyword) || 
      filename.includes(keyword) || 
      type.includes(keyword)
    )
  }

  /**
   * Setup movement detection
   */
  private setupMovementDetection(): void {
    // Movement detection will be handled by the game loop
    // This method can be extended for additional movement detection logic
  }

  /**
   * Update character data and recalculate capabilities
   */
  updateCharacter(character: CharacterData): void {
    this._characterData.value = character
    this._capabilities.value = this.getCharacterCapabilities(character)
    
    // Update last position for movement detection
    this._movementState.lastPosition.copy(character.position)
  }

  /**
   * Update character position and detect movement
   */
  updatePosition(position: Vector3, deltaTime: number): void {
    if (!this._characterData.value) return

    const now = Date.now()
    const timeSinceLastCheck = now - this._movementState.lastMovementCheck

    // Check movement at regular intervals
    if (timeSinceLastCheck >= this._settings.value.movementCheckInterval) {
      this._movementState.lastMovementCheck = now
      
      // Calculate velocity
      this._movementState.velocity.subVectors(position, this._movementState.lastPosition)
      this._movementState.velocity.divideScalar(deltaTime)
      
      // Check if moving
      const speed = this._movementState.velocity.length()
      const wasMoving = this._movementState.isMoving
      this._movementState.isMoving = speed > this._movementState.movementThreshold
      
      // Update animation state based on movement
      if (this._movementState.isMoving !== wasMoving) {
        this.updateAnimationState()
      }
      
      // Update last position
      this._movementState.lastPosition.copy(position)
    }
  }

  /**
   * Update animation state based on current conditions
   */
  private updateAnimationState(): void {
    const capabilities = this._capabilities.value
    let targetAnimation = ANIMATION_STATES.IDLE

    // Priority order: sitting > jumping > item usage > movement
    if (this._animationState.current === ANIMATION_STATES.SITTING) {
      targetAnimation = ANIMATION_STATES.SITTING
    } else if (this._animationState.current === ANIMATION_STATES.JUMPING) {
      targetAnimation = ANIMATION_STATES.JUMPING
    } else if (this._animationState.current === ANIMATION_STATES.USING_ITEM) {
      targetAnimation = ANIMATION_STATES.USING_ITEM
    } else if (this._movementState.isMoving) {
      // Determine movement animation based on capabilities
      if (capabilities.canSwim && this.isInWater()) {
        targetAnimation = ANIMATION_STATES.SWIMMING
      } else if (capabilities.canFly && this.isFlying()) {
        targetAnimation = ANIMATION_STATES.FLYING
      } else if (capabilities.canRun && this.isRunning()) {
        targetAnimation = ANIMATION_STATES.RUNNING
      } else if (capabilities.canWalk) {
        targetAnimation = ANIMATION_STATES.WALKING
      }
    }

    this.setAnimation(targetAnimation)
  }

  /**
   * Set animation state with transition
   */
  setAnimation(animation: AnimationStateType, immediate: boolean = false): void {
    if (!this.isValidAnimation(animation)) {
      console.warn(`Invalid animation state: ${animation}`)
      return
    }

    if (this._animationState.current === animation) {
      return // Already in this animation state
    }

    // Check if character can perform this animation
    if (!this.canPerformAnimation(animation)) {
      console.warn(`Character cannot perform animation: ${animation}`)
      return
    }

    if (immediate) {
      // Immediate transition
      this._animationState.previous = this._animationState.current
      this._animationState.current = animation
      this._animationState.isTransitioning = false
    } else {
      // Smooth transition
      this._animationState.previous = this._animationState.current
      this._animationState.current = animation
      this._animationState.isTransitioning = true
      this._animationState.transitionStartTime = Date.now()
      this._animationState.transitionDuration = this._settings.value.fadeDuration
    }
  }

  /**
   * Check if animation is valid for current character
   */
  private isValidAnimation(animation: AnimationStateType): boolean {
    return Object.values(ANIMATION_STATES).includes(animation)
  }

  /**
   * Check if character can perform the given animation
   */
  private canPerformAnimation(animation: AnimationStateType): boolean {
    const capabilities = this._capabilities.value

    switch (animation) {
      case ANIMATION_STATES.JUMPING:
        return capabilities.canJump
      case ANIMATION_STATES.SWIMMING:
        return capabilities.canSwim
      case ANIMATION_STATES.FLYING:
        return capabilities.canFly
      case ANIMATION_STATES.WALKING:
        return capabilities.canWalk
      case ANIMATION_STATES.RUNNING:
        return capabilities.canRun
      case ANIMATION_STATES.SITTING:
        return capabilities.canSit
      default:
        return true // Other animations are generally available
    }
  }

  /**
   * Check if character is in water (for swimming detection)
   */
  private isInWater(): boolean {
    if (!this._characterData.value) return false
    
    // Check if character is in an ocean world or underwater
    const position = this._characterData.value.position
    return position.y < 0 // Simple water detection based on Y position
  }

  /**
   * Check if character is flying
   */
  private isFlying(): boolean {
    if (!this._characterData.value) return false
    
    // Check if character is above ground level
    const position = this._characterData.value.position
    return position.y > 5 // Simple flying detection
  }

  /**
   * Check if character is running
   */
  private isRunning(): boolean {
    const speed = this._movementState.velocity.length()
    return speed > 2.0 // Threshold for running speed
  }

  /**
   * Force sit animation
   */
  sit(): void {
    if (this._capabilities.value.canSit) {
      this.setAnimation(ANIMATION_STATES.SITTING)
    }
  }

  /**
   * Force stand animation
   */
  stand(): void {
    this.updateAnimationState()
  }

  /**
   * Force jump animation
   */
  jump(): void {
    if (this._capabilities.value.canJump) {
      this.setAnimation(ANIMATION_STATES.JUMPING)
    }
  }

  /**
   * Use item animation
   */
  useItem(): void {
    this.setAnimation(ANIMATION_STATES.USING_ITEM)
  }

  /**
   * Talk animation
   */
  talk(): void {
    this.setAnimation(ANIMATION_STATES.TALKING)
  }

  /**
   * Emote animation
   */
  emote(): void {
    this.setAnimation(ANIMATION_STATES.EMOTING)
  }

  /**
   * Get current animation state
   */
  getCurrentAnimation(): AnimationStateType {
    return this._animationState.current
  }

  /**
   * Get animation state object
   */
  getAnimationState(): AnimationState {
    return { ...this._animationState }
  }

  /**
   * Get movement state
   */
  getMovementState(): MovementState {
    return { ...this._movementState }
  }

  /**
   * Get character capabilities
   */
  getCapabilities(): CharacterCapabilities {
    return { ...this._capabilities.value }
  }

  /**
   * Update animation settings
   */
  updateSettings(settings: Partial<AnimationSettings>): void {
    this._settings.value = { ...this._settings.value, ...settings }
  }

  /**
   * Get debug information
   */
  getDebugInfo(): any {
    return {
      currentAnimation: this._animationState.current,
      isTransitioning: this._animationState.isTransitioning,
      isMoving: this._movementState.isMoving,
      capabilities: this._capabilities.value,
      velocity: this._movementState.velocity.toArray(),
      position: this._characterData.value?.position.toArray()
    }
  }

  /**
   * Reset animation state
   */
  reset(): void {
    this._animationState.current = ANIMATION_STATES.IDLE
    this._animationState.previous = ANIMATION_STATES.IDLE
    this._animationState.isTransitioning = false
    this._movementState.isMoving = false
    this._movementState.velocity.set(0, 0, 0)
  }
}

// Export singleton instance
export const animationManager = new AnimationManager()
