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
  // Birds/Katootels - can fly, walk, jump, but can't swim
  BIRD: {
    canJump: true,
    canSwim: false,
    canFly: true,
    canWalk: true,
    canRun: true,
    canSit: true,
    defaultAnimations: ['idle', 'walk', 'run', 'sit', 'jump', 'fly']
  }
}

/**
 * Animation Manager Service
 * 
 * Manages character animation states, movement detection, and character-specific
 * capabilities for the Tootsville game world.
 * 
 * @class AnimationManager
 * @description Manages character animation states and movement detection
 * 
 * @example
 * ```typescript
 * import { animationManager } from './AnimationManager'
 * 
 * // Update character data
 * animationManager.updateCharacterData({
 *   id: 'player1',
 *   name: 'Tootie',
 *   characterType: 'TOOT',
 *   position: new Vector3(0, 0, 0)
 * })
 * 
 * // Set animation state
 * animationManager.setAnimation('walk')
 * 
 * // Check if character can perform action
 * if (animationManager.canPerformAction('jump')) {
 *   animationManager.setAnimation('jump')
 * }
 * ```
 */
export class AnimationManager {
  // Reactive state
  private _characterData = ref<CharacterData | null>(null)
  private _animationState = ref<AnimationState>({
    current: ANIMATION_STATES.IDLE,
    previous: ANIMATION_STATES.IDLE,
    isTransitioning: false,
    transitionStartTime: 0,
    transitionDuration: 300
  })

  private _movementState = ref<MovementState>({
    isMoving: false,
    velocity: new Vector3(),
    lastPosition: new Vector3(),
    movementThreshold: 0.1,
    lastMovementCheck: 0
  })

  private _settings = ref<AnimationSettings>({
    fadeDuration: 300,
    crossfadeDuration: 150,
    minMovementThreshold: 0.1,
    movementCheckInterval: 100
  })

  // Computed properties
  public readonly characterData = computed(() => this._characterData.value)
  public readonly animationState = computed(() => this._animationState.value)
  public readonly movementState = computed(() => this._movementState.value)
  public readonly settings = computed(() => this._settings.value)

  /**
   * Get character capabilities based on character type
   * 
   * @description Returns movement and animation capabilities for the current character type
   * @inputs - _characterData.value?.characterType (string) - Character type identifier
   * @outputs - CharacterCapabilities object with boolean flags for movement abilities
   * @returns {CharacterCapabilities | null} Character capabilities or null if no character data
   */
  public readonly characterCapabilities = computed(() => {
    if (!this._characterData.value?.characterType) {
      return null
    }
    return CHARACTER_CAPABILITIES[this._characterData.value.characterType] || null
  })

  /**
   * Check if character can perform a specific action
   * 
   * @description Tests if the current character can perform a given animation/action
   * @inputs - action (string) - Action name to test, characterCapabilities (CharacterCapabilities)
   * @outputs - boolean - true if character can perform action, false otherwise
   * @param {string} action - Action to check (e.g., 'jump', 'swim', 'fly')
   * @returns {boolean} Whether the character can perform the action
   */
  public readonly canPerformAction = computed(() => {
    return (action: string): boolean => {
      const capabilities = this.characterCapabilities.value
      if (!capabilities) return false

      switch (action.toLowerCase()) {
        case 'jump':
          return capabilities.canJump
        case 'swim':
          return capabilities.canSwim
        case 'fly':
          return capabilities.canFly
        case 'walk':
          return capabilities.canWalk
        case 'run':
          return capabilities.canRun
        case 'sit':
          return capabilities.canSit
        default:
          return true // Allow other actions by default
      }
    }
  })

  /**
   * Update character data
   * 
   * @description Updates the character information and resets movement state
   * @inputs - data (CharacterData) - New character data
   * @sideEffects - Updates _characterData.value, resets _movementState.value.lastPosition
   * @param {CharacterData} data - Character data to update
   */
  updateCharacterData(data: CharacterData): void {
    this._characterData.value = data
    // Reset movement tracking when character data changes
    this._movementState.value.lastPosition = data.position.clone()
  }

  /**
   * Set animation state
   * 
   * @description Changes the current animation state with transition handling
   * @inputs - animation (string) - Animation state name, force (boolean) - Force immediate transition
   * @sideEffects - Updates _animationState.value with new state and transition timing
   * @param {string} animation - Animation state to set
   * @param {boolean} force - Force immediate transition (default: false)
   */
  setAnimation(animation: string, force: boolean = false): void {
    const currentState = this._animationState.value
    const capabilities = this.characterCapabilities.value

    // Check if character can perform this animation
    if (!this.canPerformAction.value(animation)) {
      console.warn(`Character cannot perform animation: ${animation}`)
      return
    }

    // Don't change if already in this state (unless forced)
    if (currentState.current === animation && !force) {
      return
    }

    // Update animation state
    this._animationState.value = {
      current: animation,
      previous: currentState.current,
      isTransitioning: true,
      transitionStartTime: Date.now(),
      transitionDuration: force ? 0 : this._settings.value.fadeDuration
    }

    // Clear transition flag after duration
    if (this._animationState.value.transitionDuration > 0) {
      setTimeout(() => {
        this._animationState.value.isTransitioning = false
      }, this._animationState.value.transitionDuration)
    } else {
      this._animationState.value.isTransitioning = false
    }
  }

  /**
   * Get current animation state
   * 
   * @description Returns the current animation state information
   * @outputs - AnimationState object with current state and transition information
   * @returns {AnimationState} Current animation state
   */
  getCurrentAnimation(): AnimationState {
    return this._animationState.value
  }

  /**
   * Check if character is moving
   * 
   * @description Determines if the character is currently in motion
   * @inputs - _movementState.value.isMoving (boolean)
   * @outputs - boolean - true if character is moving, false otherwise
   * @returns {boolean} Whether the character is moving
   */
  isMoving(): boolean {
    return this._movementState.value.isMoving
  }

  /**
   * Update movement state based on position change
   * 
   * @description Detects movement by comparing current position with last known position
   * @inputs - position (Vector3) - Current character position, timestamp (number) - Current time
   * @sideEffects - Updates _movementState.value with movement detection results
   * @param {Vector3} position - Current character position
   * @param {number} timestamp - Current timestamp in milliseconds
   */
  updateMovementState(position: Vector3, timestamp: number): void {
    const movementState = this._movementState.value
    const settings = this._settings.value

    // Check if enough time has passed since last movement check
    if (timestamp - movementState.lastMovementCheck < settings.movementCheckInterval) {
      return
    }

    // Calculate distance moved
    const distance = position.distanceTo(movementState.lastPosition)
    const isMoving = distance > settings.minMovementThreshold

    // Update movement state
    this._movementState.value = {
      isMoving,
      velocity: position.clone().sub(movementState.lastPosition),
      lastPosition: position.clone(),
      movementThreshold: settings.minMovementThreshold,
      lastMovementCheck: timestamp
    }

    // Auto-update animation based on movement
    if (isMoving && this._animationState.value.current === ANIMATION_STATES.IDLE) {
      this.setAnimation(ANIMATION_STATES.WALKING)
    } else if (!isMoving && this._animationState.value.current === ANIMATION_STATES.WALKING) {
      this.setAnimation(ANIMATION_STATES.IDLE)
    }
  }

  /**
   * Get movement state
   * 
   * @description Returns current movement state information
   * @outputs - MovementState object with movement flags and velocity
   * @returns {MovementState} Current movement state
   */
  getMovementState(): MovementState {
    return this._movementState.value
  }

  /**
   * Update animation settings
   * 
   * @description Modifies animation timing and movement detection parameters
   * @inputs - settings (Partial<AnimationSettings>) - Settings to update
   * @sideEffects - Updates _settings.value by merging with provided settings
   * @param {Partial<AnimationSettings>} settings - Settings to update
   */
  updateSettings(settings: Partial<AnimationSettings>): void {
    this._settings.value = { ...this._settings.value, ...settings }
  }

  /**
   * Get animation settings
   * 
   * @description Returns current animation configuration
   * @outputs - AnimationSettings object with timing and threshold values
   * @returns {AnimationSettings} Current animation settings
   */
  getSettings(): AnimationSettings {
    return this._settings.value
  }

  /**
   * Reset animation state
   * 
   * @description Resets animation and movement state to default values
   * @sideEffects - Resets _animationState.value and _movementState.value to initial values
   */
  reset(): void {
    this._animationState.value = {
      current: ANIMATION_STATES.IDLE,
      previous: ANIMATION_STATES.IDLE,
      isTransitioning: false,
      transitionStartTime: 0,
      transitionDuration: 300
    }

    this._movementState.value = {
      isMoving: false,
      velocity: new Vector3(),
      lastPosition: this._characterData.value?.position.clone() || new Vector3(),
      movementThreshold: this._settings.value.minMovementThreshold,
      lastMovementCheck: 0
    }
  }

  /**
   * Get character type
   * 
   * @description Returns the current character type identifier
   * @inputs - _characterData.value?.characterType (string)
   * @outputs - string | undefined - Character type or undefined
   * @returns {string | undefined} Current character type
   */
  getCharacterType(): string | undefined {
    return this._characterData.value?.characterType
  }

  /**
   * Get character name
   * 
   * @description Returns the current character name
   * @inputs - _characterData.value?.name (string)
   * @outputs - string | undefined - Character name or undefined
   * @returns {string | undefined} Current character name
   */
  getCharacterName(): string | undefined {
    return this._characterData.value?.name
  }

  /**
   * Check if animation is transitioning
   * 
   * @description Determines if an animation transition is currently in progress
   * @inputs - _animationState.value.isTransitioning (boolean)
   * @outputs - boolean - true if transitioning, false otherwise
   * @returns {boolean} Whether animation is transitioning
   */
  isTransitioning(): boolean {
    return this._animationState.value.isTransitioning
  }

  /**
   * Get transition progress
   * 
   * @description Calculates the progress of the current animation transition
   * @inputs - _animationState.value.transitionStartTime (number), _animationState.value.transitionDuration (number)
   * @outputs - number - Transition progress from 0.0 to 1.0
   * @returns {number} Transition progress (0.0 to 1.0)
   */
  getTransitionProgress(): number {
    const state = this._animationState.value
    if (!state.isTransitioning || state.transitionDuration === 0) {
      return 1.0
    }

    const elapsed = Date.now() - state.transitionStartTime
    return Math.min(elapsed / state.transitionDuration, 1.0)
  }

  /**
   * Get available animations for current character
   * 
   * @description Returns list of animations available to the current character type
   * @inputs - characterCapabilities (CharacterCapabilities)
   * @outputs - string[] - Array of available animation names
   * @returns {string[]} Available animations for current character
   */
  getAvailableAnimations(): string[] {
    const capabilities = this.characterCapabilities.value
    return capabilities?.defaultAnimations || [ANIMATION_STATES.IDLE]
  }

  /**
   * Validate animation for current character
   * 
   * @description Checks if an animation is valid for the current character type
   * @inputs - animation (string) - Animation to validate, characterCapabilities (CharacterCapabilities)
   * @outputs - boolean - true if animation is valid, false otherwise
   * @param {string} animation - Animation to validate
   * @returns {boolean} Whether the animation is valid for current character
   */
  isValidAnimation(animation: string): boolean {
    const capabilities = this.characterCapabilities.value
    if (!capabilities) return false

    // Check if animation is in default animations list
    if (capabilities.defaultAnimations.includes(animation)) {
      return true
    }

    // Check if character can perform the action
    return this.canPerformAction.value(animation)
  }

  /**
   * Get character position
   * 
   * @description Returns the current character position
   * @inputs - _characterData.value?.position (Vector3)
   * @outputs - Vector3 | undefined - Character position or undefined
   * @returns {Vector3 | undefined} Current character position
   */
  getCharacterPosition(): Vector3 | undefined {
    return this._characterData.value?.position
  }

  /**
   * Get character rotation
   * 
   * @description Returns the current character rotation
   * @inputs - _characterData.value?.rotation (Vector3)
   * @outputs - Vector3 | undefined - Character rotation or undefined
   * @returns {Vector3 | undefined} Current character rotation
   */
  getCharacterRotation(): Vector3 | undefined {
    return this._characterData.value?.rotation
  }

  /**
   * Get character scale
   * 
   * @description Returns the current character scale
   * @inputs - _characterData.value?.scale (Vector3)
   * @outputs - Vector3 | undefined - Character scale or undefined
   * @returns {Vector3 | undefined} Current character scale
   */
  getCharacterScale(): Vector3 | undefined {
    return this._characterData.value?.scale
  }

  /**
   * Get character ID
   * 
   * @description Returns the current character ID
   * @inputs - _characterData.value?.id (string)
   * @outputs - string | undefined - Character ID or undefined
   * @returns {string | undefined} Current character ID
   */
  getCharacterId(): string | undefined {
    return this._characterData.value?.id
  }

  /**
   * Get character avatar
   * 
   * @description Returns the current character avatar
   * @inputs - _characterData.value?.avatar (string)
   * @outputs - string | undefined - Character avatar or undefined
   * @returns {string | undefined} Current character avatar
   */
  getCharacterAvatar(): string | undefined {
    return this._characterData.value?.avatar
  }

  /**
   * Get character species
   * 
   * @description Returns the current character species
   * @inputs - _characterData.value?.species (string)
   * @outputs - string | undefined - Character species or undefined
   * @returns {string | undefined} Current character species
   */
  getCharacterSpecies(): string | undefined {
    return this._characterData.value?.species
  }

  /**
   * Get character avatar class
   * 
   * @description Returns the current character avatar class information
   * @inputs - _characterData.value?.avatarClass (object)
   * @outputs - object | undefined - Avatar class info or undefined
   * @returns {object | undefined} Current character avatar class
   */
  getCharacterAvatarClass(): { title?: string; filename?: string } | undefined {
    return this._characterData.value?.avatarClass
  }
}

// Export singleton instance
export const animationManager = new AnimationManager()
