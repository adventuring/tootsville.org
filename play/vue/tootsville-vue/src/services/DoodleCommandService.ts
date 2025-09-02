/**
 * DoodleCommandService.ts - Doodle Command Service for Avatar Appearance Changes
 * 
 * Implements the Doodle command functionality that allows changing skin colors
 * and patterns by interacting with Doodle, as specifically mentioned by the
 * user as a key requirement.
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { EventEmitter } from 'events'

// Types
export interface DoodleConfig {
  characterId: string
  serverEndpoint: string
}

export interface ColorChangeRequest {
  character: string
  colorType: 'base' | 'pad' | 'pattern'
  color: string
}

export interface PatternChangeRequest {
  character: string
  pattern: string
}

export interface DoodleResponse {
  success: boolean
  message?: string
  error?: string
}

export interface ColorChangeEvent {
  character: string
  colorType: 'base' | 'pad' | 'pattern'
  color: string
  success: boolean
  error?: string
}

export interface PatternChangeEvent {
  character: string
  pattern: string
  success: boolean
  error?: string
}

export interface BatchColorChange {
  type: 'base' | 'pad' | 'pattern'
  color: string
}

// Valid colors and patterns from the game
export const VALID_COLORS = [
  'Blue', 'Violet', 'Silver', 'Charcoal', 'DeepPurple', 'Yellow', 'Pink',
  'Cyan', 'Turquoise', 'Periwinkle', 'Gold', 'Burgundy', 'Green',
  'Lavender', 'Tan', 'Red', 'SpringGreen', 'Indigo', 'Orange'
]

export const VALID_PATTERNS = [
  'Flowers', 'Horseshoes', 'Hearts', 'Lightning', 'Patches',
  'Polka-Dots', 'Notes', 'Sparkles', 'Spots', 'Stars', 'Swirls'
]

/**
 * Doodle Command Service
 * 
 * @description Implements the Doodle command functionality for changing
 * avatar skin colors and patterns. This service provides the interface
 * for the critical feature mentioned by the user - the ability to change
 * skin color by interacting with Doodle.
 * 
 * @inputs
 * - Character name and identification
 * - Color type (base, pad, pattern) and new color value
 * - Pattern name for pattern changes
 * - Server endpoint for API communication
 * 
 * @outputs
 * - Success/failure responses for color and pattern changes
 * - Real-time updates via WebSocket for other players' changes
 * - Event emissions for UI updates and game logic
 * 
 * @sideEffects
 * - Makes HTTP requests to server API endpoints
 * - Establishes WebSocket connection for real-time updates
 * - Emits events for successful/failed operations
 * - Broadcasts changes to other players in real-time
 * 
 * @units
 * - Character names: string identifiers
 * - Color names: string values from VALID_COLORS
 * - Pattern names: string values from VALID_PATTERNS
 * - API endpoints: string URLs
 * - Timestamps: Unix timestamps (milliseconds)
 * 
 * @example
 * ```typescript
 * const doodleService = new DoodleCommandService({
 *   characterId: 'flora-123',
 *   serverEndpoint: 'https://tootsville.net'
 * })
 * 
 * // Change base skin color
 * const result = await doodleService.changeSkinColor('Flora', 'base', 'Pink')
 * 
 * // Change pattern
 * const patternResult = await doodleService.changePattern('Flora', 'Hearts')
 * 
 * // Listen for real-time updates
 * doodleService.on('colorChanged', (update) => {
 *   console.log(`${update.character} changed ${update.colorType} to ${update.color}`)
 * })
 * ```
 */
export class DoodleCommandService extends EventEmitter {
  private config: DoodleConfig
  private websocket: WebSocket | null = null
  private isConnected = false

  constructor(config: DoodleConfig) {
    super()
    this.config = config
    this.connectWebSocket()
  }

  /**
   * Change the skin color of a character
   * 
   * @param character - The name of the character to modify
   * @param colorType - The type of color to change (base, pad, pattern)
   * @param color - The new color value
   * @returns Promise<DoodleResponse> - Success/failure response
   */
  async changeSkinColor(
    character: string,
    colorType: 'base' | 'pad' | 'pattern',
    color: string
  ): Promise<DoodleResponse> {
    // Validate character name
    if (!this.isValidCharacterName(character)) {
      throw new Error('Invalid character name')
    }

    // Validate color
    if (!VALID_COLORS.includes(color)) {
      return {
        success: false,
        error: `Invalid color: ${color}. Valid colors are: ${VALID_COLORS.join(', ')}`
      }
    }

    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/doodle`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.config.characterId}`
        },
        body: JSON.stringify({
          character,
          colorType,
          color
        })
      })

      const result = await response.json()

      if (result.success) {
        // Broadcast change to other players
        this.broadcastColorChange(character, colorType, color)
        
        // Emit success event
        this.emit('colorChangeSuccess', {
          character,
          colorType,
          color,
          success: true
        } as ColorChangeEvent)
      } else {
        // Emit error event
        this.emit('colorChangeError', {
          character,
          colorType,
          color,
          success: false,
          error: result.error
        } as ColorChangeEvent)
      }

      return result
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unknown error'
      
      // Emit error event
      this.emit('colorChangeError', {
        character,
        colorType,
        color,
        success: false,
        error: errorMessage
      } as ColorChangeEvent)

      return {
        success: false,
        error: errorMessage
      }
    }
  }

  /**
   * Change the pattern of a character
   * 
   * @param character - The name of the character to modify
   * @param pattern - The new pattern name
   * @returns Promise<DoodleResponse> - Success/failure response
   */
  async changePattern(character: string, pattern: string): Promise<DoodleResponse> {
    // Validate character name
    if (!this.isValidCharacterName(character)) {
      throw new Error('Invalid character name')
    }

    // Validate pattern
    if (!VALID_PATTERNS.includes(pattern)) {
      return {
        success: false,
        error: `Invalid pattern: ${pattern}. Valid patterns are: ${VALID_PATTERNS.join(', ')}`
      }
    }

    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/doodle-pattern`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.config.characterId}`
        },
        body: JSON.stringify({
          character,
          pattern
        })
      })

      const result = await response.json()

      if (result.success) {
        // Broadcast change to other players
        this.broadcastPatternChange(character, pattern)
        
        // Emit success event
        this.emit('patternChangeSuccess', {
          character,
          pattern,
          success: true
        } as PatternChangeEvent)
      } else {
        // Emit error event
        this.emit('patternChangeError', {
          character,
          pattern,
          success: false,
          error: result.error
        } as PatternChangeEvent)
      }

      return result
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unknown error'
      
      // Emit error event
      this.emit('patternChangeError', {
        character,
        pattern,
        success: false,
        error: errorMessage
      } as PatternChangeEvent)

      return {
        success: false,
        error: errorMessage
      }
    }
  }

  /**
   * Change multiple colors in a single batch operation
   * 
   * @param character - The name of the character to modify
   * @param colorChanges - Array of color changes to apply
   * @returns Promise<DoodleResponse[]> - Array of success/failure responses
   */
  async changeMultipleColors(
    character: string,
    colorChanges: BatchColorChange[]
  ): Promise<DoodleResponse[]> {
    const results: DoodleResponse[] = []

    for (const change of colorChanges) {
      const result = await this.changeSkinColor(character, change.type, change.color)
      results.push(result)
    }

    return results
  }

  /**
   * Get available colors for a specific color type
   * 
   * @param colorType - The type of color (base, pad, pattern)
   * @returns string[] - Array of valid colors for the type
   */
  getAvailableColors(colorType: 'base' | 'pad' | 'pattern'): string[] {
    switch (colorType) {
      case 'base':
        return VALID_COLORS
      case 'pad':
        // Pad colors exclude some colors that don't work well as pad colors
        return VALID_COLORS.filter(color => 
          !['DeepPurple', 'Charcoal', 'Burgundy'].includes(color)
        )
      case 'pattern':
        return VALID_COLORS
      default:
        return []
    }
  }

  /**
   * Get all available patterns
   * 
   * @returns string[] - Array of valid patterns
   */
  getAvailablePatterns(): string[] {
    return [...VALID_PATTERNS]
  }

  /**
   * Validate if a character name is valid
   * 
   * @param character - The character name to validate
   * @returns boolean - True if valid, false otherwise
   */
  private isValidCharacterName(character: string): boolean {
    // Character names should be 3-32 characters, letters, hyphens, and numbers only
    const validCharacterRegex = /^[a-zA-Z0-9\-]{3,32}$/
    return validCharacterRegex.test(character)
  }

  /**
   * Connect to WebSocket for real-time updates
   */
  private connectWebSocket(): void {
    try {
      const wsUrl = this.config.serverEndpoint.replace('https://', 'wss://').replace('http://', 'ws://')
      this.websocket = new WebSocket(`${wsUrl}/ws/doodle`)
      
      this.websocket.onopen = () => {
        this.isConnected = true
        this.emit('connected')
      }

      this.websocket.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data)
          this.handleWebSocketMessage(data)
        } catch (error) {
          console.error('Failed to parse WebSocket message:', error)
        }
      }

      this.websocket.onclose = () => {
        this.isConnected = false
        this.emit('disconnected')
        
        // Attempt to reconnect after a delay
        setTimeout(() => {
          this.connectWebSocket()
        }, 5000)
      }

      this.websocket.onerror = (error) => {
        console.error('WebSocket error:', error)
        this.emit('error', error)
      }
    } catch (error) {
      console.error('Failed to connect WebSocket:', error)
    }
  }

  /**
   * Handle incoming WebSocket messages
   * 
   * @param data - The parsed message data
   */
  private handleWebSocketMessage(data: any): void {
    switch (data.type) {
      case 'colorChanged':
        this.emit('colorChanged', {
          character: data.character,
          colorType: data.colorType,
          color: data.color,
          timestamp: data.timestamp
        })
        break
      
      case 'patternChanged':
        this.emit('patternChanged', {
          character: data.character,
          pattern: data.pattern,
          timestamp: data.timestamp
        })
        break
      
      default:
        console.warn('Unknown WebSocket message type:', data.type)
    }
  }

  /**
   * Broadcast color change to other players
   * 
   * @param character - The character that was changed
   * @param colorType - The type of color that was changed
   * @param color - The new color value
   */
  private broadcastColorChange(
    character: string,
    colorType: 'base' | 'pad' | 'pattern',
    color: string
  ): void {
    if (this.websocket && this.isConnected) {
      this.websocket.send(JSON.stringify({
        type: 'colorChanged',
        character,
        colorType,
        color,
        timestamp: Date.now()
      }))
    }
  }

  /**
   * Broadcast pattern change to other players
   * 
   * @param character - The character that was changed
   * @param pattern - The new pattern value
   */
  private broadcastPatternChange(character: string, pattern: string): void {
    if (this.websocket && this.isConnected) {
      this.websocket.send(JSON.stringify({
        type: 'patternChanged',
        character,
        pattern,
        timestamp: Date.now()
      }))
    }
  }

  /**
   * Disconnect from WebSocket and clean up
   */
  disconnect(): void {
    if (this.websocket) {
      this.websocket.close()
      this.websocket = null
    }
    this.isConnected = false
  }

  /**
   * Check if the service is connected to the WebSocket
   * 
   * @returns boolean - True if connected, false otherwise
   */
  isWebSocketConnected(): boolean {
    return this.isConnected
  }
}

export default DoodleCommandService


