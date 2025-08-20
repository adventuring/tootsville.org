/**
 * ClientBindings - Universal binding layer for JavaScript and JSCL Lisp
 * Provides unified access to all client-side objects and services
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import communicationService from '../services/CommunicationService';
import useGameStore from '../stores/GameStore';
import { GameWorld } from '../components/GameWorld';
import { AvatarManager } from '../components/AvatarManager';
import { WorldTerrain } from '../components/WorldTerrain';
import { ItemManager } from '../components/ItemManager';
import { WeatherSystem } from '../components/WeatherSystem';

/**
 * @class ClientBindings
 * @description Universal binding layer providing access to all client-side objects
 * 
 * This class creates a unified interface that can be accessed from both JavaScript
 * and JSCL Lisp code. It maintains the existing Tootsville protocol while providing
 * modern reactive state management and 3D rendering capabilities.
 * 
 * @example
 * // JavaScript usage
 * const bindings = new ClientBindings();
 * bindings.game.moveAvatar(x, y, z);
 * 
 * @example
 * // JSCL Lisp usage
 * (defvar *bindings* (new ClientBindings))
 * (send *bindings* :game :moveAvatar x y z)
 */
class ClientBindings {
  constructor() {
    this.gameStore = useGameStore;
    this.communication = communicationService;
    this.components = {
      gameWorld: null,
      avatarManager: null,
      worldTerrain: null,
      itemManager: null,
      weatherSystem: null
    };
    
    this._initializeBindings();
    this._setupGlobalAccess();
  }

  /**
   * @method _initializeBindings
   * @private
   * @description Initialize all component bindings and event handlers
   */
  _initializeBindings() {
    // Initialize component references
    this.components.gameWorld = new GameWorld();
    this.components.avatarManager = new AvatarManager();
    this.components.worldTerrain = new WorldTerrain();
    this.components.itemManager = new ItemManager();
    this.components.weatherSystem = new WeatherSystem();
    
    // Set up communication event handlers
    this.communication.on('avatarUpdate', this._handleAvatarUpdate.bind(this));
    this.communication.on('worldUpdate', this._handleWorldUpdate.bind(this));
    this.communication.on('itemUpdate', this._handleItemUpdate.bind(this));
    this.communication.on('weatherUpdate', this._handleWeatherUpdate.bind(this));
  }

  /**
   * @method _setupGlobalAccess
   * @private
   * @description Set up global access for both JavaScript and JSCL
   */
  _setupGlobalAccess() {
    // Make bindings available globally for JavaScript
    if (typeof window !== 'undefined') {
      window.TootsvilleBindings = this;
      window.Tootsville = {
        game: this.game,
        communication: this.communication,
        components: this.components,
        utils: this.utils
      };
    }
    
    // Make bindings available for JSCL
    if (typeof jscl !== 'undefined') {
      jscl.TootsvilleBindings = this;
      jscl.Tootsville = window.Tootsville;
    }
  }

  /**
   * @property game
   * @description Game state and actions interface
   * 
   * Provides access to all game-related functionality including:
   * - Avatar movement and control
   * - World interaction
   * - Inventory management
   * - Chat and communication
   */
  get game() {
    return {
      /**
       * @method moveAvatar
       * @param {number} x - X coordinate
       * @param {number} y - Y coordinate  
       * @param {number} z - Z coordinate
       * @description Move the player's avatar to specified coordinates
       */
      moveAvatar: (x, y, z) => {
        this.gameStore.getState().actions.moveAvatar(x, y, z);
        this.communication.send('move', { x, y, z });
      },

      /**
       * @method teleport
       * @param {string} world - Target world name
       * @param {number} x - X coordinate
       * @param {number} y - Y coordinate
       * @param {number} z - Z coordinate
       * @description Teleport avatar to different world and coordinates
       */
      teleport: (world, x, y, z) => {
        this.gameStore.getState().actions.teleport(world, x, y, z);
        this.communication.send('teleport', { world, x, y, z });
      },

      /**
       * @method sendMessage
       * @param {string} message - Message text
       * @param {string} type - Message type (public, private, world)
       * @description Send a chat message
       */
      sendMessage: (message, type = 'public') => {
        this.gameStore.getState().actions.sendMessage(message, type);
        this.communication.send('message', { message, type });
      },

      /**
       * @method useItem
       * @param {string} itemId - Item identifier
       * @param {Object} target - Target object or coordinates
       * @description Use an item from inventory
       */
      useItem: (itemId, target) => {
        this.gameStore.getState().actions.useItem(itemId, target);
        this.communication.send('useItem', { itemId, target });
      },

      /**
       * @method getInventory
       * @returns {Array} Current inventory items
       * @description Get current inventory contents
       */
      getInventory: () => {
        return this.gameStore.getState().inventory;
      },

      /**
       * @method getPosition
       * @returns {Object} Current avatar position {x, y, z, world}
       * @description Get current avatar position
       */
      getPosition: () => {
        const state = this.gameStore.getState();
        return {
          x: state.activity.long,
          y: state.activity.alt,
          z: state.activity.lat,
          world: state.activity.world
        };
      },

      /**
       * @method getNearbyAvatars
       * @returns {Array} Array of nearby avatar objects
       * @description Get list of avatars in proximity
       */
      getNearbyAvatars: () => {
        return Object.values(this.gameStore.getState().avatars);
      }
    };
  }

  /**
   * @property communication
   * @description Direct communication interface
   * 
   * Provides low-level access to the communication service for
   * custom protocol handling and debugging.
   */
  get communication() {
    return {
      /**
       * @method send
       * @param {string} command - Command name
       * @param {Object} data - Command data
       * @description Send raw command to server
       */
      send: (command, data) => {
        this.communication.send(command, data);
      },

      /**
       * @method on
       * @param {string} event - Event name
       * @param {Function} callback - Event handler
       * @description Register event handler
       */
      on: (event, callback) => {
        this.communication.on(event, callback);
      },

      /**
       * @method off
       * @param {string} event - Event name
       * @param {Function} callback - Event handler to remove
       * @description Remove event handler
       */
      off: (event, callback) => {
        this.communication.off(event, callback);
      },

      /**
       * @method connect
       * @param {string} cluster - Server cluster
       * @description Connect to server
       */
      connect: (cluster) => {
        this.communication.connect(cluster);
      },

      /**
       * @method disconnect
       * @description Disconnect from server
       */
      disconnect: () => {
        this.communication.disconnect();
      }
    };
  }

  /**
   * @property components
   * @description 3D component access interface
   * 
   * Provides direct access to 3D rendering components for
   * advanced customization and debugging.
   */
  get components() {
    return {
      /**
       * @method getGameWorld
       * @returns {GameWorld} Game world component instance
       * @description Get game world component
       */
      getGameWorld: () => this.components.gameWorld,

      /**
       * @method getAvatarManager
       * @returns {AvatarManager} Avatar manager component instance
       * @description Get avatar manager component
       */
      getAvatarManager: () => this.components.avatarManager,

      /**
       * @method getWorldTerrain
       * @returns {WorldTerrain} World terrain component instance
       * @description Get world terrain component
       */
      getWorldTerrain: () => this.components.worldTerrain,

      /**
       * @method getItemManager
       * @returns {ItemManager} Item manager component instance
       * @description Get item manager component
       */
      getItemManager: () => this.components.itemManager,

      /**
       * @method getWeatherSystem
       * @returns {WeatherSystem} Weather system component instance
       * @description Get weather system component
       */
      getWeatherSystem: () => this.components.weatherSystem
    };
  }

  /**
   * @property utils
   * @description Utility functions interface
   * 
   * Provides common utility functions for coordinate conversion,
   * distance calculation, and other helper operations.
   */
  get utils() {
    return {
      /**
       * @method calculateDistance
       * @param {Object} pos1 - First position {x, y, z}
       * @param {Object} pos2 - Second position {x, y, z}
       * @returns {number} Distance between positions
       * @description Calculate 3D distance between two positions
       */
      calculateDistance: (pos1, pos2) => {
        const dx = pos1.x - pos2.x;
        const dy = pos1.y - pos2.y;
        const dz = pos1.z - pos2.z;
        return Math.sqrt(dx * dx + dy * dy + dz * dz);
      },

      /**
       * @method worldToScreen
       * @param {Object} worldPos - World coordinates {x, y, z}
       * @returns {Object} Screen coordinates {x, y}
       * @description Convert world coordinates to screen coordinates
       */
      worldToScreen: (worldPos) => {
        // Implementation depends on current camera and viewport
        return this.components.gameWorld.worldToScreen(worldPos);
      },

      /**
       * @method screenToWorld
       * @param {Object} screenPos - Screen coordinates {x, y}
       * @returns {Object} World coordinates {x, y, z}
       * @description Convert screen coordinates to world coordinates
       */
      screenToWorld: (screenPos) => {
        // Implementation depends on current camera and viewport
        return this.components.gameWorld.screenToWorld(screenPos);
      },

      /**
       * @method formatTime
       * @param {number} timestamp - Unix timestamp
       * @returns {string} Formatted time string
       * @description Format timestamp for display
       */
      formatTime: (timestamp) => {
        return new Date(timestamp).toLocaleTimeString();
      }
    };
  }

  /**
   * @method _handleAvatarUpdate
   * @private
   * @param {Object} data - Avatar update data
   * @description Handle avatar update events from server
   */
  _handleAvatarUpdate(data) {
    this.components.avatarManager.updateAvatar(data);
  }

  /**
   * @method _handleWorldUpdate
   * @private
   * @param {Object} data - World update data
   * @description Handle world update events from server
   */
  _handleWorldUpdate(data) {
    this.components.worldTerrain.updateTerrain(data);
  }

  /**
   * @method _handleItemUpdate
   * @private
   * @param {Object} data - Item update data
   * @description Handle item update events from server
   */
  _handleItemUpdate(data) {
    this.components.itemManager.updateItem(data);
  }

  /**
   * @method _handleWeatherUpdate
   * @private
   * @param {Object} data - Weather update data
   * @description Handle weather update events from server
   */
  _handleWeatherUpdate(data) {
    this.components.weatherSystem.updateWeather(data);
  }

  /**
   * @method destroy
   * @description Clean up bindings and event handlers
   */
  destroy() {
    this.communication.off('avatarUpdate', this._handleAvatarUpdate);
    this.communication.off('worldUpdate', this._handleWorldUpdate);
    this.communication.off('itemUpdate', this._handleItemUpdate);
    this.communication.off('weatherUpdate', this._handleWeatherUpdate);
    
    if (typeof window !== 'undefined') {
      delete window.TootsvilleBindings;
      delete window.Tootsville;
    }
    
    if (typeof jscl !== 'undefined') {
      delete jscl.TootsvilleBindings;
      delete jscl.Tootsville;
    }
  }
}

// Create and export singleton instance
const clientBindings = new ClientBindings();
export default clientBindings;

// Also export the class for custom instances
export { ClientBindings };
