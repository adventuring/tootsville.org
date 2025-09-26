/**
 * World Store - Vue Composable
 *
 * Equivalent to original ActionScript WorldManager
 * Manages world zones, rooms, navigation, and spatial data
 */

import { ref, computed, reactive, readonly } from 'vue'
import { useAMF, APIEndpoint } from '../InfinityMode/AMFManager'
import { useEventSystem, EventFactory, EventType, WorldEvent } from '../InfinityMode/EventSystem'

// World and room interfaces matching original ActionScript structures
export interface World {
  id: string
  name: string
  description: string
  zone: Zone
  boundaries: WorldBoundariess
  settings: WorldSettings
  metadata: WorldMetadata
}

export interface Zone {
  id: string
  name: string
  description: string
  worldId: string
  rooms: Room[]
  boundaries: ZoneBoundaries
  isActive: boolean
  population: number
  metadata: ZoneMetadata
}

export interface Room {
  id: string
  name: string
  description: string
  zoneId: string
  type: RoomType
  capacity: number
  currentUsers: number
  isPrivate: boolean
  password?: string
  ownerId?: string
  position: RoomPosition
  boundaries: RoomBoundaries
  objects: RoomObject[]
  metadata: RoomMetadata
}

export interface RoomObject {
  id: string
  type: ObjectType
  name: string
  position: Position3D
  rotation?: Rotation3D
  scale?: Scale3D
  interactive: boolean
  metadata: ObjectMetadata
}

export interface Position3D {
  x: number
  y: number
  z: number
}

export interface Rotation3D {
  x: number
  y: number
  z: number
}

export interface Scale3D {
  x: number
  y: number
  z: number
}

export interface WorldBoundaries {
  minX: number
  maxX: number
  minY: number
  maxY: number
  minZ: number
  maxZ: number
}

export interface ZoneBoundaries {
  minX: number
  maxX: number
  minY: number
  maxY: number
  minZ: number
  maxZ: number
}

export interface RoomBoundaries {
  minX: number
  maxX: number
  minY: number
  maxY: number
  minZ: number
  maxZ: number
}

export interface RoomPosition {
  x: number
  y: number
  z: number
  zoneId: string
}

export interface WorldSettings {
  allowTeleportation: boolean
  allowPrivateRooms: boolean
  maxRoomCapacity: number
  enablePVP: boolean
  enableTrading: boolean
  timeCycle: boolean
  weatherEffects: boolean
}

export interface WorldMetadata {
  version: string
  createdAt: string
  lastModified: string
  author: string
  tags: string[]
}

export interface ZoneMetadata {
  difficulty: number
  recommendedLevel: number
  theme: string
  backgroundMusic?: string
  ambientSound?: string
}

export interface RoomMetadata {
  theme: string
  backgroundMusic?: string
  ambientSound?: string
  lighting: string
  effects: string[]
}

export interface ObjectMetadata {
  modelUrl?: string
  textureUrl?: string
  soundUrl?: string
  interactionType?: string
  interactionData?: any
}

export enum RoomType {
  PUBLIC = 'public',
  PRIVATE = 'private',
  INSTANCE = 'instance',
  HOUSE = 'house',
  SHOP = 'shop',
  MINIGAME = 'minigame'
}

export enum ObjectType {
  SCENERY = 'scenery',
  INTERACTIVE = 'interactive',
  NPC = 'npc',
  ITEM = 'item',
  PORTAL = 'portal',
  EFFECT = 'effect'
}

// Navigation state
const currentWorld = ref<World | null>(null)
const currentZone = ref<Zone | null>(null)
const currentRoom = ref<Room | null>(null)
const playerPosition = reactive<Position3D>({ x: 0, y: 0, z: 0 })
const worlds = ref<Map<string, World>>(new Map())
const rooms = ref<Map<string, Room>>(new Map())
const isLoading = ref(false)
const error = ref<string | null>(null)
const lastUpdated = ref<string | null>(null)

// Event system integration
const { emit } = useEventSystem()

/**
 * World Store Composable
 *
 * Provides reactive world management with zones, rooms, and navigation
 */
export function useWorldStore() {
  const amf = useAMF()

  // Computed properties
  const availableWorlds = computed(() => Array.from(worlds.value.values()))
  const availableRooms = computed(() => Array.from(rooms.value.values()))

  const currentWorldId = computed(() => currentWorld.value?.id || null)
  const currentZoneId = computed(() => currentZone.value?.id || null)
  const currentRoomId = computed(() => currentRoom.value?.id || null)

  const nearbyRooms = computed(() => {
    if (!currentZone.value) return []

    // Simple proximity calculation based on position
    return currentZone.value.rooms.filter(room => {
      const distance = Math.sqrt(
        Math.pow(room.position.x - playerPosition.x, 2) +
        Math.pow(room.position.y - playerPosition.y, 2) +
        Math.pow(room.position.z - playerPosition.z, 2)
      )
      return distance < 100 // Within 100 units
    })
  })

  // Readonly reactive state
  const state = readonly({
    currentWorld,
    currentZone,
    currentRoom,
    playerPosition,
    worlds,
    rooms,
    isLoading,
    error,
    lastUpdated
  })

  /**
   * Load world information
   */
  const loadWorldInfo = async (): Promise<void> => {
    try {
      isLoading.value = true
      error.value = null

      const response = await amf.getWorldInfo()

      if (response.success && response.data) {
        const worldData = response.data

        const world: World = {
          id: worldData.id,
          name: worldData.name,
          description: worldData.description,
          zone: worldData.zone || worldData.zones?.[0] || null,
          boundaries: worldData.boundaries,
          settings: {
            allowTeleportation: worldData.allowTeleportation ?? true,
            allowPrivateRooms: worldData.allowPrivateRooms ?? true,
            maxRoomCapacity: worldData.maxRoomCapacity || 50,
            enablePVP: worldData.enablePVP ?? false,
            enableTrading: worldData.enableTrading ?? true,
            timeCycle: worldData.timeCycle ?? true,
            weatherEffects: worldData.weatherEffects ?? true
          },
          metadata: {
            version: worldData.version || '1.0.0',
            createdAt: worldData.createdAt,
            lastModified: worldData.lastModified,
            author: worldData.author || 'Tootsville',
            tags: worldData.tags || []
          }
        }

        worlds.value.set(world.id, world)
        currentWorld.value = world
        currentZone.value = world.zone
        lastUpdated.value = new Date().toISOString()

        emit(EventFactory.worldLoad(world.id, { world }))

        // Load zone data for this world
        await loadZoneList(world.id)
      } else {
        error.value = response.error?.message || 'Failed to load world info'
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to load world info'
      console.error('World load error:', err)
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Load zone data
   */
  const loadZoneList = async (worldId?: string): Promise<void> => {
    try {
      const response = await amf.getZoneList()

      if (response.success && response.data?.zones) {
        const targetWorldId = worldId || currentWorld.value?.id
        if (!targetWorldId) return

        // Find the zone for this world (should be only one)
        const zoneData = response.data.zones.find((z: any) => z.worldId === targetWorldId)
        
        if (zoneData) {
          const zone: Zone = {
            id: zoneData.id,
            name: zoneData.name,
            description: zoneData.description,
            worldId: zoneData.worldId,
            rooms: zoneData.rooms || [],
            boundaries: zoneData.boundaries,
            isActive: zoneData.isActive ?? true,
            population: zoneData.population || 0,
            metadata: {
              difficulty: zoneData.difficulty || 1,
              recommendedLevel: zoneData.recommendedLevel || 1,
              theme: zoneData.theme || 'default',
              backgroundMusic: zoneData.backgroundMusic,
              ambientSound: zoneData.ambientSound
            }
          }

          // Update current zone
          currentZone.value = zone

          // Load rooms for this zone
          zone.rooms.forEach(roomData => {
            const room: Room = {
              id: roomData.id,
              name: roomData.name,
              description: roomData.description,
              zoneId: zone.id,
              type: roomData.type,
              capacity: roomData.capacity || 50,
              currentUsers: roomData.currentUsers || 0,
              isPrivate: roomData.isPrivate || false,
              password: roomData.password,
              ownerId: roomData.ownerId,
              position: roomData.position,
              boundaries: roomData.boundaries,
              objects: roomData.objects || [],
              metadata: {
                theme: roomData.theme || 'default',
                backgroundMusic: roomData.backgroundMusic,
                ambientSound: roomData.ambientSound,
                lighting: roomData.lighting || 'default',
                effects: roomData.effects || []
              }
            }
            rooms.value.set(room.id, room)
          })
        }
      }
    } catch (err) {
      console.error('Zone load error:', err)
    }
  }


  /**
   * Enter a room
   */
  const enterRoom = async (roomId: string, password?: string): Promise<boolean> => {
    const room = rooms.value.get(roomId)
    if (!room) {
      error.value = 'Room not found'
      return false
    }

    // Check capacity
    if (room.currentUsers >= room.capacity) {
      error.value = 'Room is full'
      return false
    }

    // Check privacy
    if (room.isPrivate && !password) {
      error.value = 'Password required for private room'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      const response = await amf.post(APIEndpoint.TELEPORT, {
        roomId,
        password,
        position: playerPosition
      })

      if (response.success) {
        currentRoom.value = room
        playerPosition.x = room.position.x
        playerPosition.y = room.position.y
        playerPosition.z = room.position.z

        // Update room user count
        room.currentUsers += 1

        emit(new WorldEvent(
          EventType.SCENE_UPDATE,
          currentWorld.value!.id,
          {
            zoneId: currentZone.value!.id,
            data: { room, action: 'entered' }
          }
        ))

        return true
      } else {
        error.value = response.error?.message || 'Failed to enter room'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to enter room'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Leave current room
   */
  const leaveRoom = async (): Promise<boolean> => {
    if (!currentRoom.value) {
      error.value = 'Not in a room'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      // Call server to leave room
      const response = await amf.post(`${APIEndpoint.TELEPORT}/leave`, {
        roomId: currentRoom.value.id
      })

      if (response.success) {
        const room = currentRoom.value
        room.currentUsers = Math.max(0, room.currentUsers - 1)
        currentRoom.value = null

        emit(new WorldEvent(
          EventType.SCENE_UPDATE,
          currentWorld.value!.id,
          {
            zoneId: currentZone.value!.id,
            data: { room, action: 'left' }
          }
        ))

        return true
      } else {
        error.value = response.error?.message || 'Failed to leave room'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to leave room'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Teleport to position
   */
  const teleport = async (position: Position3D, zoneId?: string, roomId?: string): Promise<boolean> => {
    try {
      isLoading.value = true
      error.value = null

      const response = await amf.post(APIEndpoint.TELEPORT, {
        position,
        zoneId: zoneId || currentZone.value?.id,
        roomId: roomId || currentRoom.value?.id
      })

      if (response.success) {
        // Update local position
        playerPosition.x = position.x
        playerPosition.y = position.y
        playerPosition.z = position.z

        // Note: Zone changes are no longer supported as there is only one zone

        if (roomId && roomId !== currentRoom.value?.id) {
          await enterRoom(roomId)
        }

        emit(new WorldEvent(
          EventType.PLAYER_MOVE,
          currentWorld.value!.id,
          {
            zoneId: currentZone.value!.id,
            position,
            data: { teleport: true }
          }
        ))

        return true
      } else {
        error.value = response.error?.message || 'Teleport failed'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Teleport failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Update player position
   */
  const updatePosition = (position: Position3D): void => {
    playerPosition.x = position.x
    playerPosition.y = position.y
    playerPosition.z = position.z

    // Emit position update event (throttled)
    emit(new WorldEvent(
      EventType.PLAYER_MOVE,
      currentWorld.value!.id,
      {
        zoneId: currentZone.value!.id,
        position,
        data: { continuous: true }
      }
    ))
  }

  /**
   * Get room by ID
   */
  const getRoom = (roomId: string): Room | undefined => {
    return rooms.value.get(roomId)
  }

  /**
   * Get current zone
   */
  const getZone = (): Zone | null => {
    return currentZone.value
  }

  /**
   * Get world by ID
   */
  const getWorld = (worldId: string): World | undefined => {
    return worlds.value.get(worldId)
  }

  /**
   * Get rooms in current zone
   */
  const getRoomsInCurrentZone = computed(() => {
    if (!currentZone.value) return []
    return currentZone.value.rooms
  })

  /**
   * Get current zone
   */
  const getCurrentZone = computed(() => {
    return currentZone.value
  })

  /**
   * Check if position is within boundaries
   */
  const isWithinBoundaries = (position: Position3D, boundaries: WorldBoundaries | ZoneBoundaries | RoomBoundaries): boolean => {
    return position.x >= boundaries.minX && position.x <= boundaries.maxX &&
           position.y >= boundaries.minY && position.y <= boundaries.maxY &&
           position.z >= boundaries.minZ && position.z <= boundaries.maxZ
  }

  /**
   * Find nearest room to position
   */
  const findNearestRoom = (position: Position3D): Room | null => {
    if (!currentZone.value) return null

    let nearestRoom: Room | null = null
    let nearestDistance = Infinity

    currentZone.value.rooms.forEach(room => {
      const distance = Math.sqrt(
        Math.pow(room.position.x - position.x, 2) +
        Math.pow(room.position.y - position.y, 2) +
        Math.pow(room.position.z - position.z, 2)
      )

      if (distance < nearestDistance) {
        nearestDistance = distance
        nearestRoom = room
      }
    })

    return nearestRoom
  }

  /**
   * Get room info from server
   */
  const getRoomInfo = async (roomId: string): Promise<Room | null> => {
    try {
      const response = await amf.get(`${APIEndpoint.GET_ROOM_INFO}/${roomId}`)

      if (response.success && response.data) {
        const roomData = response.data
        const room: Room = {
          id: roomData.id,
          name: roomData.name,
          description: roomData.description,
          zoneId: roomData.zoneId,
          type: roomData.type,
          capacity: roomData.capacity,
          currentUsers: roomData.currentUsers,
          isPrivate: roomData.isPrivate,
          position: roomData.position,
          boundaries: roomData.boundaries,
          objects: roomData.objects || [],
          metadata: roomData.metadata
        }

        rooms.value.set(room.id, room)
        return room
      }
    } catch (err) {
      console.error('Failed to get room info:', err)
    }

    return null
  }

  /**
   * Clear all world data
   */
  const clearWorldData = (): void => {
    currentWorld.value = null
    currentZone.value = null
    currentRoom.value = null
    playerPosition.x = 0
    playerPosition.y = 0
    playerPosition.z = 0
    worlds.value.clear()
    rooms.value.clear()
    error.value = null
    lastUpdated.value = null
  }

  return {
    // State
    state,
    availableWorlds,
    availableZones,
    availableRooms,
    currentWorldId,
    currentZoneId,
    currentRoomId,
    nearbyRooms,
    getRoomsInCurrentZone,
    getZonesInCurrentWorld,

    // Methods
    loadWorldInfo,
    loadZoneList,
    enterZone,
    enterRoom,
    leaveRoom,
    teleport,
    updatePosition,
    getRoom,
    getZone,
    getWorld,
    isWithinBoundaries,
    findNearestRoom,
    getRoomInfo,
    clearWorldData
  }
}

// Singleton instance
let worldStoreInstance: ReturnType<typeof useWorldStore> | null = null

/**
 * Get singleton world store instance
 */
export function getWorldStore(): ReturnType<typeof useWorldStore> {
  if (!worldStoreInstance) {
    worldStoreInstance = useWorldStore()
  }
  return worldStoreInstance
}



