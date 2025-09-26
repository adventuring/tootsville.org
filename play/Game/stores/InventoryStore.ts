/**
 * Inventory Store - Vue Composable
 *
 * Equivalent to original ActionScript InventoryManager
 * Manages user inventory, equipment, and item interactions
 */

import { ref, computed, reactive, readonly } from 'vue'
import { useAMF, APIEndpoint } from '../InfinityMode/AMFManager'
import { useEventSystem, EventFactory, EventType, EquipmentEvent } from '../InfinityMode/EventSystem'

// Item and equipment interfaces matching original ActionScript structures
export interface InventoryItem {
  id: string
  itemId: string
  name: string
  description: string
  type: ItemType
  rarity: ItemRarity
  quantity: number
  maxStack: number
  equipped: boolean
  slot?: EquipmentSlot
  attributes: ItemAttributes
  metadata: ItemMetadata
}

export interface ItemAttributes {
  power?: number
  defense?: number
  speed?: number
  luck?: number
  durability?: number
  maxDurability?: number
  effect?: string
  effectDuration?: number
  value?: number
}

export interface ItemMetadata {
  iconUrl?: string
  modelUrl?: string
  soundUrl?: string
  createdAt: string
  obtainedFrom?: string
  isTradable: boolean
  isConsumable: boolean
  cooldown?: number
}

export enum ItemType {
  WEAPON = 'weapon',
  ARMOR = 'armor',
  ACCESSORY = 'accessory',
  CONSUMABLE = 'consumable',
  MATERIAL = 'material',
  QUEST = 'quest',
  COSMETIC = 'cosmetic'
}

export enum ItemRarity {
  COMMON = 'common',
  UNCOMMON = 'uncommon',
  RARE = 'rare',
  EPIC = 'epic',
  LEGENDARY = 'legendary'
}

export enum EquipmentSlot {
  HEAD = 'head',
  BODY = 'body',
  HANDS = 'hands',
  FEET = 'feet',
  WEAPON = 'weapon',
  ACCESSORY1 = 'accessory1',
  ACCESSORY2 = 'accessory2'
}

// Equipment set for character
export interface EquipmentSet {
  [EquipmentSlot.HEAD]?: InventoryItem
  [EquipmentSlot.BODY]?: InventoryItem
  [EquipmentSlot.HANDS]?: InventoryItem
  [EquipmentSlot.FEET]?: InventoryItem
  [EquipmentSlot.WEAPON]?: InventoryItem
  [EquipmentSlot.ACCESSORY1]?: InventoryItem
  [EquipmentSlot.ACCESSORY2]?: InventoryItem
}

// Inventory state
const items = ref<Map<string, InventoryItem>>(new Map())
const equipment = reactive<EquipmentSet>({})
const isLoading = ref(false)
const error = ref<string | null>(null)
const lastUpdated = ref<string | null>(null)

// Item usage cooldowns
const itemCooldowns = ref<Map<string, number>>(new Map())

// Event system integration
const { emit } = useEventSystem()

/**
 * Inventory Store Composable
 *
 * Provides reactive inventory management with equipment, item usage, and trading
 */
export function useInventoryStore() {
  const amf = useAMF()

  // Computed properties
  const inventoryItems = computed(() => Array.from(items.value.values()))
  const equippedItems = computed(() => Object.values(equipment).filter(item => item !== undefined))
  const totalItems = computed(() => items.value.size)
  const equippedCount = computed(() => equippedItems.value.length)

  // Inventory statistics
  const inventoryStats = computed(() => {
    const stats = {
      totalValue: 0,
      byType: {} as Record<ItemType, number>,
      byRarity: {} as Record<ItemRarity, number>,
      equippedValue: 0
    }

    items.value.forEach(item => {
      // Total value
      stats.totalValue += (item.attributes.value || 0) * item.quantity

      // Count by type
      stats.byType[item.type] = (stats.byType[item.type] || 0) + item.quantity

      // Count by rarity
      stats.byRarity[item.rarity] = (stats.byRarity[item.rarity] || 0) + item.quantity
    })

    // Equipped value
    equippedItems.value.forEach(item => {
      stats.equippedValue += item.attributes.value || 0
    })

    return stats
  })

  // Readonly reactive state
  const state = readonly({
    items,
    equipment,
    isLoading,
    error,
    lastUpdated
  })

  /**
   * Load inventory from server
   */
  const loadInventory = async (): Promise<void> => {
    try {
      isLoading.value = true
      error.value = null

      const response = await amf.getInventory()

      if (response.success && response.data?.items) {
        // Clear existing items
        items.value.clear()

        // Load items
        response.data.items.forEach((itemData: any) => {
          const item: InventoryItem = {
            id: itemData.id,
            itemId: itemData.itemId,
            name: itemData.name,
            description: itemData.description,
            type: itemData.type,
            rarity: itemData.rarity,
            quantity: itemData.quantity,
            maxStack: itemData.maxStack || 1,
            equipped: itemData.equipped || false,
            slot: itemData.slot,
            attributes: itemData.attributes || {},
            metadata: {
              ...itemData.metadata,
              createdAt: itemData.createdAt,
              isTradable: itemData.isTradable ?? true,
              isConsumable: itemData.isConsumable ?? false
            }
          }

          items.value.set(item.id, item)

          // Add to equipment if equipped
          if (item.equipped && item.slot) {
            equipment[item.slot] = item
          }
        })

        lastUpdated.value = new Date().toISOString()

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'loaded',
          itemCount: items.value.size
        }))
      } else {
        error.value = response.error?.message || 'Failed to load inventory'
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to load inventory'
      console.error('Inventory load error:', err)
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Use an item
   */
  const useItem = async (itemId: string, target?: any): Promise<boolean> => {
    const item = items.value.get(itemId)
    if (!item) {
      error.value = 'Item not found'
      return false
    }

    // Check if item is on cooldown
    const cooldown = itemCooldowns.value.get(itemId)
    if (cooldown && Date.now() < cooldown) {
      error.value = 'Item is on cooldown'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      const response = await amf.useItem(itemId, target)

      if (response.success) {
        // Update item quantity
        if (item.metadata.isConsumable) {
          item.quantity -= 1

          if (item.quantity <= 0) {
            items.value.delete(itemId)
          }
        }

        // Set cooldown if applicable
        if (item.metadata.cooldown) {
          itemCooldowns.value.set(itemId, Date.now() + item.metadata.cooldown)
        }

        // Update durability if applicable
        if (item.attributes.durability && item.attributes.maxDurability) {
          item.attributes.durability -= 1

          if (item.attributes.durability <= 0) {
            // Item broke
            unequipItem(itemId)
            items.value.delete(itemId)
          }
        }

        // Emit equipment effect event
        emit(EventFactory.equipmentEffect(
          item.itemId,
          item.name,
          item.attributes.effect || 'used',
          target?.id
        ))

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'used',
          itemId,
          itemName: item.name
        }))

        return true
      } else {
        error.value = response.error?.message || 'Failed to use item'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to use item'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Equip an item
   */
  const equipItem = async (itemId: string): Promise<boolean> => {
    const item = items.value.get(itemId)
    if (!item) {
      error.value = 'Item not found'
      return false
    }

    if (!item.slot) {
      error.value = 'Item cannot be equipped'
      return false
    }

    // Check if slot is already occupied
    const currentEquipped = equipment[item.slot]
    if (currentEquipped) {
      await unequipItem(currentEquipped.id)
    }

    try {
      isLoading.value = true
      error.value = null

      // Call server to equip item
      const response = await amf.post(`${APIEndpoint.USE_ITEM}/equip`, {
        itemId,
        slot: item.slot
      })

      if (response.success) {
        // Update local state
        item.equipped = true
        equipment[item.slot] = item

        emit(new EquipmentEvent(
          EventType.ITEM_EQUIP,
          item.itemId,
          item.name,
          {
            target: item,
            data: { slot: item.slot }
          }
        ))

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'equipped',
          itemId,
          slot: item.slot
        }))

        return true
      } else {
        error.value = response.error?.message || 'Failed to equip item'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to equip item'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Unequip an item
   */
  const unequipItem = async (itemId: string): Promise<boolean> => {
    const item = items.value.get(itemId)
    if (!item || !item.equipped || !item.slot) {
      error.value = 'Item not equipped'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      // Call server to unequip item
      const response = await amf.post(`${APIEndpoint.USE_ITEM}/unequip`, {
        itemId
      })

      if (response.success) {
        // Update local state
        item.equipped = false
        delete equipment[item.slot]

        emit(new EquipmentEvent(
          EventType.ITEM_UNEQUIP,
          item.itemId,
          item.name,
          {
            target: item,
            data: { slot: item.slot }
          }
        ))

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'unequipped',
          itemId,
          slot: item.slot
        }))

        return true
      } else {
        error.value = response.error?.message || 'Failed to unequip item'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Failed to unequip item'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Buy an item from store
   */
  const buyItem = async (itemId: string, quantity: number = 1): Promise<boolean> => {
    try {
      isLoading.value = true
      error.value = null

      const response = await amf.buyItem(itemId, quantity)

      if (response.success) {
        // Refresh inventory to get updated items
        await loadInventory()

        emit(new CustomEvent(EventType.PURCHASE_SUCCESS, {
          itemId,
          quantity,
          cost: response.data?.cost
        }))

        return true
      } else {
        error.value = response.error?.message || 'Purchase failed'
        emit(new CustomEvent(EventType.PURCHASE_FAILURE, {
          itemId,
          quantity,
          error: error.value
        }))
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Purchase failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Sell an item
   */
  const sellItem = async (itemId: string, quantity: number = 1): Promise<boolean> => {
    const item = items.value.get(itemId)
    if (!item) {
      error.value = 'Item not found'
      return false
    }

    if (item.quantity < quantity) {
      error.value = 'Not enough items'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      const response = await amf.post(`${APIEndpoint.SELL_ITEM}`, {
        itemId,
        quantity
      })

      if (response.success) {
        // Update local quantity
        item.quantity -= quantity
        if (item.quantity <= 0) {
          items.value.delete(itemId)
        }

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'sold',
          itemId,
          quantity
        }))

        return true
      } else {
        error.value = response.error?.message || 'Sale failed'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Sale failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Gift an item to another player
   */
  const giftItem = async (itemId: string, recipientId: string, quantity: number = 1, message?: string): Promise<boolean> => {
    const item = items.value.get(itemId)
    if (!item) {
      error.value = 'Item not found'
      return false
    }

    if (!item.metadata.isTradable) {
      error.value = 'Item cannot be traded'
      return false
    }

    if (item.quantity < quantity) {
      error.value = 'Not enough items'
      return false
    }

    try {
      isLoading.value = true
      error.value = null

      const response = await amf.giftItem(itemId, {
        recipientId,
        quantity,
        message
      })

      if (response.success) {
        // Update local quantity
        item.quantity -= quantity
        if (item.quantity <= 0) {
          items.value.delete(itemId)
        }

        emit(new CustomEvent(EventType.INVENTORY_UPDATE, {
          action: 'gifted',
          itemId,
          recipientId,
          quantity
        }))

        return true
      } else {
        error.value = response.error?.message || 'Gift failed'
        return false
      }
    } catch (err: any) {
      error.value = err.message || 'Gift failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Get item by ID
   */
  const getItem = (itemId: string): InventoryItem | undefined => {
    return items.value.get(itemId)
  }

  /**
   * Get items by type
   */
  const getItemsByType = (type: ItemType): InventoryItem[] => {
    return inventoryItems.value.filter(item => item.type === type)
  }

  /**
   * Get items by rarity
   */
  const getItemsByRarity = (rarity: ItemRarity): InventoryItem[] => {
    return inventoryItems.value.filter(item => item.rarity === rarity)
  }

  /**
   * Check if item can be equipped
   */
  const canEquipItem = (itemId: string): boolean => {
    const item = items.value.get(itemId)
    return !!(item && item.slot && !item.equipped)
  }

  /**
   * Check if item can be used
   */
  const canUseItem = (itemId: string): boolean => {
    const item = items.value.get(itemId)
    if (!item) return false

    // Check cooldown
    const cooldown = itemCooldowns.value.get(itemId)
    if (cooldown && Date.now() < cooldown) return false

    // Check quantity
    return item.quantity > 0
  }

  /**
   * Get remaining cooldown for item
   */
  const getItemCooldown = (itemId: string): number => {
    const cooldown = itemCooldowns.value.get(itemId)
    if (!cooldown) return 0

    const remaining = cooldown - Date.now()
    return Math.max(0, remaining)
  }

  /**
   * Calculate total equipment stats
   */
  const getEquipmentStats = computed(() => {
    const stats = {
      power: 0,
      defense: 0,
      speed: 0,
      luck: 0
    }

    equippedItems.value.forEach(item => {
      stats.power += item.attributes.power || 0
      stats.defense += item.attributes.defense || 0
      stats.speed += item.attributes.speed || 0
      stats.luck += item.attributes.luck || 0
    })

    return stats
  })

  /**
   * Clear all inventory data
   */
  const clearInventory = (): void => {
    items.value.clear()
    Object.keys(equipment).forEach(key => {
      delete equipment[key as EquipmentSlot]
    })
    itemCooldowns.value.clear()
    error.value = null
    lastUpdated.value = null
  }

  return {
    // State
    state,
    inventoryItems,
    equippedItems,
    totalItems,
    equippedCount,
    inventoryStats,
    getEquipmentStats,

    // Methods
    loadInventory,
    useItem,
    equipItem,
    unequipItem,
    buyItem,
    sellItem,
    giftItem,
    getItem,
    getItemsByType,
    getItemsByRarity,
    canEquipItem,
    canUseItem,
    getItemCooldown,
    clearInventory
  }
}

// Singleton instance
let inventoryStoreInstance: ReturnType<typeof useInventoryStore> | null = null

/**
 * Get singleton inventory store instance
 */
export function getInventoryStore(): ReturnType<typeof useInventoryStore> {
  if (!inventoryStoreInstance) {
    inventoryStoreInstance = useInventoryStore()
  }
  return inventoryStoreInstance
}
