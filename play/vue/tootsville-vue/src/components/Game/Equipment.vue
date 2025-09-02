<template>
  <group :position="position">
    <!-- Equipment Model -->
    <mesh
      v-if="equipmentModel"
      :user-data="{ type: 'equipment', id: equipment.id }"
      cast-shadow
    >
      <boxGeometry :args="[0.3, 0.3, 0.3]" />
      <meshStandardMaterial
        :color="equipmentColor"
        :roughness="0.5"
        :metalness="0.3"
      />
    </mesh>

    <!-- Equipment Effects -->
    <group v-if="showEffects">
      <!-- Glow Effect -->
      <mesh
        v-if="equipment.glow"
        :position="[0, 0, 0]"
      >
        <sphereGeometry :args="[0.4, 8, 8]" />
        <meshBasicMaterial
          :color="equipmentColor"
          :transparent="true"
          :opacity="0.3"
        />
      </mesh>

      <!-- Particle Effects -->
      <mesh
        v-if="equipment.particles"
        :position="[0, 0.5, 0]"
      >
        <sphereGeometry :args="[0.05, 4, 4]" />
        <meshBasicMaterial
          :color="equipmentColor"
          :transparent="true"
          :opacity="0.6"
        />
      </mesh>
    </group>
  </group>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { Vector3 } from 'three'

const properties = defineProps<{
  equipment: {
    id: string
    type: string
    name: string
    color?: number
    glow?: boolean
    particles?: boolean
  }
  position: Vector3
}>()

// Computed
const equipmentModel = computed(() => properties.equipment && properties.equipment.type)

const equipmentColor = computed(() => {
  // Use the item's own color attribute if provided
  if (properties.equipment.color !== undefined) {
    return properties.equipment.color
  }
  
  // Fall back to type-based colors only when no color is specified
  const typeColors = {
    air_shooter: 0x00FF00,    // Green
    egg_shooter: 0xFF8000,    // Orange
    tool: 0xFFFF00,           // Yellow
    accessory: 0xFF00FF,      // Magenta
    default: 0x808080         // Gray
  }
  
  return typeColors[properties.equipment.type as keyof typeof typeColors] || typeColors.default
})

const showEffects = computed(() => properties.equipment.glow || properties.equipment.particles)
</script>


