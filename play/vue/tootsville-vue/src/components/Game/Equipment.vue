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

// Props
interface Props {
  equipment: {
    id: string
    type: string
    name: string
    color?: number
    glow?: boolean
    particles?: boolean
  }
  position: Vector3
}

const props = defineProps<Props>()

// Computed
const equipmentModel = computed(() => props.equipment && props.equipment.type)

const equipmentColor = computed(() => {
  if (props.equipment.color) {
    return props.equipment.color
  }
  
  // Default colors based on equipment type
  const colors = {
    weapon: 0xFF0000,    // Red
    shield: 0x0000FF,    // Blue
    tool: 0xFFFF00,      // Yellow
    accessory: 0xFF00FF, // Magenta
    default: 0x808080    // Gray
  }
  
  return colors[props.equipment.type as keyof typeof colors] || colors.default
})

const showEffects = computed(() => props.equipment.glow || props.equipment.particles)
</script>

