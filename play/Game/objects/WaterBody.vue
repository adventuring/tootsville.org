<template>
  <group :position="position" :rotation="rotation" :scale="scale">
    <mesh
      receive-shadow
      :user-data="{ type: 'water', id: type }"
    >
      <!-- Lake -->
      <cylinderGeometry v-if="type === 'lake'" :args="[1, 1, 0.2, 16]" />
      <!-- Stream -->
      <boxGeometry v-else-if="type === 'stream'" :args="[1, 0.1, 1]" />
      <!-- Default -->
      <boxGeometry v-else :args="[1, 0.1, 1]" />
      
      <meshStandardMaterial
        :color="waterColor"
        :transparent="true"
        :opacity="0.6"
        :roughness="0.1"
        :metalness="0.8"
      />
    </mesh>
  </group>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { Vector3, Euler } from 'three'

// Props
interface Props {
  position: Vector3
  rotation: Euler
  scale: Vector3
  type: string
}

const props = defineProps<Props>()

// Computed
const waterColor = computed(() => {
  const colors = {
    lake: 0x4169E1,      // Royal blue
    stream: 0x87CEEB,    // Sky blue
    ocean: 0x000080,     // Navy blue
    default: 0x87CEEB    // Sky blue
  }
  
  return colors[props.type as keyof typeof colors] || colors.default
})
</script>


