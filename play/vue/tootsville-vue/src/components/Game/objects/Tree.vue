<template>
  <group :position="position" :rotation="rotation" :scale="scale">
    <!-- Tree Trunk -->
    <mesh cast-shadow receive-shadow>
      <cylinderGeometry :args="[0.2, 0.3, 2, 8]" />
      <meshStandardMaterial :color="0x8B4513" :roughness="0.9" />
    </mesh>

    <!-- Tree Leaves -->
    <mesh :position="[0, 1.5, 0]" cast-shadow receive-shadow>
      <sphereGeometry :args="[1, 8, 8]" />
      <meshStandardMaterial :color="treeColor" :roughness="0.8" />
    </mesh>

    <!-- Additional Leaves for Larger Trees -->
    <mesh
      v-if="type === 'oak' || type === 'maple'"
      :position="[0, 2.2, 0]"
      cast-shadow
      receive-shadow
    >
      <sphereGeometry :args="[0.8, 8, 8]" />
      <meshStandardMaterial :color="treeColor" :roughness="0.8" />
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
const treeColor = computed(() => {
  const colors = {
    oak: 0x228B22,      // Forest green
    pine: 0x006400,     // Dark green
    maple: 0x8B4513,    // Brown (fall colors)
    cherry: 0xFF69B4    // Pink
  }
  
  return colors[props.type as keyof typeof colors] || colors.oak
})
</script>

