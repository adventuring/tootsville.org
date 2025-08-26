<template>
  <group :position="position" :rotation="rotation" :scale="scale">
    <mesh
      cast-shadow
      receive-shadow
      :user-data="{ type: 'decorative', id: type }"
    >
      <!-- Rock -->
      <dodecahedronGeometry v-if="type === 'rock'" :args="[0.3, 0]" />
      <!-- Flower -->
      <sphereGeometry v-else-if="type === 'flower'" :args="[0.2, 8, 8]" />
      <!-- Bush -->
      <sphereGeometry v-else-if="type === 'bush'" :args="[0.4, 8, 8]" />
      <!-- Sign -->
      <boxGeometry v-else-if="type === 'sign'" :args="[0.1, 0.8, 0.5]" />
      <!-- Default -->
      <boxGeometry v-else :args="[0.2, 0.2, 0.2]" />
      
      <meshStandardMaterial :color="elementColor" :roughness="0.8" />
    </mesh>

    <!-- Special Features -->
    <group v-if="type === 'flower'">
      <mesh :position="[0, -0.3, 0]" cast-shadow>
        <cylinderGeometry :args="[0.02, 0.02, 0.3, 4]" />
        <meshStandardMaterial :color="0x228B22" :roughness="0.9" />
      </mesh>
    </group>

    <group v-if="type === 'sign'">
      <mesh :position="[0, -0.4, 0]" cast-shadow>
        <cylinderGeometry :args="[0.05, 0.05, 0.8, 4]" />
        <meshStandardMaterial :color="0x8B4513" :roughness="0.9" />
      </mesh>
    </group>
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
const elementColor = computed(() => {
  const colors = {
    rock: 0x696969,      // Dim gray
    flower: 0xFF69B4,    // Hot pink
    bush: 0x228B22,      // Forest green
    sign: 0x8B4513,      // Saddle brown
    default: 0x808080    // Gray
  }
  
  return colors[props.type as keyof typeof colors] || colors.default
})
</script>


