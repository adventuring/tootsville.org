<template>
  <group :position="position" :rotation="rotation" :scale="scale">
    <mesh
      cast-shadow
      receive-shadow
      :user-data="{ type: 'interactive', id: type }"
      @click="onClick"
    >
      <!-- Mailbox -->
      <boxGeometry v-if="type === 'mailbox'" :args="[0.3, 0.8, 0.3]" />
      <!-- Bench -->
      <boxGeometry v-else-if="type === 'bench'" :args="[2, 0.3, 0.5]" />
      <!-- Fountain -->
      <cylinderGeometry v-else-if="type === 'fountain'" :args="[0.5, 0.5, 0.8, 8]" />
      <!-- Default -->
      <boxGeometry v-else :args="[0.5, 0.5, 0.5]" />
      
      <meshStandardMaterial :color="objectColor" :roughness="0.6" />
    </mesh>

    <!-- Special Features -->
    <group v-if="type === 'mailbox'">
      <mesh :position="[0, 0.6, 0]" cast-shadow>
        <boxGeometry :args="[0.4, 0.1, 0.4]" />
        <meshStandardMaterial :color="0x4169E1" :roughness="0.3" />
      </mesh>
    </group>

    <group v-if="type === 'bench'">
      <mesh :position="[0, 0.4, 0]" cast-shadow receive-shadow>
        <boxGeometry :args="[2, 0.1, 0.8]" />
        <meshStandardMaterial :color="0x8B4513" :roughness="0.8" />
      </mesh>
      <mesh :position="[-0.8, 0.2, 0]" cast-shadow>
        <boxGeometry :args="[0.1, 0.4, 0.5]" />
        <meshStandardMaterial :color="0x8B4513" :roughness="0.8" />
      </mesh>
      <mesh :position="[0.8, 0.2, 0]" cast-shadow>
        <boxGeometry :args="[0.1, 0.4, 0.5]" />
        <meshStandardMaterial :color="0x8B4513" :roughness="0.8" />
      </mesh>
    </group>

    <group v-if="type === 'fountain'">
      <mesh :position="[0, 0.9, 0]" cast-shadow>
        <cylinderGeometry :args="[0.3, 0.3, 0.2, 8]" />
        <meshStandardMaterial :color="0x87CEEB" :transparent="true" :opacity="0.8" />
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
  interactive?: boolean
}

const props = defineProps<Props>()

// Emits
const emit = defineEmits<{
  click: [object: any]
}>()

// Computed
const objectColor = computed(() => {
  const colors = {
    mailbox: 0x4169E1,   // Royal blue
    bench: 0x8B4513,     // Saddle brown
    fountain: 0x87CEEB,  // Sky blue
    default: 0x808080    // Gray
  }
  
  return colors[props.type as keyof typeof colors] || colors.default
})

// Methods
const onClick = () => {
  emit('click', { type: props.type, position: props.position })
}
</script>
