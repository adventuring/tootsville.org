<!-- Tootsville play/vue/tootsville-vue/src/components/Game/objects/Building.vue

Copyright © 2025 Interworldly Adventuring, LLC.

This component is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-->
<template>
  <group :position="position" :rotation="rotation" :scale="scale">
    <!-- Building Base -->
    <mesh
      cast-shadow
      receive-shadow
      :user-data="{ type: 'building', id: type, interactive: interactive }"
      @click="onClick"
    >
      <boxGeometry :args="[2, 3, 2]" />
      <meshStandardMaterial :color="buildingColor" :roughness="0.7" />
    </mesh>

    <!-- Roof -->
    <mesh :position="[0, 2.5, 0]" cast-shadow receive-shadow>
      <coneGeometry :args="[1.5, 1, 8]" />
      <meshStandardMaterial :color="0x8B4513" :roughness="0.8" />
    </mesh>

    <!-- Windows -->
    <mesh :position="[0, 0.5, 1.01]" cast-shadow>
      <planeGeometry :args="[0.8, 0.8]" />
      <meshStandardMaterial :color="0x87CEEB" :transparent="true" :opacity="0.8" />
    </mesh>

    <mesh :position="[0, 0.5, -1.01]" cast-shadow>
      <planeGeometry :args="[0.8, 0.8]" />
      <meshStandardMaterial :color="0x87CEEB" :transparent="true" :opacity="0.8" />
    </mesh>

    <!-- Door -->
    <mesh :position="[0, -0.5, 1.01]" cast-shadow>
      <planeGeometry :args="[0.6, 1.2]" />
      <meshStandardMaterial :color="0x8B4513" :roughness="0.9" />
    </mesh>

    <!-- Special Features -->
    <group v-if="type === 'station'">
      <!-- Train Station Platform -->
      <mesh :position="[0, -1.5, 3]" cast-shadow receive-shadow>
        <boxGeometry :args="[4, 0.5, 2]" />
        <meshStandardMaterial :color="0x696969" :roughness="0.8" />
      </mesh>
    </group>

    <group v-if="type === 'restaurant'">
      <!-- Restaurant Sign -->
      <mesh :position="[0, 2, 1.5]" cast-shadow>
        <boxGeometry :args="[1.5, 0.3, 0.1]" />
        <meshStandardMaterial :color="0xFF4500" :roughness="0.5" />
      </mesh>
    </group>

    <group v-if="type === 'castle'">
      <!-- Castle Towers -->
      <mesh :position="[-1.5, 1, 0]" cast-shadow receive-shadow>
        <cylinderGeometry :args="[0.3, 0.3, 4, 8]" />
        <meshStandardMaterial :color="0x696969" :roughness="0.8" />
      </mesh>
      <mesh :position="[1.5, 1, 0]" cast-shadow receive-shadow>
        <cylinderGeometry :args="[0.3, 0.3, 4, 8]" />
        <meshStandardMaterial :color="0x696969" :roughness="0.8" />
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
  click: [building: any]
}>()

// Computed
const buildingColor = computed(() => {
  const colors = {
    station: 0x708090,    // Slate gray
    restaurant: 0xFF6347, // Tomato red
    castle: 0x696969,     // Dim gray
    default: 0xCD853F     // Peru
  }
  
  return colors[props.type as keyof typeof colors] || colors.default
})

// Methods
const onClick = () => {
  if (props.interactive) {
    emit('click', { type: props.type, position: props.position })
  }
}
</script>


