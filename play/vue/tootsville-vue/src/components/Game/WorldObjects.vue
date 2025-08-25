<template>
  <group>
    <!-- Trees -->
    <Tree
      v-for="tree in trees"
      :key="`tree-${tree.id}`"
      :position="tree.position"
      :rotation="tree.rotation"
      :scale="tree.scale"
      :type="tree.type"
    />

    <!-- Buildings -->
    <Building
      v-for="building in buildings"
      :key="`building-${building.id}`"
      :position="building.position"
      :rotation="building.rotation"
      :scale="building.scale"
      :type="building.type"
      :interactive="building.interactive"
      @click="onBuildingClick"
    />

    <!-- Interactive Objects -->
    <InteractiveObject
      v-for="object in interactiveObjects"
      :key="`object-${object.id}`"
      :position="object.position"
      :rotation="object.rotation"
      :scale="object.scale"
      :type="object.type"
      :interactive="true"
      @click="onObjectClick"
    />

    <!-- Water Bodies -->
    <WaterBody
      v-for="water in waterBodies"
      :key="`water-${water.id}`"
      :position="water.position"
      :rotation="water.rotation"
      :scale="water.scale"
      :type="water.type"
    />

    <!-- Decorative Elements -->
    <DecorativeElement
      v-for="element in decorativeElements"
      :key="`decorative-${element.id}`"
      :position="element.position"
      :rotation="element.rotation"
      :scale="element.scale"
      :type="element.type"
    />
  </group>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import { Vector3, Euler } from 'three'
import Tree from './objects/Tree.vue'
import Building from './objects/Building.vue'
import InteractiveObject from './objects/InteractiveObject.vue'
import WaterBody from './objects/WaterBody.vue'
import DecorativeElement from './objects/DecorativeElement.vue'

// Types
interface WorldObject {
  id: string
  position: Vector3
  rotation: Euler
  scale: Vector3
  type: string
  interactive?: boolean
}

// Emits
const emit = defineEmits<{
  objectClicked: [object: WorldObject]
  buildingEntered: [building: WorldObject]
}>()

// Reactive state
const trees = ref<WorldObject[]>([])
const buildings = ref<WorldObject[]>([])
const interactiveObjects = ref<WorldObject[]>([])
const waterBodies = ref<WorldObject[]>([])
const decorativeElements = ref<WorldObject[]>([])

// Methods
const generateWorldObjects = () => {
  // Generate trees
  trees.value = Array.from({ length: 20 }, (_, i) => ({
    id: `tree-${i}`,
    position: new Vector3(
      (Math.random() - 0.5) * 80,
      0,
      (Math.random() - 0.5) * 80
    ),
    rotation: new Euler(0, Math.random() * Math.PI * 2, 0),
    scale: new Vector3(1, 1, 1),
    type: ['oak', 'pine', 'maple', 'cherry'][Math.floor(Math.random() * 4)]
  }))

  // Generate buildings
  buildings.value = [
    {
      id: 'grand-toot-station',
      position: new Vector3(0, 0, 0),
      rotation: new Euler(0, 0, 0),
      scale: new Vector3(1, 1, 1),
      type: 'station',
      interactive: true
    },
    {
      id: 'raving-burger',
      position: new Vector3(15, 0, 10),
      rotation: new Euler(0, Math.PI / 4, 0),
      scale: new Vector3(1, 1, 1),
      type: 'restaurant',
      interactive: true
    },
    {
      id: 'castillo-toot',
      position: new Vector3(-20, 0, -15),
      rotation: new Euler(0, -Math.PI / 6, 0),
      scale: new Vector3(1.5, 1.5, 1.5),
      type: 'castle',
      interactive: true
    }
  ]

  // Generate interactive objects
  interactiveObjects.value = [
    {
      id: 'mailbox-1',
      position: new Vector3(5, 0, 5),
      rotation: new Euler(0, 0, 0),
      scale: new Vector3(1, 1, 1),
      type: 'mailbox',
      interactive: true
    },
    {
      id: 'bench-1',
      position: new Vector3(-5, 0, 8),
      rotation: new Euler(0, Math.PI / 2, 0),
      scale: new Vector3(1, 1, 1),
      type: 'bench',
      interactive: true
    },
    {
      id: 'fountain-1',
      position: new Vector3(0, 0, 15),
      rotation: new Euler(0, 0, 0),
      scale: new Vector3(1, 1, 1),
      type: 'fountain',
      interactive: true
    }
  ]

  // Generate water bodies
  waterBodies.value = [
    {
      id: 'lake-1',
      position: new Vector3(25, -1, 25),
      rotation: new Euler(0, 0, 0),
      scale: new Vector3(10, 1, 8),
      type: 'lake'
    },
    {
      id: 'stream-1',
      position: new Vector3(-10, -0.5, 5),
      rotation: new Euler(0, Math.PI / 3, 0),
      scale: new Vector3(15, 0.5, 2),
      type: 'stream'
    }
  ]

  // Generate decorative elements
  decorativeElements.value = Array.from({ length: 30 }, (_, i) => ({
    id: `decorative-${i}`,
    position: new Vector3(
      (Math.random() - 0.5) * 90,
      0,
      (Math.random() - 0.5) * 90
    ),
    rotation: new Euler(0, Math.random() * Math.PI * 2, 0),
    scale: new Vector3(0.5, 0.5, 0.5),
    type: ['rock', 'flower', 'bush', 'sign'][Math.floor(Math.random() * 4)]
  }))
}

const onBuildingClick = (building: WorldObject) => {
  emit('buildingEntered', building)
}

const onObjectClick = (object: WorldObject) => {
  emit('objectClicked', object)
}

// Lifecycle
onMounted(() => {
  generateWorldObjects()
})
</script>
