<!--
  Scenery.vue - World Scenery Objects

  Loads 3D models dynamically from URIs provided by Infinity mode scene descriptions.
  Model paths are constructed from server-provided URIs over REST, WebSocket, or other
  Infinity mode communications instead of being hard-coded.

  Props:
  - position, rotation, scale: Standard Three.js transform properties
  - type: Scenery type identifier (used for fallback colors and logging)
  - modelUri: Full URI to .glb model file from Infinity mode scene description

  Behavior:
  - If modelUri is provided: Load the 3D model from that URI
  - If modelUri is missing/invalid: Use fallback geometry based on type
  - Supports loading states, error handling, and automatic resource cleanup

  Server Integration:
  Scene descriptions over Infinity mode should include modelUri fields like:
  {
    "type": "scenery",
    "sceneryType": "butterflyFrame",
    "modelUri": "https://jumbo.tootsville.org/Assets/Models/5/ButterflyFrame/ButterflyFrame.glb",
    "position": [x, y, z],
    "rotation": [x, y, z],
    "scale": [x, y, z]
  }

  Copyright © 2025 Interworldly Adventuring, LLC.
  This program is Free Software; Refer to COPYING.AGPL for details.
-->

<template>
  <Suspense>
    <group :position="position" :rotation="rotation" :scale="scale">
      <!-- Loading State -->
      <mesh v-if="isLoading" cast-shadow receive-shadow :user-data="{ type: 'scenery', sceneryType: type, loading: true }">
        <boxGeometry :args="[0.1, 0.1, 0.1]" />
        <meshStandardMaterial :color="0xcccccc" :transparent="true" :opacity="0.5" />
      </mesh>

      <!-- Error State - Fallback to simple geometry -->
      <mesh v-else-if="hasError" cast-shadow receive-shadow :user-data="{ type: 'scenery', sceneryType: type, error: true, modelUri: modelUri }">
        <boxGeometry :args="[0.2, 0.2, 0.2]" />
        <meshStandardMaterial :color="0xff0000" :roughness="0.8" />
      </mesh>

      <!-- Loaded 3D Model -->
      <primitive
        v-else-if="loadedModel"
        :object="loadedModel"
        cast-shadow
        receive-shadow
        :user-data="{ type: 'scenery', sceneryType: type, modelUri: modelUri }"
      />

      <!-- Fallback for unsupported types -->
      <mesh v-else cast-shadow receive-shadow :user-data="{ type: 'scenery', sceneryType: type, fallback: true }">
        <boxGeometry :args="[0.2, 0.2, 0.2]" />
        <meshStandardMaterial :color="sceneryColor" :roughness="0.8" />
      </mesh>
    </group>

    <!-- Loading template for Suspense -->
    <template #fallback>
      <group :position="position" :rotation="rotation" :scale="scale">
        <mesh cast-shadow receive-shadow :user-data="{ type: 'scenery', sceneryType: type, suspenseFallback: true }">
          <boxGeometry :args="[0.1, 0.1, 0.1]" />
          <meshStandardMaterial :color="0xcccccc" :transparent="true" :opacity="0.5" />
        </mesh>
      </group>
    </template>
  </Suspense>
</template>

<script setup lang="ts">
import { computed, ref, onMounted, watch } from 'vue'
import { Vector3, Euler, Object3D, Mesh } from 'three'
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js'

const properties = defineProps<{
  position: Vector3
  rotation: Euler
  scale: Vector3
  type: string
  modelUri?: string  // URI from Infinity mode scene description
}>()

// Reactive state
const loadedModel = ref<Object3D | null>(null)
const isLoading = ref(false)
const hasError = ref(false)

// GLTF Loader instance
const loader = new GLTFLoader()

// Computed properties
const sceneryColor = computed(() => {
  const colors = {
    rock: 0x696969,        // Dim gray
    flower: 0xFF69B4,      // Hot pink
    bush: 0x228B22,        // Forest green
    sign: 0x8B4513,        // Saddle brown
    tree: 0x8B4513,        // Brown trunk
    bench: 0x8B4513,       // Wooden bench
    fountain: 0xC0C0C0,    // Silver fountain
    lamp: 0x2F4F4F,        // Dark slate gray
    statue: 0xF5F5DC,      // Beige marble
    bridge: 0x8B4513,      // Brown wood/planks
    butterflyFrame: 0x8B4513, // Wooden frame
    default: 0x808080      // Gray
  }

  return colors[properties.type as keyof typeof colors] || colors.default
})

// Model URL from Infinity mode scene description
const modelUrl = computed(() => {
  return properties.modelUri || null
})

// Load model function
const loadModel = async (url: string) => {
  isLoading.value = true
  hasError.value = false
  loadedModel.value = null

  try {
    const gltf = await new Promise((resolve, reject) => {
      loader.load(
        url,
        resolve,
        (progress) => {
          console.log(`Loading ${properties.type} from Infinity mode: ${(progress.loaded / progress.total * 100).toFixed(1)}%`)
        },
        reject
      )
    })

    // Extract the scene from GLTF
    loadedModel.value = gltf.scene

    // Apply shadows to all meshes in the model
    loadedModel.value.traverse((child) => {
      if (child instanceof Mesh) {
        child.castShadow = true
        child.receiveShadow = true
      }
    })

    console.log(`Successfully loaded ${properties.type} model from Infinity mode URI: ${url}`)
  } catch (error) {
    console.error(`Failed to load ${properties.type} model from Infinity mode URI ${url}:`, error)
    hasError.value = true
  } finally {
    isLoading.value = false
  }
}

// Watch for changes in modelUri or type and load the model
watch(
  [() => properties.modelUri, () => properties.type],
  async ([newUri, newType]) => {
    if (newUri) {
      // Load model from URI provided by Infinity mode
      console.log(`Loading scenery model from Infinity mode URI: ${newUri}`)
      await loadModel(newUri)
    } else {
      // No URI provided, use fallback based on type
      console.log(`No model URI provided for ${newType}, using fallback geometry`)
      loadedModel.value = null
      isLoading.value = false
      hasError.value = false
    }
  },
  { immediate: true }
)

// Cleanup on unmount
onMounted(() => {
  return () => {
    // Dispose of geometries and materials
    if (loadedModel.value) {
      loadedModel.value.traverse((child) => {
        if (child instanceof Mesh) {
          child.geometry?.dispose()
          if (Array.isArray(child.material)) {
            child.material.forEach(material => material.dispose())
          } else {
            child.material?.dispose()
          }
        }
      })
    }
  }
})
</script>
