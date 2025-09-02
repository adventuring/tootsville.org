<template>
  <div class="world-3d-container">
    <Canvas
      :camera="{ position: [0, 5, 10], fov: 75 }"
      :gl="{ antialias: true, alpha: true }"
      @created="onCanvasCreated"
      @pointer-move="onPointerMove"
      @click="onCanvasClick"
    >
      <!-- Lighting -->
      <ambientLight :intensity="0.4" />
      <directionalLight
        :position="[10, 10, 5]"
        :intensity="1"
        cast-shadow
      />
      <hemisphereLight
        :position="[0, 20, 0]"
        :intensity="0.3"
        :color="0x87ceeb"
        :ground-color="0x90ee90"
      />

      <!-- Sky -->
      <Sky
        :distance="450000"
        :sun-position="[0, 1, 0]"
        :inclination="0.5"
        :azimuth="0.25"
      />

      <!-- Ground -->
      <mesh
        :rotation="[-Math.PI / 2, 0, 0]"
        :position="[0, -0.5, 0]"
        receive-shadow
      >
        <planeGeometry :args="[100, 100]" />
        <meshStandardMaterial :color="0x90ee90" />
      </mesh>

      <!-- World Objects -->
      <WorldObjects />

      <!-- Player Character -->
      <PlayerCharacter
        v-if="playerData"
        :character="playerData"
        :animation-state="animationState"
      />

      <!-- Other Players -->
      <OtherPlayers
        v-for="player in otherPlayers"
        :key="player.id"
        :player="player"
      />

      <!-- UI Overlay -->
      <Html :position="[0, 5, 0]">
        <div class="world-ui">
          <div class="coordinates">
            X: {{ cameraPosition.x.toFixed(2) }} 
            Y: {{ cameraPosition.y.toFixed(2) }} 
            Z: {{ cameraPosition.z.toFixed(2) }}
          </div>
          <div class="fps">FPS: {{ fps }}</div>
        </div>
      </Html>

      <!-- Controls -->
      <OrbitControls
        :enable-pan="true"
        :enable-zoom="true"
        :enable-rotate="true"
        :max-distance="50"
        :min-distance="2"
        :max-polar-angle="Math.PI / 2"
      />
    </Canvas>

    <!-- Loading Screen -->
    <div v-if="isLoading" class="loading-screen">
      <div class="loading-content">
        <h2>Loading Tootsville World...</h2>
        <div class="loading-bar">
          <div 
            class="loading-progress" 
            :style="{ width: `${loadingProgress}%` }"
          ></div>
        </div>
        <p>{{ loadingMessage }}</p>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import { Canvas, Html } from 'vue-three'
import { Vector3, Euler } from 'three'
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js'
import { Sky } from 'three/examples/jsm/objects/Sky.js'
import { useFrame } from 'vue-three'
import { useGameStore } from '@/stores/game'
import { useAnimationManager } from '@/composables/useAnimationManager'
import { mobilePlatformService } from '@/services/MobilePlatformService'
import WorldObjects from './WorldObjects.vue'
import PlayerCharacter from './PlayerCharacter.vue'
import OtherPlayers from './OtherPlayers.vue'

// Props
interface Props {
  initialPosition?: Vector3
  initialRotation?: Euler
}

const props = withDefaults(defineProps<Props>(), {
  initialPosition: () => new Vector3(0, 0, 0),
  initialRotation: () => new Euler(0, 0, 0)
})

// Emits
const emit = defineEmits<{
  worldReady: []
  playerMoved: [position: Vector3]
  objectClicked: [object: any]
}>()

// Store
const gameStore = useGameStore()

// Animation manager
const { currentAnimation, isMoving, capabilities } = useAnimationManager()

// Reactive state
const isLoading = ref(true)
const loadingProgress = ref(0)
const loadingMessage = ref('Initializing world...')
const cameraPosition = reactive(new Vector3(0, 5, 10))
const fps = ref(60)
const lastTime = ref(0)

// Computed
const playerData = computed(() => ({
  ...gameStore.character,
  position: new Vector3(
    gameStore.character.position.x,
    gameStore.character.position.y,
    gameStore.character.position.z
  ),
  rotation: new Euler(
    gameStore.character.rotation.x,
    gameStore.character.rotation.y,
    gameStore.character.rotation.z
  ),
  scale: new Vector3(
    gameStore.character.scale.x,
    gameStore.character.scale.y,
    gameStore.character.scale.z
  )
}))

const otherPlayers = computed(() => Array.from(gameStore.avatars.values()).map(player => ({
  ...player,
  avatar: player.type || 'toot',
  position: new Vector3(player.position.x, player.position.y, player.position.z),
  rotation: new Euler(player.rotation.x, player.rotation.y, player.rotation.z),
  scale: new Vector3(player.scale.x, player.scale.y, player.scale.z)
})))

const animationState = computed(() => ({
  current: currentAnimation.value,
  isMoving: isMoving.value,
  capabilities: capabilities.value
}))

// Platform optimizations
const platform = computed(() => mobilePlatformService.getPlatform())
const optimizations = computed(() => mobilePlatformService.getOptimizations())

// Methods
const onCanvasCreated = ({ gl, scene, camera }: any) => {
  console.log('3D World created')
  
  // Apply platform-specific optimizations
  if (platform.value.isMobile) {
    gl.setPixelRatio(Math.min(window.devicePixelRatio, 2))
  }
  
  // Enable shadows
  gl.shadowMap.enabled = true
  gl.shadowMap.type = 2 // PCFSoftShadowMap
  
  // Set background
  scene.background = null // Transparent for sky
  
  // Initialize world
  initializeWorld()
}

const initializeWorld = async () => {
  try {
    loadingMessage.value = 'Loading world geometry...'
    loadingProgress.value = 25
    
    // Load world objects
    await loadWorldObjects()
    loadingProgress.value = 50
    
    loadingMessage.value = 'Loading character models...'
    await loadCharacterModels()
    loadingProgress.value = 75
    
    loadingMessage.value = 'Initializing game systems...'
    await initializeGameSystems()
    loadingProgress.value = 100
    
    loadingMessage.value = 'World ready!'
    await new Promise(resolve => setTimeout(resolve, 500))
    
    isLoading.value = false
    emit('worldReady')
    
  } catch (error) {
    console.error('Failed to initialize world:', error)
    loadingMessage.value = 'Error loading world. Please refresh.'
  }
}

const loadWorldObjects = async () => {
  // Simulate loading world objects
  await new Promise(resolve => setTimeout(resolve, 1000))
}

const loadCharacterModels = async () => {
  // Simulate loading character models
  await new Promise(resolve => setTimeout(resolve, 1000))
}

const initializeGameSystems = async () => {
  // Initialize game systems
  await new Promise(resolve => setTimeout(resolve, 500))
}

const onPointerMove = (event: any) => {
  // Handle mouse/touch movement
  if (event.intersections.length > 0) {
    const intersection = event.intersections[0]
    // Update cursor or highlight objects
  }
}

const onCanvasClick = (event: any) => {
  if (event.intersections.length > 0) {
    const intersection = event.intersections[0]
    const clickedObject = intersection.object
    
    // Handle object clicks
    if (clickedObject.userData.type === 'interactive') {
      emit('objectClicked', clickedObject)
    }
    
    // Handle ground clicks for movement
    if (clickedObject.userData.type === 'ground') {
      const position = intersection.point
      movePlayerTo(position)
    }
  }
}

const movePlayerTo = (position: Vector3) => {
  if (playerData.value) {
    // Update player position
    gameStore.updateCharacterPosition({
      x: position.x,
      y: position.y,
      z: position.z
    })
    emit('playerMoved', position)
  }
}

// Frame loop
useFrame(({ clock, camera }: any) => {
  // Update camera position
  cameraPosition.copy(camera.position)
  
  // Calculate FPS
  const time = clock.getElapsedTime()
  const deltaTime = time - lastTime.value
  if (deltaTime > 0) {
    fps.value = Math.round(1 / deltaTime)
  }
  lastTime.value = time
  
  // Apply platform-specific optimizations
  if (platform.value.isMobile && fps.value < optimizations.value.performance.targetFPS) {
    // Reduce quality on mobile if FPS is low
  }
})

// Lifecycle
onMounted(() => {
  console.log('World3D component mounted')
})

onUnmounted(() => {
  console.log('World3D component unmounted')
})
</script>

<style scoped>
.world-3d-container {
  position: relative;
  width: 100%;
  height: 100vh;
  background: linear-gradient(to bottom, #87ceeb 0%, #90ee90 100%);
}

.loading-screen {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0, 0, 0, 0.8);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.loading-content {
  text-align: center;
  color: white;
  max-width: 400px;
  padding: 20px;
}

.loading-content h2 {
  margin-bottom: 20px;
  font-size: 24px;
  color: #90ee90;
}

.loading-bar {
  width: 100%;
  height: 8px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  overflow: hidden;
  margin-bottom: 15px;
}

.loading-progress {
  height: 100%;
  background: linear-gradient(90deg, #90ee90, #87ceeb);
  transition: width 0.3s ease;
}

.loading-content p {
  font-size: 14px;
  opacity: 0.8;
}

.world-ui {
  position: absolute;
  top: 20px;
  left: 20px;
  color: white;
  font-family: monospace;
  font-size: 12px;
  text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.8);
  pointer-events: none;
}

.coordinates {
  margin-bottom: 5px;
}

.fps {
  opacity: 0.7;
}

/* Platform-specific styles */
@media (max-width: 768px) {
  .world-ui {
    font-size: 10px;
    top: 10px;
    left: 10px;
  }
  
  .loading-content h2 {
    font-size: 20px;
  }
}

/* TV platform styles */
@media (min-width: 1920px) {
  .world-ui {
    font-size: 16px;
    top: 30px;
    left: 30px;
  }
  
  .loading-content h2 {
    font-size: 32px;
  }
}
</style>
