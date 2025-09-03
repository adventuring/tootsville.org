<template>
  <div class="animation-panel">
    <h3>Animation Controls</h3>
    
    <!-- Current Animation Display -->
    <div class="current-animation">
      <label>Current Animation:</label>
      <span class="animation-name">{{ currentAnimation }}</span>
    </div>
    
    <!-- Animation State -->
    <div class="animation-state">
      <label>State:</label>
      <span :class="['state', isTransitioning ? 'transitioning' : 'stable']">
        {{ isTransitioning ? 'Transitioning' : 'Stable' }}
      </span>
    </div>
    
    <!-- Movement State -->
    <div class="movement-state">
      <label>Movement:</label>
      <span :class="['state', isMoving ? 'moving' : 'stationary']">
        {{ isMoving ? 'Moving' : 'Stationary' }}
      </span>
    </div>
    
    <!-- Capabilities Grid -->
    <div class="capabilities-section">
      <h4>Character Capabilities</h4>
      <div class="capabilities-grid">
        <div 
          v-for="(capability, key) in capabilities" 
          :key="key"
          class="capability-item"
        >
          <label>{{ formatCapabilityName(key) }}:</label>
          <span :class="['capability', capability ? 'enabled' : 'disabled']">
            {{ capability ? 'Yes' : 'No' }}
          </span>
        </div>
      </div>
    </div>
    
    <!-- Animation Controls -->
    <div class="animation-controls">
      <h4>Controls</h4>
      
      <!-- Basic Animations -->
      <div class="control-group">
        <button 
          @click="playAnimation('idle')"
          :disabled="!capabilities.canIdle"
          :class="{ active: currentAnimation === 'idle' }"
        >
          Idle
        </button>
        <button 
          @click="playAnimation('walk')"
          :disabled="!capabilities.canWalk"
          :class="{ active: currentAnimation === 'walk' }"
        >
          Walk
        </button>
        <button 
          @click="playAnimation('run')"
          :disabled="!capabilities.canRun"
          :class="{ active: currentAnimation === 'run' }"
        >
          Run
        </button>
      </div>
      
      <!-- Advanced Animations -->
      <div class="control-group">
        <button 
          @click="playAnimation('jump')"
          :disabled="!capabilities.canJump"
          :class="{ active: currentAnimation === 'jump' }"
        >
          Jump
        </button>
        <button 
          @click="playAnimation('swim')"
          :disabled="!capabilities.canSwim"
          :class="{ active: currentAnimation === 'swim' }"
        >
          Swim
        </button>
        <button 
          @click="playAnimation('fly')"
          :disabled="!capabilities.canFly"
          :class="{ active: currentAnimation === 'fly' }"
        >
          Fly
        </button>
      </div>
      
      <!-- Social Animations -->
      <div class="control-group">
        <button 
          @click="playAnimation('sit')"
          :disabled="!capabilities.canSit"
          :class="{ active: currentAnimation === 'sit' }"
        >
          Sit
        </button>
        <button 
          @click="playAnimation('wave')"
          :disabled="!capabilities.canEmote"
          :class="{ active: currentAnimation === 'wave' }"
        >
          Wave
        </button>
        <button 
          @click="playAnimation('dance')"
          :disabled="!capabilities.canEmote"
          :class="{ active: currentAnimation === 'dance' }"
        >
          Dance
        </button>
      </div>
    </div>
    
    <!-- Settings -->
    <div class="animation-settings">
      <h4>Settings</h4>
      
      <div class="setting-item">
        <label for="animation-speed">Animation Speed:</label>
        <input 
          id="animation-speed"
          type="range" 
          min="0.1" 
          max="3.0" 
          step="0.1" 
          v-model="animationSpeed"
          @input="updateAnimationSpeed"
        />
        <span class="speed-value">{{ animationSpeed }}x</span>
      </div>
      
      <div class="setting-item">
        <label for="transition-duration">Transition Duration:</label>
        <input 
          id="transition-duration"
          type="range" 
          min="0.1" 
          max="2.0" 
          step="0.1" 
          v-model="transitionDuration"
          @input="updateTransitionDuration"
        />
        <span class="duration-value">{{ transitionDuration }}s</span>
      </div>
      
      <div class="setting-item">
        <label>
          <input 
            type="checkbox" 
            v-model="autoTransition"
            @change="updateAutoTransition"
          />
          Auto-transition between animations
        </label>
      </div>
    </div>
    
    <!-- Debug Information -->
    <div class="debug-info" v-if="showDebug">
      <h4>Debug Info</h4>
      <pre>{{ debugInfo }}</pre>
    </div>
    
    <!-- Toggle Debug -->
    <button 
      @click="toggleDebug"
      class="debug-toggle"
    >
      {{ showDebug ? 'Hide' : 'Show' }} Debug
    </button>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { useAnimationManager } from '@/composables/useAnimationManager'

// Animation manager composable
const {
  currentAnimation,
  isTransitioning,
  isMoving,
  capabilities,
  playAnimation,
  updateAnimationSpeed,
  updateTransitionDuration,
  updateAutoTransition,
  getDebugInfo
} = useAnimationManager()

// Local state
const animationSpeed = ref(1.0)
const transitionDuration = ref(0.5)
const autoTransition = ref(true)
const showDebug = ref(false)

// Computed properties
const debugInfo = computed(() => {
  try {
    return JSON.stringify(getDebugInfo(), null, 2)
  } catch (error) {
    return `Error getting debug info: ${error}`
  }
})

// Methods
const formatCapabilityName = (key: string): string => {
  // Convert camelCase to Title Case
  return key
    .replace(/([A-Z])/g, ' $1')
    .replace(/^./, str => str.toUpperCase())
}

const toggleDebug = () => {
  showDebug.value = !showDebug.value
}

// Lifecycle
onMounted(() => {
  // Initialize settings from animation manager
  try {
    // These would be set from the animation manager's current state
    // For now, using default values
  } catch (error) {
    console.warn('Error initializing animation settings:', error)
  }
})

onUnmounted(() => {
  // Cleanup if needed
})
</script>

<style scoped>
.animation-panel {
  padding: 1rem;
  background: rgba(0, 0, 0, 0.8);
  border-radius: 8px;
  color: white;
  max-width: 400px;
}

.animation-panel h3,
.animation-panel h4 {
  margin: 0 0 1rem 0;
  color: #4ade80;
}

.current-animation,
.animation-state,
.movement-state {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
  padding: 0.5rem;
  background: rgba(255, 255, 255, 0.1);
  border-radius: 4px;
}

.animation-name {
  font-weight: bold;
  color: #4ade80;
}

.state {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.state.transitioning {
  background: #fbbf24;
  color: #1f2937;
}

.state.stable {
  background: #10b981;
  color: white;
}

.state.moving {
  background: #3b82f6;
  color: white;
}

.state.stationary {
  background: #6b7280;
  color: white;
}

.capabilities-section {
  margin: 1rem 0;
}

.capabilities-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.5rem;
}

.capability-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.5rem;
  background: rgba(255, 255, 255, 0.1);
  border-radius: 4px;
}

.capability {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
  font-weight: bold;
}

.capability.enabled {
  background: #10b981;
  color: white;
}

.capability.disabled {
  background: #ef4444;
  color: white;
}

.animation-controls {
  margin: 1rem 0;
}

.control-group {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 0.5rem;
}

.control-group button {
  flex: 1;
  padding: 0.5rem;
  border: none;
  border-radius: 4px;
  background: #374151;
  color: white;
  cursor: pointer;
  transition: all 0.2s;
}

.control-group button:hover:not(:disabled) {
  background: #4b5563;
}

.control-group button:disabled {
  background: #1f2937;
  color: #6b7280;
  cursor: not-allowed;
}

.control-group button.active {
  background: #4ade80;
  color: #1f2937;
}

.animation-settings {
  margin: 1rem 0;
}

.setting-item {
  margin-bottom: 1rem;
}

.setting-item label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 500;
}

.setting-item input[type="range"] {
  width: 100%;
  margin-right: 0.5rem;
}

.speed-value,
.duration-value {
  font-weight: bold;
  color: #4ade80;
}

.debug-info {
  margin: 1rem 0;
  padding: 1rem;
  background: rgba(0, 0, 0, 0.5);
  border-radius: 4px;
  font-family: monospace;
  font-size: 0.875rem;
  max-height: 200px;
  overflow-y: auto;
}

.debug-toggle {
  width: 100%;
  padding: 0.5rem;
  border: none;
  border-radius: 4px;
  background: #6b7280;
  color: white;
  cursor: pointer;
  transition: background 0.2s;
}

.debug-toggle:hover {
  background: #9ca3af;
}
</style>
