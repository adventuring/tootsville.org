<template>
  <div class="animation-panel">
    <h3>Animation Manager</h3>
    
    <!-- Current State -->
    <div class="state-section">
      <h4>Current State</h4>
      <div class="state-grid">
        <div class="state-item">
          <label>Animation:</label>
          <span :class="['animation-state', currentAnimation]">{{ currentAnimation }}</span>
        </div>
        <div class="state-item">
          <label>Moving:</label>
          <span :class="['status', isMoving ? 'active' : 'inactive']">
            {{ isMoving ? 'Yes' : 'No' }}
          </span>
        </div>
        <div class="state-item">
          <label>Transitioning:</label>
          <span :class="['status', isTransitioning ? 'active' : 'inactive']">
            {{ isTransitioning ? 'Yes' : 'No' }}
          </span>
        </div>
      </div>
    </div>

    <!-- Character Capabilities -->
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
    <div class="controls-section">
      <h4>Animation Controls</h4>
      <div class="control-buttons">
        <button 
          @click="sit" 
          :disabled="!capabilities.canSit"
          class="control-btn"
        >
          Sit
        </button>
        <button 
          @click="stand" 
          class="control-btn"
        >
          Stand
        </button>
        <button 
          @click="jump" 
          :disabled="!capabilities.canJump"
          class="control-btn"
        >
          Jump
        </button>
        <button 
          @click="useItem" 
          class="control-btn"
        >
          Use Item
        </button>
        <button 
          @click="talk" 
          class="control-btn"
        >
          Talk
        </button>
        <button 
          @click="emote" 
          class="control-btn"
        >
          Emote
        </button>
      </div>
    </div>

    <!-- Character Data -->
    <div class="character-section">
      <h4>Character Data</h4>
      <div class="character-form">
        <div class="form-group">
          <label>Character Name:</label>
          <input 
            v-model="characterData.name" 
            type="text" 
            placeholder="Enter character name"
            @input="updateCharacter"
          />
        </div>
        <div class="form-group">
          <label>Avatar:</label>
          <input 
            v-model="characterData.avatar" 
            type="text" 
            placeholder="Enter avatar name"
            @input="updateCharacter"
          />
        </div>
        <div class="form-group">
          <label>Character Type:</label>
          <select v-model="characterData.characterType" @change="updateCharacter">
            <option value="">Select type</option>
            <option value="toot">Toot (Elephant)</option>
            <option value="manatee">Manatee</option>
            <option value="bird">Bird/Katootel</option>
            <option value="default">Default</option>
          </select>
        </div>
      </div>
    </div>

    <!-- Debug Information -->
    <div class="debug-section">
      <h4>Debug Information</h4>
      <button @click="showDebugInfo = !showDebugInfo" class="debug-btn">
        {{ showDebugInfo ? 'Hide' : 'Show' }} Debug Info
      </button>
      <pre v-if="showDebugInfo" class="debug-info">{{ debugInfo }}</pre>
    </div>

    <!-- Settings -->
    <div class="settings-section">
      <h4>Animation Settings</h4>
      <div class="settings-form">
        <div class="form-group">
          <label>Fade Duration:</label>
          <input 
            v-model.number="settings.fadeDuration" 
            type="range" 
            min="0.1" 
            max="1.0" 
            step="0.1"
            @input="updateSettings"
          />
          <span>{{ settings.fadeDuration }}s</span>
        </div>
        <div class="form-group">
          <label>Movement Threshold:</label>
          <input 
            v-model.number="settings.minMovementThreshold" 
            type="range" 
            min="0.001" 
            max="0.1" 
            step="0.001"
            @input="updateSettings"
          />
          <span>{{ settings.minMovementThreshold }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, reactive, computed } from 'vue'
import { Vector3 } from 'three'
import { useAnimationManager } from '@/composables/useAnimationManager'
import type { CharacterData } from '@/services/AnimationManager'

// Animation manager composable
const {
  currentAnimation,
  isTransitioning,
  isMoving,
  capabilities,
  updateCharacter: updateCharacterData,
  setAnimation,
  sit,
  stand,
  jump,
  useItem,
  talk,
  emote,
  getDebugInfo,
  updateSettings: updateSettingsData
} = useAnimationManager()

// Local state
const showDebugInfo = ref(false)

// Character data
const characterData = reactive<CharacterData>({
  id: '1',
  name: 'Test Toot',
  avatar: 'zap',
  characterType: 'toot',
  position: new Vector3(0, 0, 0),
  rotation: new Vector3(0, 0, 0),
  scale: new Vector3(1, 1, 1)
})

// Settings
const settings = reactive({
  fadeDuration: 0.3,
  minMovementThreshold: 0.01
})

// Computed
const debugInfo = computed(() => {
  return JSON.stringify(getDebugInfo(), null, 2)
})

// Methods
const updateCharacter = () => {
  updateCharacterData(characterData)
}

const updateSettings = () => {
  updateSettingsData(settings)
}

const formatCapabilityName = (key: string): string => {
  return key
    .replace(/([A-Z])/g, ' $1')
    .replace(/^./, str => str.toUpperCase())
}
</script>

<style scoped>
.animation-panel {
  background: var(--bg-secondary);
  border-radius: 8px;
  padding: 20px;
  max-width: 600px;
  margin: 20px auto;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}

.animation-panel h3 {
  margin: 0 0 20px 0;
  color: var(--text-primary);
  text-align: center;
}

.animation-panel h4 {
  margin: 15px 0 10px 0;
  color: var(--text-primary);
  border-bottom: 1px solid var(--border-color);
  padding-bottom: 5px;
}

.state-section,
.capabilities-section,
.controls-section,
.character-section,
.debug-section,
.settings-section {
  margin-bottom: 20px;
}

.state-grid,
.capabilities-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 10px;
}

.state-item,
.capability-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 8px;
  background: var(--bg-primary);
  border-radius: 4px;
}

.state-item label,
.capability-item label {
  font-weight: 500;
  color: var(--text-secondary);
}

.animation-state {
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
  text-transform: uppercase;
  font-size: 0.9em;
}

.animation-state.idle { background: #4CAF50; color: white; }
.animation-state.walk { background: #2196F3; color: white; }
.animation-state.run { background: #FF9800; color: white; }
.animation-state.sit { background: #9C27B0; color: white; }
.animation-state.jump { background: #F44336; color: white; }
.animation-state.swim { background: #00BCD4; color: white; }
.animation-state.fly { background: #E91E63; color: white; }
.animation-state.emote { background: #FF5722; color: white; }
.animation-state.use_item { background: #795548; color: white; }
.animation-state.talk { background: #607D8B; color: white; }

.status,
.capability {
  font-weight: bold;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 0.9em;
}

.status.active,
.capability.enabled {
  background: #4CAF50;
  color: white;
}

.status.inactive,
.capability.disabled {
  background: #f44336;
  color: white;
}

.control-buttons {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
  gap: 10px;
}

.control-btn {
  padding: 10px;
  border: none;
  border-radius: 4px;
  background: var(--primary-color);
  color: white;
  cursor: pointer;
  font-weight: 500;
  transition: background-color 0.2s;
}

.control-btn:hover:not(:disabled) {
  background: var(--primary-hover);
}

.control-btn:disabled {
  background: var(--disabled-color);
  cursor: not-allowed;
  opacity: 0.6;
}

.character-form,
.settings-form {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 5px;
}

.form-group label {
  font-weight: 500;
  color: var(--text-secondary);
}

.form-group input,
.form-group select {
  padding: 8px;
  border: 1px solid var(--border-color);
  border-radius: 4px;
  background: var(--bg-primary);
  color: var(--text-primary);
}

.form-group input:focus,
.form-group select:focus {
  outline: none;
  border-color: var(--primary-color);
}

.form-group input[type="range"] {
  display: flex;
  align-items: center;
  gap: 10px;
}

.form-group input[type="range"] + span {
  min-width: 50px;
  text-align: right;
  color: var(--text-secondary);
}

.debug-btn {
  padding: 8px 16px;
  border: 1px solid var(--border-color);
  border-radius: 4px;
  background: var(--bg-primary);
  color: var(--text-primary);
  cursor: pointer;
  margin-bottom: 10px;
}

.debug-btn:hover {
  background: var(--bg-hover);
}

.debug-info {
  background: var(--bg-primary);
  border: 1px solid var(--border-color);
  border-radius: 4px;
  padding: 15px;
  font-family: monospace;
  font-size: 0.9em;
  color: var(--text-primary);
  white-space: pre-wrap;
  max-height: 300px;
  overflow-y: auto;
}

/* Dark mode support */
@media (prefers-color-scheme: dark) {
  .animation-panel {
    --bg-primary: #2a2a2a;
    --bg-secondary: #1a1a1a;
    --bg-hover: #3a3a3a;
    --text-primary: #ffffff;
    --text-secondary: #cccccc;
    --border-color: #444444;
    --primary-color: #4CAF50;
    --primary-hover: #45a049;
    --disabled-color: #666666;
  }
}

/* Light mode */
@media (prefers-color-scheme: light) {
  .animation-panel {
    --bg-primary: #ffffff;
    --bg-secondary: #f5f5f5;
    --bg-hover: #eeeeee;
    --text-primary: #333333;
    --text-secondary: #666666;
    --border-color: #dddddd;
    --primary-color: #4CAF50;
    --primary-hover: #45a049;
    --disabled-color: #cccccc;
  }
}
</style>
