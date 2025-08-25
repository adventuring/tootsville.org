<template>
  <div class="toot-speech-panel">
    <div class="panel-header">
      <h3>Toot Speech Synthesis</h3>
      <div class="status-indicators">
        <span 
          :class="['status', { 'connected': isInitialized, 'error': error }]"
          :title="statusTooltip"
        >
          {{ statusText }}
        </span>
        <span v-if="isSpeaking" class="speaking-indicator">🔊</span>
        <span v-if="queueLength > 0" class="queue-indicator">{{ queueLength }}</span>
      </div>
    </div>

    <div class="panel-content">
      <!-- Text Input -->
      <div class="input-group">
        <label for="speech-text">Text to Speak:</label>
        <textarea
          id="speech-text"
          v-model="speechText"
          placeholder="Enter text for Toot speech synthesis..."
          rows="3"
          :disabled="!canSpeak"
        ></textarea>
      </div>

      <!-- Custom Parameters -->
      <div class="parameters-group">
        <h4>Custom Parameters</h4>
        <div class="parameter-controls">
          <div class="parameter">
            <label for="pitch">Pitch:</label>
            <input
              id="pitch"
              v-model.number="customPitch"
              type="range"
              min="0.5"
              max="2.0"
              step="0.1"
              :disabled="!canSpeak"
            />
            <span class="value">{{ customPitch }}</span>
          </div>
          <div class="parameter">
            <label for="duration">Duration:</label>
            <input
              id="duration"
              v-model.number="customDuration"
              type="range"
              min="0.5"
              max="2.0"
              step="0.1"
              :disabled="!canSpeak"
            />
            <span class="value">{{ customDuration }}</span>
          </div>
          <div class="parameter">
            <label for="volume">Volume:</label>
            <input
              id="volume"
              v-model.number="volume"
              type="range"
              min="0"
              max="1"
              step="0.1"
              :disabled="!canSpeak"
            />
            <span class="value">{{ volume }}</span>
          </div>
        </div>
      </div>

      <!-- Control Buttons -->
      <div class="control-buttons">
        <button
          @click="speak"
          :disabled="!canSpeak || !speechText.trim()"
          class="btn btn-primary"
        >
          {{ isSpeaking ? 'Speaking...' : 'Speak' }}
        </button>
        <button
          @click="queueSpeech"
          :disabled="!canSpeak || !speechText.trim()"
          class="btn btn-secondary"
        >
          Queue
        </button>
        <button
          @click="stopSpeaking"
          :disabled="!isSpeaking"
          class="btn btn-danger"
        >
          Stop
        </button>
        <button
          @click="clearSpeechQueue"
          :disabled="queueLength === 0"
          class="btn btn-warning"
        >
          Clear Queue
        </button>
      </div>

      <!-- Preset Phrases -->
      <div class="preset-phrases">
        <h4>Preset Phrases</h4>
        <div class="phrase-buttons">
          <button
            v-for="phrase in presetPhrases"
            :key="phrase"
            @click="speakPreset(phrase)"
            :disabled="!canSpeak"
            class="btn btn-outline"
          >
            {{ phrase }}
          </button>
        </div>
      </div>

      <!-- Speech History -->
      <div class="speech-history" v-if="speechHistory.length > 0">
        <h4>Speech History</h4>
        <div class="history-list">
          <div
            v-for="(entry, index) in speechHistory"
            :key="index"
            class="history-item"
          >
            <span class="history-text">{{ entry.text }}</span>
            <span class="history-time">{{ formatTime(entry.timestamp) }}</span>
          </div>
        </div>
        <button @click="clearHistory" class="btn btn-text">Clear History</button>
      </div>

      <!-- Error Display -->
      <div v-if="error" class="error-message">
        <strong>Error:</strong> {{ error }}
        <button @click="clearError" class="btn btn-text">Dismiss</button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import { useTootSpeechWithVolume } from '@/composables/useTootSpeech'
import type { SpeechParameters } from '@/services/TootSpeechService'

// Props
interface Props {
  initialVolume?: number
}

const props = withDefaults(defineProps<Props>(), {
  initialVolume: 0.5
})

// Composables
const {
  isSpeaking,
  speechStatus,
  isInitialized,
  error,
  canSpeak,
  queueLength,
  speak: speakService,
  stopSpeaking,
  queueSpeech: queueSpeechService,
  clearSpeechQueue,
  setVolume,
  clearError: clearServiceError
} = useTootSpeechWithVolume(props.initialVolume)

// Local state
const speechText = ref('')
const customPitch = ref(1.0)
const customDuration = ref(1.0)
const volume = ref(props.initialVolume)
const speechHistory = ref<Array<{ text: string; timestamp: number }>>([])

// Preset phrases
const presetPhrases = [
  'Hello, Tootsville!',
  'Welcome to our magical world!',
  'How are you today?',
  'Let\'s go on an adventure!',
  'Toot toot!',
  'This is amazing!',
  'I love this place!',
  'Time for some fun!'
]

// Computed
const statusText = computed(() => {
  if (error.value) return 'Error'
  if (isInitialized.value) return 'Ready'
  return 'Initializing...'
})

const statusTooltip = computed(() => {
  if (error.value) return `Error: ${error.value}`
  if (isInitialized.value) return 'Speech service ready'
  return 'Initializing speech service...'
})

// Methods
const speak = async () => {
  if (!speechText.value.trim()) return

  const parameters: Partial<SpeechParameters> = {
    pitch: customPitch.value,
    duration: customDuration.value,
    volume: volume.value
  }

  const success = await speakService(speechText.value, parameters)
  if (success) {
    addToHistory(speechText.value)
  }
}

const queueSpeech = () => {
  if (!speechText.value.trim()) return

  const parameters: Partial<SpeechParameters> = {
    pitch: customPitch.value,
    duration: customDuration.value,
    volume: volume.value
  }

  queueSpeechService(speechText.value, parameters)
  addToHistory(speechText.value)
}

const speakPreset = async (phrase: string) => {
  speechText.value = phrase
  await speak()
}

const addToHistory = (text: string) => {
  speechHistory.value.unshift({
    text,
    timestamp: Date.now()
  })

  // Keep only last 10 entries
  if (speechHistory.value.length > 10) {
    speechHistory.value = speechHistory.value.slice(0, 10)
  }
}

const clearHistory = () => {
  speechHistory.value = []
}

const clearError = () => {
  clearServiceError()
}

const formatTime = (timestamp: number) => {
  return new Date(timestamp).toLocaleTimeString()
}

// Watch volume changes
const handleVolumeChange = (newVolume: number) => {
  setVolume(newVolume)
}

// Lifecycle
onMounted(() => {
  // Set initial volume
  setVolume(volume.value)
})
</script>

<style scoped>
.toot-speech-panel {
  background: var(--color-background-soft);
  border: 1px solid var(--color-border);
  border-radius: 8px;
  padding: 1rem;
  max-width: 500px;
  font-family: var(--font-family);
}

.panel-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--color-border);
}

.panel-header h3 {
  margin: 0;
  color: var(--color-heading);
}

.status-indicators {
  display: flex;
  gap: 0.5rem;
  align-items: center;
}

.status {
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  font-size: 0.875rem;
  font-weight: 500;
}

.status.connected {
  background: var(--color-success);
  color: white;
}

.status.error {
  background: var(--color-danger);
  color: white;
}

.speaking-indicator {
  animation: pulse 1s infinite;
}

.queue-indicator {
  background: var(--color-warning);
  color: white;
  border-radius: 50%;
  width: 20px;
  height: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: bold;
}

.panel-content {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.input-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.input-group label {
  font-weight: 500;
  color: var(--color-text);
}

.input-group textarea {
  padding: 0.5rem;
  border: 1px solid var(--color-border);
  border-radius: 4px;
  resize: vertical;
  font-family: inherit;
}

.parameters-group h4 {
  margin: 0 0 0.5rem 0;
  color: var(--color-heading);
}

.parameter-controls {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.parameter {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.parameter label {
  min-width: 60px;
  font-size: 0.875rem;
}

.parameter input[type="range"] {
  flex: 1;
}

.parameter .value {
  min-width: 30px;
  text-align: right;
  font-size: 0.875rem;
  font-weight: 500;
}

.control-buttons {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.btn {
  padding: 0.5rem 1rem;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
  font-weight: 500;
  transition: all 0.2s;
}

.btn:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.btn-primary {
  background: var(--color-primary);
  color: white;
}

.btn-primary:hover:not(:disabled) {
  background: var(--color-primary-dark);
}

.btn-secondary {
  background: var(--color-secondary);
  color: white;
}

.btn-secondary:hover:not(:disabled) {
  background: var(--color-secondary-dark);
}

.btn-danger {
  background: var(--color-danger);
  color: white;
}

.btn-danger:hover:not(:disabled) {
  background: var(--color-danger-dark);
}

.btn-warning {
  background: var(--color-warning);
  color: white;
}

.btn-warning:hover:not(:disabled) {
  background: var(--color-warning-dark);
}

.btn-outline {
  background: transparent;
  border: 1px solid var(--color-border);
  color: var(--color-text);
}

.btn-outline:hover:not(:disabled) {
  background: var(--color-background-mute);
}

.btn-text {
  background: transparent;
  color: var(--color-text);
  text-decoration: underline;
}

.btn-text:hover:not(:disabled) {
  color: var(--color-primary);
}

.preset-phrases h4 {
  margin: 0 0 0.5rem 0;
  color: var(--color-heading);
}

.phrase-buttons {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.speech-history h4 {
  margin: 0 0 0.5rem 0;
  color: var(--color-heading);
}

.history-list {
  max-height: 200px;
  overflow-y: auto;
  border: 1px solid var(--color-border);
  border-radius: 4px;
  margin-bottom: 0.5rem;
}

.history-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.5rem;
  border-bottom: 1px solid var(--color-border);
}

.history-item:last-child {
  border-bottom: none;
}

.history-text {
  flex: 1;
  font-size: 0.875rem;
}

.history-time {
  font-size: 0.75rem;
  color: var(--color-text-light);
}

.error-message {
  background: var(--color-danger);
  color: white;
  padding: 0.75rem;
  border-radius: 4px;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}

/* Dark mode support */
@media (prefers-color-scheme: dark) {
  .toot-speech-panel {
    background: var(--color-background-soft-dark, #2a2a2a);
    border-color: var(--color-border-dark, #404040);
  }
  
  .input-group textarea {
    background: var(--color-background-dark, #1a1a1a);
    border-color: var(--color-border-dark, #404040);
    color: var(--color-text-dark, #ffffff);
  }
}
</style>
