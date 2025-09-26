<template>
  <div class="new-toot-panel">
    <h2>Create New Toot</h2>

    <!-- Character Preview -->
    <div class="character-preview">
      <h3>Character Preview</h3>
      <div class="preview-canvas" ref="previewCanvas"></div>
    </div>

    <!-- Character Name -->
    <div class="form-group">
      <label for="toot-name">Toot Name:</label>
      <input
        id="toot-name"
        v-model="newTootData.name"
        type="text"
        placeholder="Enter Toot name"
        @input="validateName"
      />
      <div v-if="nameErrors.length" class="error-messages">
        <p v-for="error in nameErrors" :key="error" class="error">{{ error }}</p>
      </div>
    </div>

    <!-- Color Selection -->
    <div class="color-selection">
      <h3>Colors</h3>

      <!-- Base Color -->
      <div class="color-group">
        <label>Base Color:</label>
        <div class="color-picker">
          <button
            v-for="color in availableColors.base"
            :key="color"
            class="color-swatch"
            :class="{ selected: newTootData.baseColor === color }"
            :style="{ backgroundColor: interpretTootColor(color) }"
            @click="selectColor('base', color)"
          ></button>
        </div>
        <button @click="randomizeColor('base')" class="random-btn">🎲</button>
      </div>

      <!-- Pad Color -->
      <div class="color-group">
        <label>Pad Color:</label>
        <div class="color-picker">
          <button
            v-for="color in availableColors.pad"
            :key="color"
            class="color-swatch"
            :class="{ selected: newTootData.padColor === color }"
            :style="{ backgroundColor: interpretTootColor(color) }"
            @click="selectColor('pad', color)"
          ></button>
        </div>
        <button @click="randomizeColor('pad')" class="random-btn">🎲</button>
      </div>

      <!-- T-Shirt Color -->
      <div class="color-group">
        <label>T-Shirt Color:</label>
        <div class="color-picker">
          <button
            v-for="color in availableColors['t-shirt']"
            :key="color"
            class="color-swatch"
            :class="{ selected: newTootData.tShirtColor === color }"
            :style="{ backgroundColor: interpretTootColor(color) }"
            @click="selectColor('t-shirt', color)"
          ></button>
        </div>
        <button @click="randomizeColor('t-shirt')" class="random-btn">🎲</button>
      </div>

      <!-- Pattern Color -->
      <div class="color-group">
        <label>Pattern Color:</label>
        <div class="color-picker">
          <button
            v-for="color in availableColors.pattern"
            :key="color"
            class="color-swatch"
            :class="{ selected: newTootData.patternColor === color }"
            :style="{ backgroundColor: interpretTootColor(color) }"
            @click="selectColor('pattern', color)"
          ></button>
        </div>
        <button @click="randomizeColor('pattern')" class="random-btn">🎲</button>
      </div>
    </div>

    <!-- Pattern Selection -->
    <div class="pattern-selection">
      <h3>Pattern</h3>
      <div class="pattern-picker">
        <button
          v-for="pattern in availablePatterns"
          :key="pattern"
          class="pattern-button"
          :class="{ selected: newTootData.pattern === pattern }"
          @click="selectPattern(pattern)"
        >
          <div
            class="pattern-preview"
            :style="{ backgroundColor: interpretTootColor(newTootData.baseColor) }"
          >
            <div
              v-if="pattern !== 'Solid'"
              class="pattern-overlay"
              :style="{ backgroundColor: interpretTootColor(newTootData.patternColor) }"
            ></div>
          </div>
          <span>{{ pattern }}</span>
        </button>
      </div>
      <button @click="randomizePattern()" class="random-btn">🎲 Random Pattern</button>
    </div>

    <!-- Randomize All -->
    <div class="randomize-all">
      <button @click="randomizeAll()" class="random-all-btn">🎲 Randomize Everything</button>
    </div>

    <!-- Create Button -->
    <div class="create-section">
      <button
        @click="createToot()"
        :disabled="!isReady"
        class="create-btn"
      >
        Create Toot
      </button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, reactive, computed, onMounted, watch } from 'vue'
import { interpretTootColor } from '@/utils/colorUtils'

// Types
interface NewTootData {
  name: string
  baseColor: string
  padColor: string
  tShirtColor: string
  patternColor: string
  pattern: string
}

// Available colors and patterns (from the original JS)
const availableColors = {
  base: ['White', 'Black', 'Red', 'Blue', 'Green', 'Yellow', 'Purple', 'Pink', 'Orange', 'Brown'],
  pad: ['White', 'Black', 'Red', 'Blue', 'Green', 'Yellow', 'Purple', 'Pink', 'Orange', 'Brown'],
  't-shirt': ['White', 'Black', 'Red', 'Blue', 'Green', 'Yellow', 'Purple', 'Pink', 'Orange', 'Brown'],
  pattern: ['White', 'Black', 'Red', 'Blue', 'Green', 'Yellow', 'Purple', 'Pink', 'Orange', 'Brown', 'Rainbow']
}

const availablePatterns = [
  'Solid', 'Stripes', 'Polka Dots', 'Checkerboard', 'Zigzag', 'Swirls', 'Hearts', 'Stars'
]

// Reactive data
const newTootData = reactive<NewTootData>({
  name: '',
  baseColor: 'White',
  padColor: 'Black',
  tShirtColor: 'Blue',
  patternColor: 'Red',
  pattern: 'Solid'
})

const nameErrors = ref<string[]>([])
const previewCanvas = ref<HTMLCanvasElement>()

// Computed properties
const isReady = computed(() => {
  return newTootData.name.length >= 3 &&
         newTootData.name.length <= 32 &&
         nameErrors.value.length === 0
})

// Methods
const validateName = () => {
  nameErrors.value = []

  if (newTootData.name.length < 3) {
    nameErrors.value.push('Name must be at least 3 characters')
  }

  if (newTootData.name.length > 32) {
    nameErrors.value.push('Name must be at most 32 characters')
  }

  if (!/^[a-zA-Z]/.test(newTootData.name)) {
    nameErrors.value.push('Name must start with a letter')
  }

  if (!/^[a-zA-Z0-9\s\-_]+$/.test(newTootData.name)) {
    nameErrors.value.push('Name can only contain letters, numbers, spaces, hyphens, and underscores')
  }
}

const selectColor = (type: keyof typeof availableColors, color: string) => {
  switch (type) {
    case 'base':
      newTootData.baseColor = color
      break
    case 'pad':
      newTootData.padColor = color
      break
    case 't-shirt':
      newTootData.tShirtColor = color
      break
    case 'pattern':
      newTootData.patternColor = color
      break
  }
  updatePreview()
}

const selectPattern = (pattern: string) => {
  newTootData.pattern = pattern
  updatePreview()
}

const randomizeColor = (type: keyof typeof availableColors) => {
  const colors = availableColors[type]
  const randomColor = colors[Math.floor(Math.random() * colors.length)]
  selectColor(type, randomColor)
}

const randomizePattern = () => {
  const randomPattern = availablePatterns[Math.floor(Math.random() * availablePatterns.length)]
  selectPattern(randomPattern)
}

const randomizeAll = () => {
  randomizeColor('base')
  randomizeColor('pad')
  randomizeColor('t-shirt')
  randomizeColor('pattern')
  randomizePattern()
}

const updatePreview = () => {
  // Update the character preview
  // This would integrate with the AvatarViewer or 3D rendering system
  console.log('Updating character preview with:', newTootData)
}

const createToot = async () => {
  if (!isReady.value) return

  try {
    // Call the API to create the new Toot
    console.log('Creating Toot with data:', newTootData)

    // Reset form
    newTootData.name = ''
    newTootData.baseColor = 'White'
    newTootData.padColor = 'Black'
    newTootData.tShirtColor = 'Blue'
    newTootData.patternColor = 'Red'
    newTootData.pattern = 'Solid'

  } catch (error) {
    console.error('Failed to create Toot:', error)
  }
}

// Lifecycle
onMounted(() => {
  // Initialize with random values
  randomizeAll()
  validateName()
})

// Watchers
watch(() => newTootData.name, validateName)
</script>

<style scoped>
.new-toot-panel {
  max-width: 800px;
  margin: 0 auto;
  padding: 20px;
  background: #f9f9f9;
  border-radius: 8px;
}

.character-preview {
  text-align: center;
  margin-bottom: 30px;
}

.preview-canvas {
  width: 200px;
  height: 200px;
  border: 2px solid #ccc;
  border-radius: 8px;
  margin: 0 auto;
  background: #fff;
}

.form-group {
  margin-bottom: 20px;
}

.form-group label {
  display: block;
  margin-bottom: 5px;
  font-weight: bold;
}

.form-group input {
  width: 100%;
  padding: 8px;
  border: 1px solid #ccc;
  border-radius: 4px;
}

.error-messages {
  margin-top: 5px;
}

.error {
  color: #d32f2f;
  font-size: 0.875em;
  margin: 2px 0;
}

.color-selection, .pattern-selection {
  margin-bottom: 30px;
}

.color-group, .pattern-picker {
  margin-bottom: 15px;
}

.color-group label {
  display: block;
  margin-bottom: 8px;
  font-weight: bold;
}

.color-picker {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-bottom: 10px;
}

.color-swatch {
  width: 30px;
  height: 30px;
  border: 2px solid #ccc;
  border-radius: 50%;
  cursor: pointer;
  transition: border-color 0.2s;
}

.color-swatch:hover {
  border-color: #666;
}

.color-swatch.selected {
  border-color: #000;
  box-shadow: 0 0 0 2px #000;
}

.random-btn {
  padding: 6px 12px;
  background: #f0f0f0;
  border: 1px solid #ccc;
  border-radius: 4px;
  cursor: pointer;
}

.pattern-picker {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
  gap: 10px;
}

.pattern-button {
  padding: 10px;
  border: 2px solid #ccc;
  border-radius: 8px;
  background: #fff;
  cursor: pointer;
  transition: border-color 0.2s;
}

.pattern-button:hover {
  border-color: #666;
}

.pattern-button.selected {
  border-color: #000;
  box-shadow: 0 0 0 2px #000;
}

.pattern-preview {
  width: 60px;
  height: 60px;
  border-radius: 4px;
  margin: 0 auto 5px;
  position: relative;
  overflow: hidden;
}

.pattern-overlay {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  opacity: 0.7;
  background-image:
    repeating-linear-gradient(45deg, transparent, transparent 5px, rgba(255,255,255,0.5) 5px, rgba(255,255,255,0.5) 10px);
}

.pattern-button span {
  display: block;
  text-align: center;
  font-size: 0.875em;
}

.randomize-all {
  text-align: center;
  margin-bottom: 30px;
}

.random-all-btn {
  padding: 12px 24px;
  background: #4CAF50;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1.1em;
}

.random-all-btn:hover {
  background: #45a049;
}

.create-section {
  text-align: center;
}

.create-btn {
  padding: 15px 30px;
  background: #2196F3;
  color: white;
  border: none;
  border-radius: 8px;
  cursor: pointer;
  font-size: 1.2em;
  font-weight: bold;
}

.create-btn:hover:not(:disabled) {
  background: #1976D2;
}

.create-btn:disabled {
  background: #ccc;
  cursor: not-allowed;
}
</style>


