<template>
  <group
    :position="characterPosition"
    :rotation="characterRotation"
    :scale="characterScale"
    @click="onCharacterClick"
  >
    <!-- Character Model -->
    <mesh
      ref="characterMesh"
      cast-shadow
      receive-shadow
      :user-data="{ type: 'player', id: character.id }"
    >
      <!-- Toot (Elephant) Model -->
      <cylinderGeometry
        v-if="isToot"
        :args="[0.5, 0.5, 2, 8]"
      />
      <!-- Bird/Katootel Model -->
      <coneGeometry
        v-else-if="isBird"
        :args="[0.3, 1.5, 8]"
      />
      <!-- Manatee Model -->
      <capsuleGeometry
        v-else-if="isManatee"
        :args="[0.8, 2, 4, 8]"
      />
      <!-- Default Model -->
      <boxGeometry
        v-else
        :args="[1, 2, 1]"
      />
      
      <meshStandardMaterial
        :color="characterColor"
        :roughness="0.8"
        :metalness="0.1"
      />
    </mesh>

    <!-- Character Name Tag -->
    <Html :position="[0, 2.5, 0]">
      <div class="character-name">
        {{ character.name }}
        <div v-if="isChildAccount" class="diamond-icon">💎</div>
      </div>
    </Html>

    <!-- Speech Bubble -->
    <Html
      v-if="isSpeaking"
      :position="[0, 3, 0]"
    >
      <div class="speech-bubble">
        {{ currentSpeech }}
      </div>
    </Html>

    <!-- Animation Effects -->
    <group v-if="showAnimationEffects">
      <!-- Walking Dust -->
      <mesh
        v-if="animationState.current === 'walk'"
        :position="[0, 0.1, 0]"
      >
        <sphereGeometry :args="[0.1, 4, 4]" />
        <meshBasicMaterial :color="0x8B4513" :transparent="true" :opacity="0.6" />
      </mesh>

      <!-- Swimming Splash -->
      <mesh
        v-if="animationState.current === 'swim'"
        :position="[0, 0.5, 0]"
      >
        <sphereGeometry :args="[0.2, 6, 6]" />
        <meshBasicMaterial :color="0x87CEEB" :transparent="true" :opacity="0.4" />
      </mesh>

      <!-- Flying Sparkles -->
      <mesh
        v-if="animationState.current === 'fly'"
        :position="[0, 1, 0]"
      >
        <sphereGeometry :args="[0.05, 4, 4]" />
        <meshBasicMaterial :color="0xFFD700" :transparent="true" :opacity="0.8" />
      </mesh>
    </group>

    <!-- Equipment -->
    <Equipment
      v-if="character.equipment"
      :equipment="character.equipment"
      :position="[0, 1, 0]"
    />
  </group>
</template>

<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { Vector3, Euler } from 'three'
import { Html } from 'vue-three'
import { useFrame } from 'vue-three'
import Equipment from './Equipment.vue'

// Props
interface Props {
  character: {
    id: string
    name: string
    avatar: string
    characterType?: string
    species?: string
    position: Vector3
    rotation: Euler
    scale: Vector3
    equipment?: any
    isChildAccount?: boolean
  }
  animationState: {
    current: string
    isMoving: boolean
    capabilities: any
  }
}

const props = defineProps<Props>()

// Emits
const emit = defineEmits<{
  characterClicked: [character: any]
  characterMoved: [position: Vector3]
}>()

// Refs
const characterMesh = ref()

// Reactive state
const isSpeaking = ref(false)
const currentSpeech = ref('')
const showAnimationEffects = ref(true)

// Computed
const characterPosition = computed(() => props.character.position)
const characterRotation = computed(() => props.character.rotation)
const characterScale = computed(() => props.character.scale)

const isToot = computed(() => {
  const type = props.character.characterType || props.character.species || ''
  const name = props.character.name.toLowerCase()
  const avatar = props.character.avatar.toLowerCase()
  
  return type.includes('toot') || 
         type.includes('elephant') || 
         name.includes('toot') || 
         avatar.includes('toot') ||
         ['zap', 'flora', 'sparkle', 'moo', 'superstar', 'lil mc', 'cupid', 'dottie'].some(keyword => 
           name.includes(keyword) || avatar.includes(keyword)
         )
})

const isBird = computed(() => {
  const type = props.character.characterType || props.character.species || ''
  const name = props.character.name.toLowerCase()
  const avatar = props.character.avatar.toLowerCase()
  
  return type.includes('bird') || 
         type.includes('katootel') || 
         name.includes('bird') || 
         avatar.includes('bird') ||
         name.includes('katootel') || 
         avatar.includes('katootel')
})

const isManatee = computed(() => {
  const type = props.character.characterType || props.character.species || ''
  const name = props.character.name.toLowerCase()
  const avatar = props.character.avatar.toLowerCase()
  
  return type.includes('manatee') || 
         type.includes('sea cow') || 
         name.includes('manatee') || 
         avatar.includes('manatee')
})

const isChildAccount = computed(() => props.character.isChildAccount || false)

const characterColor = computed(() => {
  if (isToot.value) {
    // Toot colors based on character
    const colors = {
      zap: 0xFFD700,      // Gold
      flora: 0x90EE90,    // Light green
      sparkle: 0xFF69B4,  // Pink
      moo: 0x8B4513,      // Brown
      superstar: 0xFF4500, // Orange red
      'lil mc': 0x9370DB,  // Purple
      cupid: 0xFF1493,    // Deep pink
      dottie: 0xFF69B4    // Pink with dots
    }
    
    const name = props.character.name.toLowerCase()
    for (const [key, color] of Object.entries(colors)) {
      if (name.includes(key)) {
        return color
      }
    }
    
    return 0x87CEEB // Default sky blue
  }
  
  if (isBird.value) {
    return 0x32CD32 // Lime green for birds
  }
  
  if (isManatee.value) {
    return 0x4682B4 // Steel blue for manatees
  }
  
  return 0x87CEEB // Default sky blue
})

// Methods
const onCharacterClick = () => {
  emit('characterClicked', props.character)
}

const speak = (text: string, duration: number = 3000) => {
  currentSpeech.value = text
  isSpeaking.value = true
  
  setTimeout(() => {
    isSpeaking.value = false
    currentSpeech.value = ''
  }, duration)
}

const updatePosition = (newPosition: Vector3) => {
  props.character.position.copy(newPosition)
  emit('characterMoved', newPosition)
}

// Animation effects
useFrame(({ clock }) => {
  if (characterMesh.value) {
    // Subtle breathing animation
    const time = clock.getElapsedTime()
    const breathingScale = 1 + Math.sin(time * 2) * 0.02
    characterMesh.value.scale.y = breathingScale
    
    // Movement animation
    if (props.animationState.isMoving) {
      const bounceHeight = Math.sin(time * 8) * 0.1
      characterMesh.value.position.y = bounceHeight
    } else {
      characterMesh.value.position.y = 0
    }
  }
})

// Watch for animation changes
watch(() => props.animationState.current, (newAnimation) => {
  console.log(`Character ${props.character.name} animation: ${newAnimation}`)
  
  // Handle animation-specific effects
  if (newAnimation === 'talk') {
    speak('Hello!', 2000)
  }
})

// Expose methods
defineExpose({
  speak,
  updatePosition
})

// Lifecycle
onMounted(() => {
  console.log(`PlayerCharacter mounted for ${props.character.name}`)
})
</script>

<style scoped>
.character-name {
  background: rgba(0, 0, 0, 0.7);
  color: white;
  padding: 4px 8px;
  border-radius: 12px;
  font-size: 12px;
  font-weight: bold;
  text-align: center;
  white-space: nowrap;
  pointer-events: none;
  text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.8);
  display: flex;
  align-items: center;
  gap: 4px;
}

.diamond-icon {
  font-size: 10px;
  animation: sparkle 2s infinite;
}

.speech-bubble {
  background: white;
  color: black;
  padding: 8px 12px;
  border-radius: 16px;
  font-size: 12px;
  max-width: 150px;
  word-wrap: break-word;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
  position: relative;
  pointer-events: none;
}

.speech-bubble::after {
  content: '';
  position: absolute;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%);
  width: 0;
  height: 0;
  border-left: 8px solid transparent;
  border-right: 8px solid transparent;
  border-top: 8px solid white;
}

@keyframes sparkle {
  0%, 100% { opacity: 1; transform: scale(1); }
  50% { opacity: 0.7; transform: scale(1.2); }
}

/* Platform-specific styles */
@media (max-width: 768px) {
  .character-name {
    font-size: 10px;
    padding: 2px 6px;
  }
  
  .speech-bubble {
    font-size: 10px;
    max-width: 120px;
    padding: 6px 10px;
  }
}

@media (min-width: 1920px) {
  .character-name {
    font-size: 14px;
    padding: 6px 12px;
  }
  
  .speech-bubble {
    font-size: 14px;
    max-width: 180px;
    padding: 10px 16px;
  }
}
</style>
