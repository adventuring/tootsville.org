<!-- GameHUD.vue - Comprehensive HUD for Tootsville Vue 3 -->
<template>
  <div id="hud">
    <!-- Control Panel launcher icon -->
    <img 
      src="https://jumbo.tootsville.org/Assets/Icons/toots-t-logo-circled.svg"
      alt="Ⓣ" 
      id="control-panel-icon"
      title="Tootsville Control Panel"
      @click="toggleControlPanel"
    >

    <!-- Time left for child account -->
    <div id="time-left" title="Time left to play">
      <span v-if="timeLeft > 0">{{ formatTime(timeLeft) }}</span>
      <span v-else>Unlimited</span>
    </div>
    
    <!-- Equipment in use icons -->
    <div id="equip">
      <div id="active-item-box">
        <img id="active-item-gauge-left" :src="activeItem?.gaugeLeft">
        <img id="active-item" :src="activeItem?.icon" :alt="activeItem?.name">
        <img id="active-item-gauge-right" :src="activeItem?.gaugeRight">
        <span id="active-item-qty">{{ activeItem?.quantity || 0 }}</span>
        <span id="active-item-label" class="nametag">{{ activeItem?.name || 'None' }}</span>
      </div>
      <div id="inactive-item-box">
        <img id="inactive-item-gauge-left" :src="inactiveItem?.gaugeLeft">
        <img id="inactive-item" :src="inactiveItem?.icon" :alt="inactiveItem?.name">
        <img id="inactive-item-gauge-right" :src="inactiveItem?.gaugeRight">
        <span id="inactive-item-qty">{{ inactiveItem?.quantity || 0 }}</span>
        <span id="inactive-item-label" class="nametag">{{ inactiveItem?.name || 'None' }}</span>
      </div>
    </div>

    <!-- Mobile icons -->
    <div id="toolbox-mobile-phone" :style="{ visibility: isMobile ? 'visible' : 'hidden' }">
      <h3><i title="Your Mobile Device" class="fa fa-mobile-alt fa-fw"></i></h3>
      <div id="mobile-phone-info">
        <span class="tootsville-time">
          <i class="fas fa-spin fa-spinner"></i>
        </span>
        <br>
        <span title="Tootsville Date and Time" class="tootsville-date">{{ currentDate }}</span>
      </div>
    </div>

    <!-- Wallet - Peanuts -->
    <div id="wallet-peanuts">
      <span id="wallet-peanuts-icon" title="Peanuts">🥜</span>
      <span id="wallet-peanuts-display">
        <i v-if="loading" class="fas fa-spin fa-spinner"></i>
        <span v-else>{{ peanuts }}</span>
      </span>
    </div>

    <!-- Wallet - Fairy Dust -->
    <div id="wallet-fairy-dust">
      <span id="wallet-fairy-dust-icon" title="Fairy Dust">⁂</span>
      <span id="wallet-fairy-dust-display">
        <i v-if="loading" class="fas fa-spin fa-spinner"></i>
        <span v-else>{{ fairyDust }}</span>
      </span>
    </div>

    <!-- Talk Box -->
    <div id="talk-loud-menu" title="Talking Volume">
      <label>
        <input 
          type="radio" 
          name="talk-loud-menu-item"
          class="talk-loud-menu-item" 
          id="talk-loud-shout"
          title="Shout"
          :checked="talkVolume === 'shout'"
          @change="setTalkVolume('shout')"
        >
        <i class="fas fa-comment-alt"></i>
      </label>
      <label>
        <input 
          type="radio" 
          name="talk-loud-menu-item"
          class="talk-loud-menu-item" 
          id="talk-loud-talk"
          title="Talk"
          :checked="talkVolume === 'talk'"
          @change="setTalkVolume('talk')"
        >
        <i class="fas fa-comment"></i>
      </label>
      <label>
        <input 
          type="radio" 
          name="talk-loud-menu-item"
          class="talk-loud-menu-item" 
          id="talk-loud-whisper"
          title="Whisper"
          :checked="talkVolume === 'whisper'"
          @change="setTalkVolume('whisper')"
        >
        <i class="fas fa-comment-dots"></i>
      </label>
    </div>

    <!-- Expression Menu -->
    <div id="talk-expression-menu">
      <button 
        v-for="expression in expressions" 
        :key="expression.id"
        :id="expression.id"
        @click="setExpression(expression.id)"
        :title="expression.title"
      >
        <i :class="expression.icon"></i>
      </button>
    </div>

    <!-- Emoji Menu -->
    <div id="talk-emoji-menu" v-show="showEmojiMenu">
      <button id="talk-emoji-menu-close" @click="closeEmojiMenu">✗ Close</button>
      <section v-for="(emojiSection, sectionIndex) in emojiSections" :key="sectionIndex">
        <span 
          v-for="emoji in emojiSection" 
          :key="emoji"
          class="emoji"
          @click="selectEmoji(emoji)"
        >
          {{ emoji }}
        </span>
      </section>
    </div>

    <!-- Talk Box Input -->
    <div id="talk-box" :class="`talk-${talkVolume}`">
      <span id="talk-loud-selector" title="Talking volume">
        <i :class="talkVolumeIcon"></i>
      </span>
      <span id="talk-expression" style="display: none">☺</span>
      <span id="talk-emoji" title="Emoji" @click="toggleEmojiMenu">🐘</span>
      <input 
        id="talk-speak" 
        v-model="talkMessage"
        autofocus 
        autocapitalize="sentences"
        @keyup.enter="sendMessage"
        placeholder="Type your message..."
      >
      <span id="talk-speak-button" @click="sendMessage">
        <i class="fa fa-comment-alt"></i>
      </span>
    </div>

    <!-- Mini paperdoll launches inventory window -->
    <div id="paperdoll-mini" @click="openInventory">
      <img :alt="characterName" :src="characterAvatar">
    </div>

    <!-- Bug reports, troubleshooting -->
    <div id="bugs">
      <h1 class="centered hide-when-game-ready">
        <a href="https://wiki.tootsville.org/wiki/Troubleshooting_slow_loading">
          Trouble Loading? Click here for Troubleshooting.
        </a>
      </h1>
      <div id="troubleshooting-icon">
        <i class="fas fa-bug fa-4x"></i>
        <i class="fas fa-bomb fa-4x"></i>
      </div>
    </div>

    <!-- Gossip Parrots -->
    <div id="parrot" v-show="showParrot">
      <img 
        id="parrot-image" 
        alt="parrot"
        src="https://jumbo.tootsville.org/Assets/Avatars/Violet-Volts/Assets/MistParrot1080.png"
      >
      <div id="parrot-speech">
        {{ parrotMessage }}
      </div>
      <div id="parrot-reply">
        <div class="button-box">
          <button @click="closeParrot">OK</button>
        </div>
      </div>
    </div>

    <!-- Game canvas -->
    <canvas id="tootsville3d" touch-action="none"></canvas>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { useGameStore } from '@/stores/game'

// Types
interface InventoryItem {
  id: string
  name: string
  icon: string
  gaugeLeft?: string
  gaugeRight?: string
  quantity: number
}

// Game store
const gameStore = useGameStore()

// Reactive state
const loading = ref(true)
const timeLeft = ref(0)
const currentDate = ref('')
const peanuts = ref(0)
const fairyDust = ref(0)
const talkVolume = ref<'shout' | 'talk' | 'whisper'>('talk')
const talkMessage = ref('')
const showEmojiMenu = ref(false)
const showParrot = ref(false)
const parrotMessage = ref("I'm a talking parrot. Human want a cracker?")
const activeItem = ref<InventoryItem | null>(null)
const inactiveItem = ref<InventoryItem | null>(null)
const characterName = ref('Player')
const characterAvatar = ref('')

// Expressions configuration
const expressions = [
  { id: 'talk-happy', title: 'Happy', icon: 'fas fa-laugh' },
  { id: 'talk-laugh', title: 'Laugh', icon: 'fas fa-laugh-squint' },
  { id: 'talk-wink', title: 'Wink', icon: 'fas fa-grin-tongue-wink' },
  { id: 'talk-sad', title: 'Sad', icon: 'fas fa-sad-tear' },
  { id: 'talk-scared', title: 'Scared', icon: 'fas fa-grimace' },
  { id: 'talk-disgusted', title: 'Disgusted', icon: 'fas fa-tired' },
  { id: 'talk-angry', title: 'Angry', icon: 'fas fa-angry' },
  { id: 'talk-contempt', title: 'Contempt', icon: 'fas fa-meh' },
  { id: 'talk-surprised', title: 'Surprised', icon: 'fas fa-surprise' }
]

// Emoji sections (simplified for brevity)
const emojiSections = [
  ['😀', '😁', '😂', '🤣', '😃', '😄', '😅', '😆', '😉', '😊'],
  ['😋', '😎', '😍', '😘', '🥰', '😗', '😙', '😚', '☺️', '🙂'],
  ['🐶', '🐱', '🐭', '🐹', '🐰', '🦊', '🦝', '🐻', '🐼', '🦘'],
  ['❤️', '🧡', '💛', '💚', '💙', '💜', '🖤', '💔', '❣️', '💕']
]

// Computed properties
const talkVolumeIcon = computed(() => {
  switch (talkVolume.value) {
    case 'shout': return 'fas fa-comment-alt'
    case 'talk': return 'fas fa-comment'
    case 'whisper': return 'fas fa-comment-dots'
    default: return 'fas fa-comment'
  }
})

const isMobile = computed(() => {
  return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent)
})

// Methods
const toggleControlPanel = () => {
  // TODO: Implement control panel toggle
  console.log('Control panel toggled')
}

const setTalkVolume = (volume: 'shout' | 'talk' | 'whisper') => {
  talkVolume.value = volume
  // TODO: Update game state
}

const setExpression = (expressionId: string) => {
  // TODO: Set character expression
  console.log('Expression set:', expressionId)
}

const toggleEmojiMenu = () => {
  showEmojiMenu.value = !showEmojiMenu.value
}

const closeEmojiMenu = () => {
  showEmojiMenu.value = false
}

const selectEmoji = (emoji: string) => {
  talkMessage.value += emoji
  closeEmojiMenu()
}

const sendMessage = () => {
  if (talkMessage.value.trim()) {
    // TODO: Send message to game
    console.log('Sending message:', talkMessage.value)
    talkMessage.value = ''
  }
}

const openInventory = () => {
  // TODO: Open inventory window
  console.log('Opening inventory')
}

const closeParrot = () => {
  showParrot.value = false
}

const formatTime = (seconds: number): string => {
  const hours = Math.floor(seconds / 3600)
  const minutes = Math.floor((seconds % 3600) / 60)
  const secs = seconds % 60
  
  if (hours > 0) {
    return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`
  } else {
    return `${minutes}:${secs.toString().padStart(2, '0')}`
  }
}

// Lifecycle
onMounted(() => {
  // Initialize HUD data
  const timer = setInterval(() => {
    currentDate.value = new Date().toLocaleDateString('en-US', {
      weekday: 'long',
      year: 'numeric',
      month: 'long',
      day: 'numeric'
    })
  }, 1000)

  // Simulate loading completion
  setTimeout(() => {
    loading.value = false
    peanuts.value = 150
    fairyDust.value = 25
    timeLeft.value = 7200 // 2 hours
  }, 2000)

  // Cleanup timer on unmount
  onUnmounted(() => {
    clearInterval(timer)
  })
})
</script>

<style scoped>
/* HUD Styles - matching legacy Tootsville */
#hud {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  z-index: 1000;
}

#hud > * {
  pointer-events: auto;
}

/* Control Panel Icon */
#control-panel-icon {
  position: absolute;
  top: 10px;
  left: 10px;
  width: 48px;
  height: 48px;
  cursor: pointer;
  z-index: 1001;
}

/* Time Left */
#time-left {
  position: absolute;
  top: 10px;
  right: 10px;
  background: rgba(0, 0, 0, 0.7);
  color: white;
  padding: 5px 10px;
  border-radius: 5px;
  font-size: 14px;
  font-weight: bold;
}

/* Equipment */
#equip {
  position: absolute;
  top: 70px;
  left: 10px;
  display: flex;
  flex-direction: column;
  gap: 10px;
}

#active-item-box,
#inactive-item-box {
  display: flex;
  align-items: center;
  background: rgba(0, 0, 0, 0.7);
  padding: 5px;
  border-radius: 5px;
  gap: 5px;
}

#active-item,
#inactive-item {
  width: 32px;
  height: 32px;
}

#active-item-gauge-left,
#active-item-gauge-right {
  width: 16px;
  height: 32px;
}

.nametag {
  color: white;
  font-size: 12px;
  white-space: nowrap;
}

/* Mobile Phone */
#toolbox-mobile-phone {
  position: absolute;
  top: 10px;
  right: 120px;
  background: rgba(0, 0, 0, 0.7);
  color: white;
  padding: 10px;
  border-radius: 5px;
  text-align: center;
}

#toolbox-mobile-phone h3 {
  margin: 0 0 10px 0;
  font-size: 14px;
}

/* Wallet */
#wallet-peanuts,
#wallet-fairy-dust {
  position: absolute;
  top: 70px;
  right: 10px;
  background: rgba(0, 0, 0, 0.7);
  color: white;
  padding: 5px 10px;
  border-radius: 5px;
  display: flex;
  align-items: center;
  gap: 5px;
  margin-bottom: 5px;
}

#wallet-peanuts {
  top: 70px;
}

#wallet-fairy-dust {
  top: 110px;
}

/* Talk Controls */
#talk-loud-menu {
  position: absolute;
  bottom: 80px;
  left: 10px;
  display: flex;
  gap: 5px;
}

.talk-loud-menu-item {
  display: none;
}

.talk-loud-menu label {
  background: rgba(0, 0, 0, 0.7);
  color: white;
  padding: 8px;
  border-radius: 5px;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 40px;
  height: 40px;
}

.talk-loud-menu label:hover {
  background: rgba(0, 0, 0, 0.9);
}

/* Expression Menu */
#talk-expression-menu {
  position: absolute;
  bottom: 130px;
  left: 10px;
  display: flex;
  gap: 5px;
  flex-wrap: wrap;
  max-width: 200px;
}

#talk-expression-menu button {
  background: rgba(0, 0, 0, 0.7);
  color: white;
  border: none;
  padding: 8px;
  border-radius: 5px;
  cursor: pointer;
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
}

#talk-expression-menu button:hover {
  background: rgba(0, 0, 0, 0.9);
}

/* Emoji Menu */
#talk-emoji-menu {
  position: absolute;
  bottom: 200px;
  left: 10px;
  background: rgba(0, 0, 0, 0.9);
  color: white;
  padding: 10px;
  border-radius: 5px;
  max-width: 300px;
  max-height: 400px;
  overflow-y: auto;
}

#talk-emoji-menu-close {
  background: #ff4444;
  color: white;
  border: none;
  padding: 5px 10px;
  border-radius: 3px;
  cursor: pointer;
  margin-bottom: 10px;
  width: 100%;
}

.emoji {
  display: inline-block;
  padding: 2px;
  cursor: pointer;
  font-size: 20px;
}

.emoji:hover {
  background: rgba(255, 255, 255, 0.2);
  border-radius: 3px;
}

/* Talk Box */
#talk-box {
  position: absolute;
  bottom: 20px;
  left: 50%;
  transform: translateX(-50%);
  display: flex;
  align-items: center;
  background: rgba(0, 0, 0, 0.8);
  padding: 10px;
  border-radius: 25px;
  gap: 10px;
  min-width: 300px;
}

#talk-loud-selector,
#talk-expression,
#talk-emoji {
  color: white;
  cursor: pointer;
  padding: 5px;
}

#talk-speak {
  flex: 1;
  background: transparent;
  border: none;
  color: white;
  outline: none;
  font-size: 16px;
}

#talk-speak::placeholder {
  color: rgba(255, 255, 255, 0.6);
}

#talk-speak-button {
  color: white;
  cursor: pointer;
  padding: 5px;
}

/* Paperdoll */
#paperdoll-mini {
  position: absolute;
  bottom: 80px;
  right: 10px;
  cursor: pointer;
}

#paperdoll-mini img {
  width: 48px;
  height: 48px;
  border-radius: 50%;
  border: 2px solid white;
}

/* Bugs */
#bugs {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  text-align: center;
  display: none;
}

#bugs.hide-when-game-ready {
  display: block;
}

#bugs a {
  color: white;
  text-decoration: none;
  background: rgba(0, 0, 0, 0.8);
  padding: 20px;
  border-radius: 10px;
}

#troubleshooting-icon {
  position: absolute;
  bottom: 20px;
  right: 20px;
  color: #ff4444;
  cursor: pointer;
}

/* Parrot */
#parrot {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  background: rgba(0, 0, 0, 0.9);
  color: white;
  padding: 20px;
  border-radius: 10px;
  text-align: center;
  max-width: 300px;
}

#parrot-image {
  width: 64px;
  height: 64px;
  margin-bottom: 10px;
}

#parrot-speech {
  margin-bottom: 15px;
  font-style: italic;
}

.button-box button {
  background: #4CAF50;
  color: white;
  border: none;
  padding: 8px 16px;
  border-radius: 5px;
  cursor: pointer;
}

.button-box button:hover {
  background: #45a049;
}

/* Game Canvas */
#tootsville3d {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -1;
}

/* Talk volume styles */
.talk-shout #talk-loud-selector i {
  color: #ff4444;
}

.talk-talk #talk-loud-selector i {
  color: #4CAF50;
}

.talk-whisper #talk-loud-selector i {
  color: #2196F3;
}
</style>
