<!-- GameView.vue - Main game view for Tootsville Vue 3 -->
<template>
  <div class="game-view">
    <!-- Game HUD with all legacy features -->
    <GameHUD />
    
    <!-- 3D World -->
    <World3D />
    
    <!-- TODO: Re-enable these components after fixing TypeScript errors -->
    <!-- Other Players -->
    <!-- <OtherPlayers /> -->
    
    <!-- World Objects -->
    <!-- <WorldObjects /> -->
    
    <!-- Animation Panel -->
    <!-- <AnimationPanel /> -->
    
    <!-- Toot Speech Panel -->
    <!-- <TootSpeechPanel /> -->
  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue'
import { useGameStore } from '@/stores/game'
import GameHUD from '@/components/Game/GameHUD.vue'
import World3D from '@/components/Game/World3D.vue'
// import OtherPlayers from '@/components/Game/OtherPlayers.vue'
// import WorldObjects from '@/components/Game/WorldObjects.vue'
// import AnimationPanel from '@/components/UI/AnimationPanel.vue'
// import TootSpeechPanel from '@/components/TootSpeechPanel.vue'

// Game store
const gameStore = useGameStore()

// Lifecycle
onMounted(() => {
  // Initialize character if not already set
  if (!gameStore.character.id) {
    gameStore.setCharacter({
      id: 'player-1',
      name: 'Player',
      avatar: 'default-avatar.png',
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0 },
      scale: { x: 1, y: 1, z: 1 },
      animation: 'idle',
      isMoving: false,
      isSitting: false,
      isJumping: false,
      isSwimming: false,
      isFlying: false,
      level: 1,
      experience: 0
    })
  }
  
  // Hide loading screen
  const bugsElement = document.getElementById('bugs')
  if (bugsElement) {
    bugsElement.classList.remove('hide-when-game-ready')
  }
})

onUnmounted(() => {
  // Reset game state when unmounting
  gameStore.resetGameState()
})
</script>

<style scoped>
.game-view {
  position: relative;
  width: 100vw;
  height: 100vh;
  overflow: hidden;
  background: linear-gradient(135deg, #87CEEB 0%, #98FB98 100%);
}
</style>

