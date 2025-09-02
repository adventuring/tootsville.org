<!-- Tootsville play/vue/tootsville-vue/src/views/HomeView.vue

Copyright © 2025 Interworldly Adventuring, LLC.

This component is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-->

<script setup lang="ts">
import { RouterLink } from 'vue-router'
</script>

<template>
  <main>
    <div class="home-container">
      <div class="hero-section">
        <h1>Welcome to Tootsville</h1>
        <p>A magical world of colorful elephants and endless adventures!</p>
        <RouterLink to="/game" class="play-button">
          Start Playing
        </RouterLink>
      </div>
      
      <div class="features-section">
        <h2>Game Features</h2>
        <div class="features-grid">
          <div class="feature-card">
            <h3>🎮 3D World</h3>
            <p>Explore a beautiful 3D world with modern graphics</p>
          </div>
          <div class="feature-card">
            <h3>🐘 Characters</h3>
            <p>Play as colorful elephants and other magical creatures</p>
          </div>
          <div class="feature-card">
            <h3>🌐 Multiplayer</h3>
            <p>Connect with friends in real-time multiplayer</p>
          </div>
          <div class="feature-card">
            <h3>🎨 Customization</h3>
            <p>Customize your character's appearance and items</p>
          </div>
        </div>
      </div>
      
      <div class="info-section">
        <h2>About Tootsville</h2>
        <p>
          Tootsville is a family-friendly MMORPG where players explore a magical world 
          inhabited by colorful elephants called "Toots". Built with modern web technologies 
          and Vue 3, it provides a smooth and engaging gaming experience.
        </p>
      </div>
    </div>
  </main>
</template>

<style scoped>
.home-container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

.hero-section {
  text-align: center;
  padding: 60px 20px;
  background: linear-gradient(135deg, #87ceeb 0%, #90ee90 100%);
  border-radius: 20px;
  margin-bottom: 40px;
  color: white;
}

.hero-section h1 {
  font-size: 3rem;
  margin-bottom: 20px;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
}

.hero-section p {
  font-size: 1.2rem;
  margin-bottom: 30px;
  opacity: 0.9;
}

.play-button {
  display: inline-block;
  background: #ff6b6b;
  color: white;
  padding: 15px 30px;
  border-radius: 50px;
  text-decoration: none;
  font-size: 1.2rem;
  font-weight: bold;
  transition: all 0.3s ease;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
}

.play-button:hover {
  background: #ff5252;
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(0, 0, 0, 0.3);
}

.features-section {
  margin-bottom: 40px;
}

.features-section h2 {
  text-align: center;
  font-size: 2.5rem;
  margin-bottom: 30px;
  color: #333;
}

.features-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 20px;
}

.feature-card {
  background: white;
  padding: 30px;
  border-radius: 15px;
  text-align: center;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
  transition: transform 0.3s ease;
}

.feature-card:hover {
  transform: translateY(-5px);
}

.feature-card h3 {
  font-size: 1.5rem;
  margin-bottom: 15px;
  color: #333;
}

.feature-card p {
  color: #666;
  line-height: 1.6;
}

.info-section {
  background: #f8f9fa;
  padding: 40px;
  border-radius: 15px;
  text-align: center;
}

.info-section h2 {
  font-size: 2rem;
  margin-bottom: 20px;
  color: #333;
}

.info-section p {
  font-size: 1.1rem;
  line-height: 1.8;
  color: #666;
  max-width: 800px;
  margin: 0 auto;
}

@media (max-width: 768px) {
  .hero-section h1 {
    font-size: 2rem;
  }
  
  .hero-section p {
    font-size: 1rem;
  }
  
  .features-grid {
    grid-template-columns: 1fr;
  }
  
  .play-button {
    padding: 12px 24px;
    font-size: 1rem;
  }
}
</style>
