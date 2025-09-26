<template>
  <div class="infinity-mode-manager">
    <div class="connection-status">
      <div class="status-indicator"
           :class="{
             'connected': smartFox.connectionState === 'connected' || smartFox.connectionState === 'loggedIn',
             'connecting': smartFox.connectionState === 'connecting' || smartFox.connectionState === 'loggingIn',
             'disconnected': smartFox.connectionState === 'disconnected',
             'error': connectionError
           }">
      </div>
      <span class="status-text">{{ connectionStatusText }}</span>
    </div>

    <div v-if="connectionError" class="error-message">
      {{ connectionError }}
    </div>

    <!-- Login Form -->
    <div v-if="!isLoggedIn" class="login-form">
      <h3>Login to Tootsville</h3>
      <form @submit.prevent="handleLogin">
        <div class="form-group">
          <label for="username">Username:</label>
          <input
            id="username"
            v-model="loginForm.username"
            type="text"
            required
          />
        </div>
        <div class="form-group">
          <label for="password">Password:</label>
          <input
            id="password"
            v-model="loginForm.password"
            type="password"
            required
          />
        </div>
        <button type="submit" :disabled="isConnecting">
          {{ isConnecting ? 'Connecting...' : 'Login' }}
        </button>
      </form>
    </div>

    <!-- Game Interface -->
    <div v-else class="game-interface">
      <div class="user-info">
        <h3>Welcome, {{ currentUser?.name }}!</h3>
        <div class="user-stats">
          <span>Peanuts: {{ balance }}</span>
          <span>Room: {{ currentRoom?.name || 'None' }}</span>
        </div>
      </div>

      <!-- Chat Interface -->
      <div class="chat-section">
        <div class="chat-messages" ref="chatMessages">
          <div
            v-for="message in chatMessages"
            :key="message.id"
            class="chat-message"
            :class="{ 'own-message': message.from?.id === currentUser?.id }"
          >
            <span class="message-sender">{{ message.from?.name || 'System' }}:</span>
            <span class="message-text">{{ message.data.message }}</span>
            <span class="message-time">{{ formatTime(message.timestamp) }}</span>
          </div>
        </div>
        <div class="chat-input">
          <input
            v-model="chatInput"
            @keyup.enter="sendChatMessage"
            placeholder="Type a message..."
            type="text"
          />
          <button @click="sendChatMessage" :disabled="!chatInput.trim()">
            Send
          </button>
        </div>
      </div>

      <!-- Quick Actions -->
      <div class="quick-actions">
        <button @click="joinRandomRoom" :disabled="!isLoggedIn">
          Join Random Room
        </button>
        <button @click="leaveRoom" :disabled="!currentRoom">
          Leave Room
        </button>
        <button @click="refreshBalance" :disabled="!isLoggedIn">
          Refresh Balance
        </button>
      </div>

      <!-- Debug Panel (only in development) -->
      <div v-if="isDevelopment" class="debug-panel">
        <h4>Debug Information</h4>
        <div class="debug-info">
          <div>Connection State: {{ smartFox.connectionState }}</div>
          <div>Current Room: {{ currentRoom?.id || 'None' }}</div>
          <div>Active Requests: {{ amf.pendingRequests }}</div>
          <div>Users in Room: {{ currentRoom?.userCount || 0 }}</div>
          <div>Network Status: {{ amf.isOnline ? 'Online' : 'Offline' }}</div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted, nextTick } from 'vue'
import { useSmartFox, destroySmartFox, MessageType } from './SmartFoxManager'
import { useAMF, destroyAMF, APIEndpoint } from './AMFManager'

// Reactive data
const loginForm = ref({
  username: '',
  password: ''
})

const chatInput = ref('')
const chatMessages = ref<Array<any>>([])
const connectionError = ref('')
const balance = ref(0)
const messageId = ref(0)

// Managers
const smartFox = useSmartFox({
  host: 'localhost',
  port: 5004,
  debug: true
})

const amf = useAMF({
  baseUrl: 'http://localhost:5000/api',
  debug: true
})

// Computed properties
const isLoggedIn = computed(() =>
  smartFox.connectionState === 'loggedIn'
)

const isConnecting = computed(() =>
  smartFox.connectionState === 'connecting' ||
  smartFox.connectionState === 'loggingIn'
)

const connectionStatusText = computed(() => {
  switch (smartFox.connectionState) {
    case 'disconnected': return 'Disconnected'
    case 'connecting': return 'Connecting...'
    case 'connected': return 'Connected'
    case 'loggingIn': return 'Logging in...'
    case 'loggedIn': return 'Logged in'
    case 'disconnecting': return 'Disconnecting...'
    default: return 'Unknown'
  }
})

const currentUser = computed(() => smartFox.currentUser)
const currentRoom = computed(() => smartFox.currentRoom)
const isDevelopment = computed(() => import.meta.env.DEV)

// Methods
const handleLogin = async () => {
  try {
    connectionError.value = ''

    // Connect to WebSocket first
    await smartFox.connect()

    // Login via WebSocket
    await smartFox.login(loginForm.value.username, loginForm.value.password)

    // Also login via REST API for additional features
    const apiResponse = await amf.login(loginForm.value.username, loginForm.value.password)
    if (apiResponse.success && apiResponse.data?.token) {
      amf.setAuthToken(apiResponse.data.token)
    }

    // Load initial data
    await loadUserData()

  } catch (error: any) {
    connectionError.value = error.message || 'Login failed'
    console.error('Login error:', error)
  }
}

const loadUserData = async () => {
  try {
    // Get user balance
    const balanceResponse = await amf.getBalance()
    if (balanceResponse.success) {
      balance.value = balanceResponse.data?.balance || 0
    }

    // Get user profile
    const profileResponse = await amf.getUserProfile()
    if (profileResponse.success) {
      // Update user profile data
      console.log('User profile:', profileResponse.data)
    }
  } catch (error) {
    console.error('Failed to load user data:', error)
  }
}

const sendChatMessage = async () => {
  if (!chatInput.value.trim()) return

  try {
    if (currentRoom.value) {
      await smartFox.sendPublicMessage(chatInput.value.trim())
    } else {
      // Send via REST API if not in a room
      await amf.post(APIEndpoint.SEND_MESSAGE, {
        message: chatInput.value.trim(),
        global: true
      })
    }

    chatInput.value = ''
  } catch (error) {
    console.error('Failed to send message:', error)
  }
}

const joinRandomRoom = async () => {
  try {
    const rooms = await smartFox.getRoomList()
    if (rooms.length > 0) {
      const randomRoom = rooms[Math.floor(Math.random() * rooms.length)]
      await smartFox.joinRoom(randomRoom.id)
      addSystemMessage(`Joined room: ${randomRoom.name}`)
    } else {
      addSystemMessage('No rooms available')
    }
  } catch (error) {
    console.error('Failed to join room:', error)
    addSystemMessage('Failed to join room')
  }
}

const leaveRoom = async () => {
  try {
    await smartFox.leaveRoom()
    addSystemMessage('Left room')
  } catch (error) {
    console.error('Failed to leave room:', error)
  }
}

const refreshBalance = async () => {
  try {
    const response = await amf.getBalance()
    if (response.success) {
      balance.value = response.data?.balance || 0
      addSystemMessage(`Balance: ${balance.value} peanuts`)
    }
  } catch (error) {
    console.error('Failed to refresh balance:', error)
  }
}

const addSystemMessage = (message: string) => {
  chatMessages.value.push({
    id: ++messageId.value,
    from: null,
    data: { message },
    timestamp: Date.now(),
    type: 'system'
  })
  scrollToBottom()
}

const formatTime = (timestamp: number) => {
  return new Date(timestamp).toLocaleTimeString()
}

const scrollToBottom = async () => {
  await nextTick()
  const chatContainer = document.querySelector('.chat-messages')
  if (chatContainer) {
    chatContainer.scrollTop = chatContainer.scrollHeight
  }
}

// Event handlers
const handlePublicMessage = (message: any) => {
  chatMessages.value.push({
    id: ++messageId.value,
    ...message
  })
  scrollToBottom()
}

const handlePrivateMessage = (message: any) => {
  chatMessages.value.push({
    id: ++messageId.value,
    ...message,
    data: { message: `[Private] ${message.data.message}` }
  })
  scrollToBottom()
}

const handleConnectionLost = () => {
  connectionError.value = 'Connection lost. Attempting to reconnect...'
}

const handleLoginSuccess = (data: any) => {
  connectionError.value = ''
  addSystemMessage('Successfully logged in!')
}

const handleRoomJoined = (data: any) => {
  addSystemMessage(`Joined room: ${data.room.name}`)
}

const handleRoomLeft = (data: any) => {
  addSystemMessage('Left room')
}

const handleUserEnterRoom = (data: any) => {
  addSystemMessage(`${data.user.name} entered the room`)
}

const handleUserExitRoom = (data: any) => {
  const user = smartFox.users.get(data.userId)
  if (user) {
    addSystemMessage(`${user.name} left the room`)
  }
}

// Setup event listeners
onMounted(() => {
  // SmartFox event listeners
  smartFox.on(MessageType.PUBLIC_MESSAGE, handlePublicMessage)
  smartFox.on(MessageType.PRIVATE_MESSAGE, handlePrivateMessage)
  smartFox.on('connectionLost', handleConnectionLost)
  smartFox.on('loginSuccess', handleLoginSuccess)
  smartFox.on('roomJoined', handleRoomJoined)
  smartFox.on('roomLeft', handleRoomLeft)
  smartFox.on(MessageType.USER_ENTER_ROOM, handleUserEnterRoom)
  smartFox.on(MessageType.USER_EXIT_ROOM, handleUserExitRoom)

  // Add welcome message
  addSystemMessage('Welcome to Tootsville! Please login to continue.')
})

// Cleanup
onUnmounted(() => {
  destroySmartFox()
  destroyAMF()
})
</script>

<style scoped>
.infinity-mode-manager {
  max-width: 800px;
  margin: 0 auto;
  padding: 20px;
  font-family: Arial, sans-serif;
}

.connection-status {
  display: flex;
  align-items: center;
  margin-bottom: 20px;
  padding: 10px;
  background-color: #f5f5f5;
  border-radius: 5px;
}

.status-indicator {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  margin-right: 10px;
}

.status-indicator.connected {
  background-color: #4caf50;
}

.status-indicator.connecting {
  background-color: #ff9800;
  animation: pulse 1s infinite;
}

.status-indicator.disconnected {
  background-color: #f44336;
}

.status-indicator.error {
  background-color: #f44336;
}

@keyframes pulse {
  0% { opacity: 1; }
  50% { opacity: 0.5; }
  100% { opacity: 1; }
}

.error-message {
  color: #f44336;
  background-color: #ffebee;
  padding: 10px;
  border-radius: 5px;
  margin-bottom: 20px;
}

.login-form, .game-interface {
  background-color: #f9f9f9;
  padding: 20px;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.form-group {
  margin-bottom: 15px;
}

.form-group label {
  display: block;
  margin-bottom: 5px;
  font-weight: bold;
}

.form-group input {
  width: 100%;
  padding: 8px;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 16px;
}

button {
  background-color: #2196f3;
  color: white;
  border: none;
  padding: 10px 20px;
  border-radius: 4px;
  cursor: pointer;
  font-size: 16px;
}

button:hover:not(:disabled) {
  background-color: #1976d2;
}

button:disabled {
  background-color: #cccccc;
  cursor: not-allowed;
}

.user-info {
  margin-bottom: 20px;
  padding-bottom: 15px;
  border-bottom: 1px solid #ddd;
}

.user-stats {
  display: flex;
  gap: 20px;
  margin-top: 10px;
}

.chat-section {
  margin-bottom: 20px;
}

.chat-messages {
  height: 300px;
  overflow-y: auto;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 10px;
  background-color: white;
  margin-bottom: 10px;
}

.chat-message {
  margin-bottom: 8px;
  padding: 5px;
  border-radius: 4px;
}

.chat-message.own-message {
  background-color: #e3f2fd;
  text-align: right;
}

.message-sender {
  font-weight: bold;
  margin-right: 5px;
}

.message-time {
  font-size: 0.8em;
  color: #666;
  margin-left: 10px;
}

.chat-input {
  display: flex;
  gap: 10px;
}

.chat-input input {
  flex: 1;
  padding: 8px;
  border: 1px solid #ddd;
  border-radius: 4px;
}

.quick-actions {
  display: flex;
  gap: 10px;
  margin-bottom: 20px;
  flex-wrap: wrap;
}

.debug-panel {
  margin-top: 20px;
  padding: 15px;
  background-color: #f5f5f5;
  border-radius: 5px;
  font-family: monospace;
  font-size: 0.9em;
}

.debug-info div {
  margin-bottom: 5px;
}

@media (max-width: 600px) {
  .infinity-mode-manager {
    padding: 10px;
  }

  .quick-actions {
    flex-direction: column;
  }

  .user-stats {
    flex-direction: column;
    gap: 5px;
  }
}
</style>
