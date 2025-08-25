# Tootsville Vue 3 Client

A modern Vue 3 implementation of the Tootsville virtual world client, featuring TypeScript, Pinia state management, and Three.js integration.

## 🚀 Features

- **Vue 3 Composition API** - Modern reactive programming with TypeScript
- **Pinia State Management** - Centralized reactive state management
- **Three.js Integration** - 3D graphics and world rendering
- **Mobile Platform Support** - Cross-platform compatibility
- **Animation System** - Character animation with movement restrictions
- **Speech Synthesis** - Toot speech with pitch and duration control
- **Communication Services** - Multi-protocol networking (WebSocket, REST, P2P)
- **Comprehensive Testing** - Unit tests with Vitest

## 📋 Prerequisites

- Node.js 18+ 
- pnpm (recommended) or npm
- Modern web browser with WebGL support

## 🛠️ Installation

```bash
# Clone the repository
git clone https://github.com/tootsville/tootsville-vue.git
cd tootsville-vue

# Install dependencies
pnpm install

# Start development server
pnpm dev

# Run tests
pnpm test:unit

# Build for production
pnpm build
```

## 🏗️ Project Structure

```
src/
├── components/          # Vue components
│   ├── UI/             # User interface components
│   │   ├── TootSpeechPanel.vue
│   │   └── AnimationPanel.vue
│   └── Game/           # Game-specific components
├── composables/        # Vue composables
│   ├── useTootSpeech.ts
│   └── useAnimationManager.ts
├── services/           # Core services
│   ├── CommunicationService.ts
│   ├── PacketHandler.ts
│   ├── TootSpeechService.ts
│   ├── MobilePlatformService.ts
│   └── AnimationManager.ts
├── stores/             # Pinia stores
│   └── GameStore.ts
├── types/              # TypeScript type definitions
└── utils/              # Utility functions
```

## 🎮 Core Services

### CommunicationService
Multi-protocol communication service supporting WebSocket, REST, and P2P networking.

```typescript
import { communicationService } from '@/services/CommunicationService'

// Connect to server
await communicationService.connect('wss://tootsville.org')

// Send message
communicationService.send({
  type: 'chat',
  message: 'Hello, Tootsville!'
})
```

### TootSpeechService
Speech synthesis service with pitch and duration control for Toot characters.

```typescript
import { tootSpeechService } from '@/services/TootSpeechService'

// Initialize service
await tootSpeechService.initialize()

// Speak with custom parameters
tootSpeechService.speak('Hello, world!', {
  pitch: 1.2,
  duration: 0.8
})
```

### AnimationManager
Character animation state management with movement restrictions.

```typescript
import { animationManager } from '@/services/AnimationManager'

// Update character
animationManager.updateCharacter(characterData)

// Set animation
animationManager.setAnimation('walk')

// Check capabilities
const canJump = animationManager.getCapabilities().canJump
```

### MobilePlatformService
Platform detection and optimization for mobile devices, tablets, and smart TVs.

```typescript
import { mobilePlatformService } from '@/services/MobilePlatformService'

// Get platform info
const platform = mobilePlatformService.getPlatform()

// Get optimization settings
const optimizations = mobilePlatformService.getOptimizations()

// Check feature support
const hasTouch = mobilePlatformService.supportsFeature('touch')
```

## 🎯 Vue Composables

### useTootSpeech
Reactive integration of TootSpeechService with Vue components.

```vue
<script setup>
import { useTootSpeech } from '@/composables/useTootSpeech'

const { 
  isSpeaking, 
  speechStatus, 
  speak, 
  stopSpeaking 
} = useTootSpeech()
</script>
```

### useAnimationManager
Reactive integration of AnimationManager with Vue components.

```vue
<script setup>
import { useAnimationManager } from '@/composables/useAnimationManager'

const { 
  currentAnimation, 
  isMoving, 
  capabilities, 
  sit, 
  jump 
} = useAnimationManager()
</script>
```

## 📱 Platform Support

### Supported Platforms
- **Desktop**: Windows, macOS, Linux
- **Mobile**: iOS, Android
- **Tablets**: iPad, Android tablets
- **Smart TVs**: FireTV, Samsung TV, LG WebOS, Roku
- **Web Browsers**: Chrome, Firefox, Safari, Edge

### Platform Detection
The MobilePlatformService automatically detects the platform and applies appropriate optimizations:

- **Mobile**: Reduced graphics quality, virtual joystick controls
- **Tablet**: Medium graphics quality, touch-optimized interface
- **TV**: High graphics quality, remote control navigation
- **Desktop**: Full graphics quality, keyboard/mouse controls

## 🎨 Character System

### Character Types
- **Toots (Elephants)**: Cannot jump, can walk, run, sit, swim
- **Manatees**: Can only swim, cannot walk, run, jump, or fly
- **Birds/Katootels**: Can fly, jump, walk, run, sit
- **Other Characters**: Default movement capabilities

### Animation States
- `idle` - Standing still
- `walk` - Walking movement
- `run` - Running movement
- `sit` - Sitting down
- `jump` - Jumping (if capable)
- `swim` - Swimming (if capable)
- `fly` - Flying (if capable)
- `emote` - Emotional expressions
- `use_item` - Using equipment
- `talk` - Speaking animation

## 🔧 Development

### Scripts
```bash
# Development
pnpm dev              # Start development server
pnpm build            # Build for production
pnpm preview          # Preview production build

# Testing
pnpm test:unit        # Run unit tests
pnpm test:unit:run    # Run tests once
pnpm test:unit:ui     # Run tests with UI
pnpm test:unit:coverage # Run tests with coverage

# Linting
pnpm lint             # Run ESLint
pnpm lint:fix         # Fix ESLint issues

# Type checking
pnpm type-check       # Run TypeScript type checking
```

### Testing
The project uses Vitest for unit testing with comprehensive coverage:

```bash
# Run all tests
pnpm test:unit

# Run specific test file
pnpm test:unit AnimationManager.spec.ts

# Run tests with coverage
pnpm test:unit:coverage
```

### Code Quality
- **ESLint** - Code linting and style enforcement
- **Prettier** - Code formatting
- **TypeScript** - Static type checking
- **Vitest** - Unit testing framework

## 🌍 World Integration

### Communication Protocols
- **WebSocket** - Real-time bidirectional communication
- **REST API** - HTTP-based API calls
- **P2P** - Peer-to-peer networking (future)

### Packet Handling
All network communication uses schema-based validation:

```typescript
import { packetHandler } from '@/services/PacketHandler'

// Create validated packet
const packet = packetHandler.createPacket('chat', {
  message: 'Hello!',
  volume: 'normal'
})

// Process incoming packet
packetHandler.processPacket(packet)
```

## 🚀 Deployment

### Production Build
```bash
# Build for production
pnpm build

# Preview production build
pnpm preview
```

### Environment Variables
Create a `.env` file in the project root:

```env
VITE_API_URL=https://api.tootsville.org
VITE_WS_URL=wss://ws.tootsville.org
VITE_APP_VERSION=1.0.0
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines
- Follow Vue 3 Composition API patterns
- Use TypeScript for all new code
- Write unit tests for new features
- Follow the existing code style
- Update documentation as needed

## 📄 License

This project is licensed under the AGPL-3.0 License - see the [COPYING.AGPL](../COPYING.AGPL) file for details.

## 🆘 Support

- **Documentation**: [docs/PlayersGuide.html](../docs/PlayersGuide.html)
- **Issues**: [GitHub Issues](https://github.com/tootsville/tootsville-vue/issues)
- **Discord**: [Tootsville Community](https://discord.gg/tootsville)

## 🙏 Acknowledgments

- **Vue.js Team** - For the amazing Vue 3 framework
- **Three.js Team** - For 3D graphics library
- **Pinia Team** - For state management
- **Vitest Team** - For testing framework
- **Tootsville Community** - For feedback and contributions

---

**Tootsville Vue 3 Client** - Bringing the magical world of Tootsville to modern web technologies! 🐘✨
