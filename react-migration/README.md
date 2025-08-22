# Tootsville React Client

A modern React + Three.js client for Tootsville, preserving the existing communications protocol while providing a modern, responsive 3D interface.

## Overview

This React client replaces the legacy Babylon.js client while maintaining 100% backwards compatibility with the existing Tootsville server protocol. It provides:

- **Modern 3D Rendering**: React Three Fiber for efficient 3D graphics
- **Reactive State Management**: Zustand for predictable state updates
- **Universal Bindings**: JavaScript and JSCL Lisp interfaces
- **Static Deployment**: No runtime server dependencies beyond static file service
- **Backwards Compatibility**: Preserves existing WebSocket protocol

## Architecture

### Client-Side Components

- **App.js**: Main application component
- **GameWorld.js**: 3D world rendering and management
- **AvatarManager.js**: Avatar rendering and animation
- **WorldTerrain.js**: Procedural terrain generation
- **ItemManager.js**: Item and object management
- **WeatherSystem.js**: Weather and atmospheric effects
- **GameHUD.js**: User interface components

### Services

- **CommunicationService.js**: WebSocket communication with tootsville.net server
- **GameStore.js**: Centralized state management (Zustand)

### Bindings

- **ClientBindings.js**: Universal JavaScript interface
- **JSCLBindings.lisp**: Native Common Lisp interface

## Installation

```bash
# Install dependencies
npm install

# Start development server
npm start

# Build for production
npm run build:static
```

## Development

### Prerequisites

- Node.js 16+ 
- npm or yarn
- tootsville.net server running on localhost:5000

### Development Workflow

1. **Start the tootsville.net server**:
   ```bash
   cd lib/tootsville.net
   make run
   ```

2. **Start the React development server**:
   ```bash
   npm start
   ```

3. **Access the application**:
   - Client: http://localhost:3000
   - Server: http://localhost:5000

### Available Scripts

- `npm start` - Start development server
- `npm run build` - Build for production
- `npm run build:static` - Build static files with deployment config
- `npm test` - Run unit tests
- `npm run lint` - Run ESLint
- `npm run format` - Format code with Prettier

## Deployment

### Static File Deployment

The client is designed for static file deployment with no runtime server dependencies:

```bash
# Build static files
npm run build:static

# Deploy build/ contents to your web server
```

### Server Requirements

- **Static File Server**: HTTPS-capable web server (Apache, Nginx, etc.)
- **WebSocket Server**: tootsville.net Lisp server on port 5000
- **SSL Certificate**: Required for production (WebSocket over WSS)

### Deployment Files

The build process creates:

- `build/index.html` - Main entry point
- `build/static/` - Compiled JavaScript, CSS, and assets
- `build/.htaccess` - Apache configuration
- `build/nginx.conf` - Nginx configuration
- `build/deployment.json` - Deployment manifest

### Web Server Configuration

#### Apache (.htaccess)

```apache
RewriteEngine On
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d
RewriteRule ^(.*)$ /index.html [QSA,L]
```

#### Nginx (nginx.conf)

```nginx
location / {
    try_files $uri $uri/ /index.html;
}

location /websocket {
    proxy_pass http://localhost:5000;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
}
```

## API Reference

### JavaScript Bindings

```javascript
// Access global bindings
const bindings = window.TootsvilleBindings;

// Game actions
bindings.game.moveAvatar(x, y, z);
bindings.game.teleport(world, x, y, z);
bindings.game.sendMessage(message, type);

// Communication
bindings.communication.connect(cluster);
bindings.communication.send(command, data);

// Utilities
bindings.utils.calculateDistance(pos1, pos2);
```

### JSCL Lisp Bindings

```lisp
;; Initialize bindings
(defvar *tootsville* (make-tootsville-bindings))

;; Game actions
(tootsville-move-avatar *tootsville* 100 50 200)
(tootsville-teleport *tootsville* "CHOR" 0 0 0)
(tootsville-send-message *tootsville* "Hello, world!" :public)

;; Event handling
(tootsville-on *tootsville* "avatarUpdate"
  (lambda (data)
    (format t "Avatar updated: ~A~%" data)))
```

## Communication Protocol

The client maintains backwards compatibility with the existing Tootsville WebSocket protocol:

### Message Format

```javascript
{
  command: "move",
  data: { x: 100, y: 50, z: 200 },
  timestamp: 1640995200000
}
```

### Supported Commands

- `move` - Avatar movement
- `teleport` - World teleportation
- `message` - Chat messages
- `useItem` - Item usage
- `pong` - Connection health

### Event Types

- `connected` - Connection established
- `authenticated` - Login successful
- `avatarUpdate` - Avatar state changes
- `publicMessage` - Chat messages
- `inventoryUpdate` - Inventory changes
- `error` - Server errors

## State Management

The application uses Zustand for state management:

```javascript
import useGameStore from './stores/GameStore';

const { character, avatars, inventory, actions } = useGameStore();

// Access state
console.log(character);

// Dispatch actions
actions.moveAvatar(x, y, z);
actions.sendMessage(message);
```

## 3D Rendering

React Three Fiber provides efficient 3D rendering:

```jsx
import { Canvas } from '@react-three/fiber';
import { GameWorld } from './components/GameWorld';

<Canvas>
  <GameWorld />
  <OrbitControls />
  <ambientLight intensity={0.4} />
  <directionalLight position={[10, 10, 5]} intensity={1} />
</Canvas>
```

## Testing

```bash
# Run all tests
npm test

# Run tests in watch mode
npm test -- --watch

# Run tests with coverage
npm test -- --coverage
```

## Documentation

### JSDoc Comments

All functions include JSDoc comments for TeXinfo extraction:

```javascript
/**
 * @method moveAvatar
 * @param {number} x - X coordinate
 * @param {number} y - Y coordinate
 * @param {number} z - Z coordinate
 * @description Move avatar to specified coordinates
 */
```

### Generated Documentation

- **Developer's Guide**: TeXinfo format extracted from JSDoc
- **Player's Guide**: HTML format for end users

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Update documentation
6. Submit a pull request

## License

Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 CIWTA; © 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA

## Support

For support and questions:

- **Website**: https://interworldly.com/
- **Contact**: Interworldly Adventuring, LLC, Portland, OR, USA
