# Tootsville Development Guide

## Color Palette

**IMPORTANT**: All colors should prefer to use the designated canonical palette defined in `play/react/src/constants/Colors.js`. 

The canonical colors are:
- **Blue**: `#0082c8` - Primary brand color
- **Violet**: `#9669ad` - Primary brand color  
- **Silver**: `#ddd`
- **Charcoal**: `#333`
- **White**: `#fff`
- **Black**: `#000`
- **DeepPurple**: `#b117ff`
- **Yellow**: `#fff216`
- **Pink**: `#e73e97`
- **Cyan**: `#ccffff`
- **Turquoise**: `#00a290`
- **Periwinkle**: `#96b4de`
- **Gold**: `#f7d023`
- **Burgundy**: `#9c0059`
- **Green**: `#7ac142`
- **Lavender**: `#ba9dca`
- **Tan**: `#ffd2a0`
- **Red**: `#e51b24`
- **SpringGreen**: `#c4d82d`
- **Indigo**: `#0028ff`
- **Orange**: `#ff7b26`

### Usage

```javascript
import { Colors } from '../constants/Colors';

// Use canonical color names
const elementStyle = {
  backgroundColor: Colors.Blue,
  color: Colors.White
};
```

## Performance Guidelines

### Baseline Machine Requirements

The baseline machine is a 4-year-old Chromebook or similar age PC running either ChromeOS or Fedora Linux, or anything more powerful.

### Quality Settings

- **Low**: Optimized for baseline machines
- **Medium**: Balanced performance and quality
- **High**: Enhanced visuals for capable machines
- **Ultra**: Maximum quality for high-end systems

### Performance Monitoring

- Shows a turtle icon when frame rate drops below 25 FPS
- No manual FPS display option
- Resolution is always auto-detected
- No manual frame rate limiting

## Input Handling

### Supported Input Methods

1. **Cursor Keys** - Arrow keys for movement
2. **Mouse Clicks** - Walk-to-point and object interaction
3. **Gamepad Input** - Full gamepad support including remote controls with action buttons
4. **Touch Input** - Mobile touch controls

### Unsupported Input

- **WASD Keys** - Not supported, use arrow keys instead
- **Mouse Inversion** - Not supported, let OS handle mouse
- **Manual Mouse Sensitivity** - Not supported, let OS handle mouse

## Audio Settings

### Volume Controls

- **Master Volume** - Overall audio level
- **Music Volume** - Background music
- **SFX Volume** - Sound effects
- **Speech Volume** - In-game character speech
- **Voice Chat (Incoming)** - Other players' voice
- **Voice Chat (Outgoing)** - Your voice transmission

## Error Handling

### Parrot Interface

All client-side errors should use the parrot interface for consistency:

```javascript
import { handleErrorWithParrot } from './components/UI/ParrotErrorHandler';

try {
  // Some operation
} catch (error) {
  handleErrorWithParrot(error, () => {
    // Error dismissed callback
  });
}
```

## UI Scale Effects

### Small (75%)
- More screen space for game world
- Smaller interface elements
- May be harder to read on some displays

### Normal (100%)
- Standard interface size
- Optimized for baseline machines
- Balanced screen space allocation

### Large (125%)
- Easier to read on high-DPI displays
- Larger interface elements
- Less screen space for game world

## Communication

### Volume Control
- Use the volume button to whisper (quiet) or shout (loud)
- No slash commands for volume control

### Emotes
- Type emotes like `/smile`, `/frown`, `/wave` to express yourself
- No `/help` or `/whisper` commands

## Platform Support

### Mobile Platforms
- Android (WebView, PWA, Capacitor)
- iOS (WebView, PWA, Capacitor)
- iPad (WebView, PWA, Capacitor)

### TV Platforms
- Amazon Fire TV
- Samsung Smart TV
- LG WebOS TV
- Roku TV
- Web TV

### Desktop Platforms
- Windows 8, 10, 11
- macOS
- Linux

### Gaming Platforms
- Steam
- Oculus Quest

### Wearable Platforms
- Apple Watch
- Android Wear

### Smart Home Platforms
- Amazon Alexa
- Google Home

### Embedded Platforms
- Raspberry Pi
- Digital Kiosk

## Testing

### Unit Tests
- Test all edge cases including `nil`, empty, malformed inputs
- Test invalid inputs, out-of-range values, and boundary conditions
- Test error conditions and recovery

### Integration Tests
- Test component interactions
- Test communication with backend services
- Test error handling flows

## Code Style

### Copyright Headers
All new files should include:
```
Copyright © 2025 Interworldly Adventuring, LLC.
This program is Free Software; Refer to COPYING.AGPL for details.
```

### Documentation
- Use JSDoc for all functions and components
- Include `@author` and `@version` tags
- Document parameters, return values, and examples

### Error Handling
- Use appropriate error classes with data slots
- Include custom print methods for Lisp errors
- Signal errors with relevant context information
