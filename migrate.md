# Tootsville.org Work In Progress (WIP) Document

**Project:** Tootsville.org 5.0.0 (frontend) / Tootsville.net 2.0 (backend)  
**Date:** 2025-01-22  
**Status:** Active Development - MariaDB Testing Docker Image, Server Build Issues, Authentication Updates, Equipment Implementation  
**Document Purpose:** Comprehensive tracking of all requested changes and discovered requirements

## Executive Summary

This document tracks all changes requested by the user and requirements discovered during development. The project involves a React migration of the Tootsville virtual world game, with both frontend (React/Three.js) and backend (Common Lisp) components requiring comprehensive testing, modernization, and feature updates.

## Primary User Requests

### 1. Authentication System Updates

**Request:** Remove Twitter sign-in option and provide modern alternatives alongside Google and Yahoo

**Specific Changes Made:**
- Removed Twitter authentication from `play/Login/Login.js`
- Replaced Twitter with Apple Sign-In (`apple.com` OAuth provider)
- Added Bluesky authentication (`bluesky.social` OAuth provider)
- Updated UI text constants in `react-migration/src/constants/UIText.js` to include:
  - `APPLE_LOGIN: 'Sign in with Apple'`
  - `BLUESKY_LOGIN: 'Sign in with Bluesky'`
  - `YAHOO_LOGIN: 'Sign in with Yahoo!'`
  - `GOOGLE_LOGIN: 'Sign in with Google'`
  - `OR_LOGIN_WITH: 'Or sign in with'`
  - `TERMS_NOTICE: 'By signing in, you agree to our Terms of Service and Privacy Policy.'`
  - `SUPPORT_INFO: 'Need help? Contact support at help@tootsville.org'`

**Current Authentication Providers:**
- Google (existing)
- Yahoo! (existing)
- Apple (newly added)
- Bluesky (newly added)

### 2. CSS and HTML Modernization

**Request:** Validate all CSS and HTML for current standards compliance and compatibility with 2025 browser stacks (Chromium, Firefox, and native component browsers), remove unnecessary or outdated elements and replace with modern equivalents

**Specific Changes Made:**
- Created `react-migration/src/styles/modern-reset.css` with 2025 standards
- Removed deprecated `-khtml-user-select` vendor prefix from `www/error/500.shtml`
- Identified vendor prefixes that remain necessary for cross-browser compatibility:
  - `-webkit-appearance` and `-moz-appearance` for form elements (still needed)
  - `-webkit-scrollbar` styles for webkit browsers (still needed)
  - `-webkit-font-smoothing` and `-moz-osx-font-smoothing` for font rendering (still needed)

**Modern CSS Features Implemented:**
- Modern box-sizing reset
- Focus-visible for accessibility
- Prefers-reduced-motion media query support
- Color-scheme support for light/dark mode
- Modern scrollbar styling with scrollbar-width and scrollbar-color
- Logical properties support (margin-block, padding-inline, etc.)
- Modern viewport units (dvh, dvw)
- Container queries support structure
- Modern color functions (color-mix, color-contrast)
- Aspect-ratio property support

**HTML Standards Compliance:**
- Verified React migration `public/index.html` follows modern standards
- Main `www/index.html` contains appropriate meta tags and schema.org structured data
- No deprecated HTML elements identified in current files

## Discovered Requirements and Technical Specifications

### 3. World Locations and Transportation System

**Locations Discovered:**
- **Grand Toot Station**: Main transit hub for Tootanga's trains, accessible from any train station
- **Big Tootoona Beach**: Beach area featuring surfing, boat tours of the Bay of Elphelas, water sports, beach volleyball, and a Raving Burger location. Accessible via train at Big Tootoona Station
- **Castillo Toot**: Ancient castle on Greffle Point featuring:
  - Friendly dragon Dragón who teaches Spanish phrases
  - Medieval sports at the Jousting field
  - Horseback riding on Greffle Point cliffs from Castillo stables
  - Underground train station (Castillo Station) for return to modern civilization

**Transportation System:**
- Train network connecting all major locations
- Underground stations for seamless transitions
- Multiple access points for different activities

### 4. Input Handling System

**Requirements Discovered:**
- **Supported Input Methods:**
  - Cursor keys (Arrow keys)
  - Mouse clicks (walk-to-point)
  - Gamepad input
  - Remote control action buttons
  - Mobile touch input

- **Explicitly Unsupported:**
  - WASD keys (removed from all documentation and tests)
  - Jump functionality ("elephants don't jump")
  - Spacebar for jumping

**Implementation Status:**
- Updated `docs/PlayersGuide.html` to reflect correct input methods
- Removed WASD key tests from `react-migration/src/tests/unit/GameWorld.test.js`
- Added Arrow key tests to replace WASD tests

### 5. Communication System

**Requirements Discovered:**
- **Supported Commands:**
  - `/emote` - emote string (e.g., `/smile`, `/frown`)
  - Volume control for whispering/shouting (not slash commands)

- **Explicitly Unsupported:**
  - `/help` command (removed from documentation)
  - `/whisper` command (removed from documentation)

**Implementation Status:**
- Updated `docs/PlayersGuide.html` to remove `/help` and `/whisper`
- Documented volume control for whispering/shouting
- Added emote examples (`/smile`, `/frown`, `/wave`)

### 5. Player Statistics System

**Requirements Discovered:**
- **Player Attributes:**
  - Peanuts (currency)
  - Fairy Dust (currency)
  - Appearance details (clothing slots, skin color, pattern, pattern color)

- **Explicitly Removed:**
  - Health system
  - Energy system
  - Experience/level system
  - Coins (replaced with Peanuts)

**Implementation Status:**
- Updated `play/react/src/components/UI/PlayerStats.js` to remove health/energy/experience/level/coins
- Added peanuts, fairyDust, and appearance properties
- Appearance includes skinColor, pattern, patternColor, and clothing slots

### 6. Inventory System

**Requirements Discovered:**
- **Item Types:**
  - Clothing
  - Accessories
  - Toys
  - Food
  - Tools
  - Collectibles

- **Explicitly Removed:**
  - Weapons (non-violent MMORPG)
  - Armor (non-violent MMORPG)

**Implementation Status:**
- Updated `play/react/src/components/UI/InventoryPanel.js` to remove weapon/armor types
- Added non-violent item categories
- Updated filter dropdown to exclude weapon/armor options

### 7. Color System

**Requirements Discovered:**
- Must use canonical color names (e.g., "Violet" not "purple")
- Colors must be accessible by name returning hex values
- Global color object required for JavaScript

**Implementation Status:**
- Created `play/react/src/constants/Colors.js` with canonical color palette
- Colors include: Blue, Violet, Silver, Charcoal, DeepPurple, Yellow, Pink, Cyan, Turquoise, Periwinkle, Gold, Burgundy, Green, Lavender, Tan, Red, SpringGreen, Indigo, Orange, TranslucentBlack, TranslucentWhite, Transparent, CurrentColor
- Updated `play/react/src/components/UI/InventoryPanel.js` to use Colors constants
- Updated `play/react/src/components/UI/SettingsPanel.js` to use Colors constants

### 8. Settings and Performance

**Requirements Discovered:**
- **Performance Monitoring:**
  - Display "turtle" icon when FPS drops below 25fps
  - Do not show FPS counter
  - Resolution always auto
  - Frame rate not limited

- **Audio Settings:**
  - Separate volume controls for speech
  - Separate volume controls for voice chat (incoming)
  - Separate volume controls for voice chat (outgoing)

- **Removed Settings:**
  - Mouse sensitivity (OS handles mouse pointer)
  - Invert Y settings (OS handles mouse pointer)
  - Show FPS toggle
  - Show coordinates toggle
  - Chat opacity (always opaque)

- **UI Scale:**
  - Must document effects thoroughly
  - Performance monitor replaces resolution/frame rate settings

**Implementation Status:**
- Updated `play/react/src/components/UI/SettingsPanel.js` to reflect these requirements
- Removed unnecessary graphics/control options
- Added specific audio volume controls
- Updated UI scale options with detailed descriptions
- Added performance monitor description

### 9. Error Handling

**Requirements Discovered:**
- Client errors (uncaught exceptions) must use parrot interface
- Lisp errors must use appropriate error classes with data slots and print methods

**Implementation Status:**
- Created `play/react/src/components/UI/ParrotErrorHandler.js` for client error handling
- Updated `play/react/src/App.js` to use ParrotErrorHandler instead of generic error screen
- Implemented comprehensive error handling system in `lib/tootsville.net/src/errors.lisp` with custom error conditions
- Created error utility functions: `signal-authentication-error`, `with-error-handling`, `log-error`, `error-to-json`

### 10. Platform Support

**Requirements Discovered:**
- **New Build Targets Added:**
  - Windows 8, Windows 10
  - macOS, Linux
  - Steam
  - Alexa, Google Home
  - Apple Watch, Android Wear
  - Oculus
  - Raspberry Pi
  - Kiosk

**Implementation Status:**
- Updated `play/react/scripts/build-mobile.js` to include new platform targets
- Added corresponding build scripts to `play/react/package.json`
- Added aggregate build scripts for multiple platforms

### 11. Dependency Management

**Requirements Discovered:**
- Replace `.deps~` files with dated `.ready-YYYYMMDD` format
- Use today's date or last dependency change date

**Implementation Status:**
- Renamed `.deps~` to `.ready-20250822` in project root
- Renamed `lib/tootsville.net/.deps~` to `lib/tootsville.net/.ready-20250822`
- Updated `Makefile` and `lib/tootsville.net/Makefile` to reference new file names

### 12. Character Creation System

**Requirements Discovered:**
- Transparent signup flow for new users
- Guided by Parrot Nick and Parrot Jodie through Tootangan Wish Mist
- Multi-step process: welcome, name selection, appearance customization, account type selection, completion
- Name validation with specific rules (3-32 characters, letters/hyphens/numbers, no offensive content)
- Appearance customization with colors and patterns
- Account type selection (adult/child, sensitive player flag)
- Child code generation for child accounts

**Implementation Status:**
- Created `play/react/src/components/Auth/CharacterCreation.js` with complete multi-step flow
- Implemented name validation with comprehensive rules
- Added appearance customization with color and pattern selection
- Integrated with `/api/toots` endpoint for character creation
- Updates GameStore with new character data

### 13. Child Account System

**Requirements Discovered:**
- Comprehensive child account system with parental verification
- Verification methods: email, SMS, robo-calls
- "Waiting For Parents Mode" with curated G-rated content
- Time limits for child play sessions
- Three-symbol Secret Code for child login
- States: waiting, approved, denied, timeout, error

**Implementation Status:**
- Created `play/react/src/components/Auth/ChildAccountSystem.js` with complete verification flow
- Implements verification via `/api/child-verification/request` and `/api/child-verification/status/:requestId`
- Curated content rotation during waiting period
- Countdown timer for approved sessions
- Handles time expiration and logout

### 14. Diamond Icon System

**Requirements Discovered:**
- Black diamond (◆) for child players under 13
- White diamond (◇) for sensitive players
- Appears next to talk button or player names
- Includes "Be Nice!" warnings and "Adults Only" areas
- Prioritizes child diamond if both flags are true

**Implementation Status:**
- Created `play/react/src/components/UI/DiamondIcon.js` with configurable display
- Supports size variants (small, medium, large)
- Includes tooltip text and accessibility attributes
- Prioritizes child indicator over sensitive indicator

### 15. Game Lore and Documentation

**Requirements Discovered:**
- **Toots:** Colorful, intelligent, bipedal elephants (rhymes with "boots")
- **Basic 8:** Original elephants transformed by Tootangan Wish Mist (Zap, Flora, Sparkle, Moo, Superstar, Lil MC, Cupid, Dottie)
- **Special Characters:** 27 total including Pil (system root account, "Elephant" in ancient Hebrew), Doodle (painting elephant)
- **Katootels:** Colorful giant bird pets
- **Gossip Mice:** News/mail carriers led by Little Reminder
- **Mist Parrots:** Parrot Nick and Parrot Jodie (player membership software)
- **Vehicles:** Hang gliders, cars, trains, horses, boats, flying saucers, moon rockets, submarines, skateboards, bicycles, surfboards, wheelchairs
- **Train Lines:** South Line (Grand Toot, Looking-Glass Falls, Big Tootoona, Taft Landing, Castillo, Brunner Stations), West/North lines not yet running
- **Neighborhoods:** City Limits, Tootlantis, Big Tootoona, Comic World, Dino Land, Enchanted Forest, Moonbase T, Pirates' Cove, Toot Castle, Toot Safari
- **Address System:** Streets run E/W, Avenues run N/S, format: [House Number] [Direction] [Street/Avenue Number] [St/Ave]

**Implementation Status:**
- Updated `docs/PlayersGuide.html` with comprehensive lore sections
- Added detailed character descriptions and backstories
- Documented transportation systems and neighborhoods
- Added address system documentation with examples (1112 NW 16th St, 1612 NW 11th Ave)

### 16. Equipment vs. Wardrobe System

**Requirements Discovered:**
- **Equipment:** Items you can "use" with icon at top-left, potentially with target, counter/gauge for limited uses
- **Wardrobe:** Clothing items
- Equipment selection and usage mechanics documented

**Implementation Status:**
- Updated `docs/PlayersGuide.html` to clarify distinction
- Added "Equipment System" section with usage instructions
- Documented limited-use items and targeting mechanics

### 17. Testing Requirements

**Requirements Discovered:**
- All documented functions must be correctly implemented and wrapped in unit tests
- Thorough evaluation of edge cases, invalid inputs, out-of-range values, nils, and other error conditions
- Both frontend (React) and backend (Lisp) components require comprehensive testing

**Implementation Status:**
- Created comprehensive Lisp test suite in `lib/tootsville.net/t/` directory
- Test files include: `test-suite.lisp`, `test-users.lisp`, `test-websockets.lisp`, `test-items.lisp`, `test-terrain.lisp`, `test-world.lisp`, `test-metronome.lisp`, `test-utils.lisp`, `test-auth.lisp`
- Created test runner scripts: `run-tests.lisp` and `run-tests.sh`
- Updated frontend tests to remove WASD support and add arrow key tests
- Created tests for new components: `CharacterCreation.test.js`, `DiamondIcon.test.js`, `ChildAccountSystem.test.js`
- Encountered test environment issues with Three.js mocking and import paths

### 18. Copyright and Documentation Standards

**Requirements Discovered:**
- **Copyright Headers:**
  - Files created in 2025: `Copyright © 2025 Interworldly Adventuring, LLC`
  - Files existing before 2024: `Copyright © 2018-2024 The Corporation for Inter-World Tourism and Adventuring (CIWTA.org)`
  - Files existing before 2018: `Copyright © 2008-2017 Bruce-Robert Pocock`
  - All three may exist in same file
  - Interworldly did not exist before 2024

- **Documentation:**
  - JSDoc for JavaScript/TypeScript
  - Sphinx for Python
  - Texinfo for Lisp
  - MediaWiki for external documentation

**Implementation Status:**
- Updated copyright headers in all newly created files
- Corrected historical ownership information
- Added comprehensive JSDoc documentation to React components
- Created `docs/DevelopmentGuide.md` with technical guidelines

### 19. Package Management

**Requirements Discovered:**
- Prefer `pnpm` over `npm` for JavaScript package management
- Avoid deprecated libraries
- Update all libraries to supported versions

**Implementation Status:**
- Updated `react-migration/package.json` with latest dependency versions
- Replaced `react-query` with `@tanstack/react-query`
- Updated React to 19.1.1, React-DOM to 19.1.1
- Updated Three.js ecosystem: @react-three/fiber 9.3.0, @react-three/drei 10.7.3, three 0.179.1
- Updated testing libraries: Jest 30.0.5, @testing-library/react 16.3.0, Cypress 15.0.0
- Updated build tools: ESLint 9.33.0, Prettier 3.6.2, TypeScript 5.0.0
- Created `pnpm-workspace.yaml` and `.npmrc` for pnpm configuration
- Encountered pnpm installation issues, reverted to npm with `--legacy-peer-deps`

### 20. Internationalization (i18n)

**Requirements Discovered:**
- "Allowed for _ hour(s)" must be handled as unified expression with plural needs
- Use i18n library for pluralization

**Implementation Status:**
- Updated `react-migration/src/constants/UIText.js` to use i18n pluralization
- Changed `LOGIN.ALLOWED_FOR_HOURS` to use plural format: `'Allowed for {hours, plural, =1 {1 hour} other {# hours}}, with'`
- Removed separate `LOGIN.HOURS` key as it's now included in the pluralized string

### 21. Organization Information

**Requirements Discovered:**
- Update outdated organization information in `www/index.html`
- Current organization: "Interworldly Adventuring, LLC" as commercial organization

**Implementation Status:**
- Updated `www/index.html` schema.org data to reflect "Interworldly Adventuring, LLC"
- Changed from "Corporation for Inter-World Tourism and Adventuring" to current organization
- Updated URL to `https://interworldly.com/`

### 22. Physical Contact Information

**Requirements Discovered:**
- Remove incorrect physical mailing addresses from code files

**Implementation Status:**
- Removed physical mailing addresses from:
  - `play/Game/BallSystem.js`
  - `play/Gossip/Gossip.js`
  - `play/UI/Keys.js`
- Kept only URL references to Interworldly Adventuring, LLC

### 23. Gossip Network

**Requirements Discovered:**
- Re-enable gossip network functionality

**Implementation Status:**
- Re-enabled `Tootsville.Gossip.ensureConnected` function in `play/Gossip/Gossip.js`
- Uncommented core gossip network logic

### 24. File Purpose Clarification

**Requirements Discovered:**
- Clarify purpose of `play/react/public/index.html`

**Implementation Status:**
- Identified that `play/react/public/index.html` is for the React client in the play directory
- Different from main `play/index.html` which is for the original Babylon.js client

### 22. Communications Packet Handling

**Requirements Discovered:**
- **Robust Input Handling**: All communications packets must be handled with "strict in what they produce (send), very relaxed about what they accept (input)"
- **Client-Server Compatibility**: All documented packets must be handled on both client and server sides
- **Error Resilience**: Systems must gracefully handle malformed, missing, or unexpected packet fields
- **Backward Compatibility**: New packet versions should maintain compatibility with older clients/servers
- **Packet Validation**: Input validation without strict rejection of unknown fields
- **Graceful Degradation**: Continue operation even when receiving unexpected packet formats

**Implementation Status:**
- **Completed**: Created comprehensive `PacketHandler.js` service implementing robust packet handling
- **Completed**: Implemented strict validation for outgoing packets (commands)
- **Completed**: Implemented relaxed validation for incoming packets (gatekeeper messages)
- **Completed**: Added packet type detection and routing system
- **Completed**: Created comprehensive error handling and statistics tracking
- **Completed**: Updated `CommunicationService.js` to integrate with PacketHandler
- **Completed**: Added comprehensive unit tests for all packet handling scenarios
- **Completed**: Implemented backward compatibility for unknown fields
- **Completed**: Added event emission for packet monitoring and debugging
- **Completed**: Created schema management system for custom packet types

## Technical Issues Encountered

### 1. Test Environment Issues

**Issues:**
- Three.js mocking problems in Jest tests
- Import path issues between different React client directories
- Lisp test environment dependency loading problems
- CommunicationService constructor errors in tests

**Status:** Partially resolved, some issues remain

### 2. Dependency Resolution Issues

**Issues:**
- npm peer dependency conflicts with React 19
- pnpm installation permission errors
- Legacy peer dependency warnings

**Status:** Using npm with `--legacy-peer-deps` as workaround

### 3. Lisp Test Environment

**Issues:**
- Quicklisp not found in test environment
- ASDF loading problems
- Missing CL-DBI dependency

**Status:** Created custom test runner, some issues remain

## Current Status

### Completed Tasks
- ✅ Removed Twitter authentication, added Apple and Bluesky
- ✅ Updated CSS for modern standards compliance
- ✅ Removed WASD key support, updated to arrow keys
- ✅ Updated player statistics system (removed health/energy, added currency)
- ✅ Created canonical color system
- ✅ Updated settings panel for modern requirements
- ✅ Implemented error handling with parrot interface
- ✅ Added new platform build targets
- ✅ Updated dependency tracking system
- ✅ Created character creation system
- ✅ Created child account system
- ✅ Created diamond icon system
- ✅ Updated game lore documentation
- ✅ Updated copyright headers
- ✅ Updated organization information
- ✅ Re-enabled gossip network
- ✅ Updated i18n for pluralization
- ✅ Created comprehensive test suite structure

### In Progress
- 🔄 Fixing test environment issues
- 🔄 Resolving dependency conflicts
- 🔄 Completing Lisp test execution

### Pending
- ⏳ Final test execution and validation
- ⏳ Performance testing with turtle icon implementation
- ⏳ Mobile platform testing
- ⏳ Accessibility audit
- ⏳ Security review of authentication changes

## Notes

- All changes maintain backward compatibility where possible
- Modern standards compliance prioritized over legacy support
- Accessibility and performance considerations included in all updates
- Documentation updated to reflect all changes
- Copyright and licensing requirements strictly followed
- Non-violent game design principles maintained throughout

## File Locations

**Key Files Modified:**
- `play/Login/Login.js` - Authentication providers
- `react-migration/src/constants/UIText.js` - UI text constants
- `docs/PlayersGuide.html` - Game documentation
- `play/react/src/components/UI/PlayerStats.js` - Player statistics
- `play/react/src/components/UI/InventoryPanel.js` - Inventory system
- `play/react/src/constants/Colors.js` - Color system
- `play/react/src/components/UI/SettingsPanel.js` - Settings
- `play/react/src/components/UI/ParrotErrorHandler.js` - Error handling
- `play/react/src/components/Auth/CharacterCreation.js` - Character creation
- `play/react/src/components/Auth/ChildAccountSystem.js` - Child accounts
- `play/react/src/components/UI/DiamondIcon.js` - Diamond icons
- `lib/tootsville.net/src/errors.lisp` - Lisp error handling
- `lib/tootsville.net/t/` - Lisp test suite
- `react-migration/src/styles/modern-reset.css` - Modern CSS reset

**Configuration Files:**
- `react-migration/package.json` - Dependencies
- `play/react/package.json` - Build scripts
- `Makefile` - Build configuration
- `pnpm-workspace.yaml` - Package management
- `.npmrc` - npm configuration

## Current Status Updates (2025-01-22)

### Recently Completed
- **Complete Equipment System Implementation**: All remaining equipment items have been implemented:
  - **LightField.js**: Creates magical light field around player with pulsing effects and point lighting
  - **ShrinkPotion.js**: Makes player smaller with cyan sparkle effects and temporary duration
  - **ShrinkToHalfSize.js**: Powerful shrinking device that makes player exactly half size with spiral particle effects
  - **ZapWishBolt.js**: Epic magical device that shoots wish bolts with trail effects, impact explosions, and damage system
- **Audio Autoplay Policy Fix**: Created comprehensive `AudioAutoplayFix.js` that handles modern browser autoplay policies:
  - User interaction-based audio initialization
  - Automatic audio context management
  - Pending audio element queue system
  - Volume control integration
  - Error handling and retry mechanisms
  - Permission request UI for audio access
- **Player Character Animation System**: Created comprehensive `AnimationManager.js` with character-specific movement capabilities:
  - **Character Type Detection**: Proper detection of different character types (Toots, Manatees, Birds, etc.)
  - **Movement Restrictions**: Elephants (Toots) cannot jump, Manatees can only swim, Birds can fly and jump
  - **Important Correction**: Updated to NOT default to treating all characters as Toots - only UltraToot and similar elephant avatars are restricted
  - **Animation States**: idle, walk, run, sit, jump, swim, fly, emote, use_item, talk
  - **Smooth Transitions**: Crossfade between animations with configurable duration
  - **Movement Detection**: Velocity-based movement detection with configurable thresholds
  - **Water Detection**: Automatic swimming detection in ocean worlds or underwater
  - **State Prioritization**: sitting > jumping > item usage > movement
  - **Animation State Tracking**: Integrated with GameStore for state management
  - **Comprehensive Testing**: Unit tests for all character types and animation states
  - **Debug Indicators**: Development mode shows character capabilities (✓Jump ✗Jump ✓Swim ✗Swim ✓Fly ✗Fly)
- **Character-Specific Movement Capabilities**: Implemented proper character type detection and movement restrictions:
  - **Toots (Elephants)**: Can walk, run, sit, swim, but CANNOT jump (elephants can't jump)
  - **Manatees**: Can swim, cannot walk, run, jump, or fly
  - **Birds/Katootels**: Can fly, jump, walk, run, sit
  - **Other Characters**: Default capabilities (can jump, walk, run, sit)
  - **Avatar Type Detection**: Checks avatar name, avatarClass title/filename, and character type/species
  - **Swimming Detection**: Automatic detection in ocean worlds or when altitude < 0
  - **Comprehensive Unit Tests**: Tests for all character types and movement restrictions
- **MariaDB Testing Docker Image**: Created comprehensive `Dockerfile.mariadb-test` with full database testing environment including:
  - MariaDB server and client installation
  - Tootsville-specific database configuration
  - Complete test table schema (users, characters, items, inventory, chat, world events)
  - Database testing scripts with performance monitoring
  - Backup and restore functionality testing
  - Integration testing with Tootsville server
- **Server Version Update**: Bumped to 0.7.0 in `build/version`
- **Additional Authentication Providers**: Added GitHub and Microsoft alongside existing Apple/Bluesky
- **User Graphics Service**: Created comprehensive `UserGraphicService.js` for fetching user profile pictures from login services with Gravatar fallback
- **Equipment System Expansion**: 
  - Implemented `GrowPotion.js` with scaling effects and networking
  - Implemented `PropsAirBlaster.js` with knockdown mechanics (corrected to put players in sitting position)
  - Implemented `ShadowCaster.js` with black gummy shadow energy/tar effects
  - Implemented `GrowSlightlyLarger.js` with temporary size increase
  - Implemented `KaTootelEggShooter.js` with projectile physics and collision detection
- **Terminal Updates**: Switched from gnome-terminal to ptyxis for development runner targets

### Current Issues Being Resolved
- **Lisp Server Build Failure**: 
  - VERBOSE package loading before thread-pool-taskmaster compilation
  - JWT library replacement (cljwt-custom → jose/jwt) with restored verification
  - Thread-pool-taskmaster dependency ordering
- **Network Connectivity**: 
  - CORS errors preventing frontend-backend communication
  - WebSocket server (port 5004) not starting
  - REST API server (port 5000) not available

### In Progress
- **Dependency Resolution**: Fixing ASDF system load order for VERBOSE package
- **JWT Verification**: Restoring proper Firebase token verification using jose/jwt library
- **Server Startup**: Building new Tootsville binary after dependency fixes

### Pending Tasks
- **Testing**: Comprehensive testing of all new features
- **Version Updates**: Update version numbers in remaining configuration files
- **RPM Packaging**: Complete Docker testing environment for server nodes
- **Production Deployment**: Final testing and validation for production readiness
- **Transit System**: Implement Grand Toot Station as main transit hub for Tootanga's train network
  - **Grand Toot Station**: Main transit hub connecting all train stations in Tootanga
  - **Functionality**: Players can reach Grand Toot Station from any train station
  - **Implementation**: Requires navigation system updates and station connectivity

### Future Optimization Tasks
- **Mezzano Port**: Once Fedora deployment is stable, port to Mezzano OS for improved efficiency and reduced resource usage
  - Mezzano is a Common Lisp operating system that could provide better performance for Lisp-based services
  - Potential for reduced memory footprint and faster startup times
  - Simplified deployment with native Lisp environment
