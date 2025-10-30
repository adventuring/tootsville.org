# TODO/FIXME/XXX Items - GitHub Issues Summary

**Date:** October 30, 2025  
**Task:** Comprehensive review and GitHub issue creation for all TODO/FIXME/XXX items in the codebase

## Overview

A complete scan of the Tootsville codebase was performed to identify all TODO, FIXME, and XXX comments. GitHub issues were created for tracking and resolution.

## Repositories Scanned

### 1. Main Frontend Repository: `adventuring/tootsville.org`
- **Repository URL:** https://github.com/adventuring/tootsville.org
- **Directories Scanned:** `play/`, `mesh/`, `www/`, `build/`, `README.org`
- **Languages:** JavaScript, Vue.js, TypeScript

### 2. Backend Repository: `adventuring/tootsville.net`
- **Repository URL:** https://github.com/adventuring/tootsville.net
- **Directories Scanned:** `src/` (all subdirectories)
- **Languages:** Common Lisp

## Results Summary

### Frontend Repository (tootsville.org)

**Total Issues Created: 51**

#### 🔴 FIXME Issues (Critical Bugs) - Issues #42-#46
**Count:** 5 issues  
**Label:** `bug`  
**Priority:** High

Files with FIXME items:
1. `play/Game/Gatekeeper.js` - Format and documentation issues in game server communication
2. `play/Scene/AvatarBuilder.js` - Irregular pattern handling (spots, patches, sparkles)
3. `play/Scene/SceneBuilder.js` - Shape building on null values
4. `play/Scene/SkyBuilder.js` - Star opacity and moon update parameters
5. `play/Scene/Tank.js` - Resize event handling issues

#### 🟡 TODO Issues (Pending Features) - Issues #47-#91
**Count:** 45 issues  
**Label:** `enhancement`  
**Priority:** Medium

Major areas requiring implementation:
- **Game Commands** (`play/Game/Commands.js`) - 40+ unimplemented command handlers
- **Game Systems** - Ball physics, gravity, growth, missile systems
- **NPC Behaviors** - Collector, Cook, Croquet Player, Doodle, Fetcher, Job Worker, Maze Builder, Sleeper, Trolley Driver, Waiter
- **Tools** - Shovel, axe, pickaxe, hammer, sewing kit, butterfly net, fishing rod
- **Navigation** - Vehicle direction, riding restrictions
- **UI Panels** - Book reader, furniture mover, HUD elements
- **Keyboard Handlers** - Multiple key binding implementations
- **Scene Building** - Vector textures, ground heightmaps, furniture updates
- **Wardrobe System** - Avatar clothing and pattern management
- **Test Fixes** - World3D component test mocking issues

#### 🟢 XXX Issues (Future Improvements) - Issue #92
**Count:** 1 consolidated issue  
**Label:** `enhancement`  
**Priority:** Low

Future enhancements include:
- GLSL shader implementation for sky backgrounds
- Texture caching for identical avatars
- Passport system restoration
- Audio control UI improvements
- Performance optimizations
- Pattern extraction from SVG files

### Backend Repository (tootsville.net)

**Total Issues Created: 35**

#### 🔴 Backend FIXME Issues - Issues #4-#11
**Count:** 8 issues  
**Label:** `bug`  
**Priority:** High  
**Component:** Backend (Common Lisp)

Critical backend bugs:
1. `src/characters/characters.lisp` - Character loading issues
2. `src/infinity/legacy-commands.lisp` - Children-present filter issues
3. `src/toots.lisp` - IGNORE-NOT-FOUND workaround for Superstar
4. `src/weather/weather.lisp` - Cloud layer generation crash
5. `src/lib/oliphaunt/edn.lisp` - EDN parsing issues
6. `src/endpoints/auth.lisp` - Authentication validation
7. `src/endpoints/infinity.lisp` - Command parameter validation
8. `src/state.lisp` - State management issues

#### 🟡 Backend TODO Issues - Issues #12-#37
**Count:** 26 issues  
**Label:** `enhancement`  
**Priority:** Medium  
**Component:** Backend (Common Lisp)

Major backend features requiring implementation:
- **Terrain System** - Underwater detection, pond creation, heightmaps
- **Weather System** - Temperature, precipitation, cloud cover calculations
- **Items System** - On-zero energy cases, wear slot sensitivity
- **Command System** - Permission errors, house/room creation, lot ownership
- **Config System** - Site name configuration
- **Characters** - Emotion handling for Robo-Toot
- **Database** - Multi-database support
- **Authentication** - JWT implementation
- **Notifications** - Gift recipient notifications
- **Messaging** - Children-present filtering
- **Error Reporting** - Enhanced client-side error reporting

#### 🟢 Backend XXX Issues - Issue #38
**Count:** 1 consolidated issue  
**Label:** `enhancement`  
**Priority:** Low  
**Component:** Backend (Common Lisp)

Backend future improvements:
- Thread pool taskmaster job queueing
- Query string parsing optimization
- Database connection improvements
- Command parameter handling
- Child request function extraction
- Speech filtering enhancements

## Statistics

| Metric | Frontend | Backend | Total |
|--------|----------|---------|-------|
| **Total Issues** | 51 | 35 | **86** |
| **FIXME (Bugs)** | 5 | 8 | **13** |
| **TODO (Features)** | 45 | 26 | **71** |
| **XXX (Future)** | 1 | 1 | **2** |

## Issue Breakdown by Priority

- **High Priority (Bugs):** 13 issues
- **Medium Priority (Features):** 71 issues
- **Low Priority (Future):** 2 issues

## Labels Applied

### Frontend Repository Labels
- `bug` - Something isn't working
- `enhancement` - New feature or request

### Backend Repository Labels
- `bug` - Something isn't working
- `enhancement` - New feature or request

## Next Steps

### Immediate Actions (High Priority)
1. Review and assign all FIXME issues (13 total)
2. Prioritize critical bugs affecting user experience
3. Address format documentation issues in game communication
4. Fix avatar pattern irregularities
5. Resolve backend authentication and validation issues

### Short-term Goals (Medium Priority)
1. Implement missing game command handlers
2. Complete NPC behavior systems
3. Finish tool implementations
4. Address terrain and weather system TODOs
5. Complete database multi-instance support

### Long-term Planning (Low Priority)
1. Performance optimization with GLSL shaders
2. Enhanced caching mechanisms
3. Code cleanup and refactoring
4. Feature enhancements from XXX comments

## Tracking Progress

All issues can be tracked through GitHub:

- **Frontend Issues:** https://github.com/adventuring/tootsville.org/issues
- **Backend Issues:** https://github.com/adventuring/tootsville.net/issues

Each issue contains:
- File path and line numbers
- Description of the TODO/FIXME/XXX comment
- Priority level
- Appropriate labels for filtering

## Notes

- The `TODO.org` file in the main repository is automatically generated via `make TODO.org`
- The Makefile includes a `no-fixmes` target that prevents production deployment when FIXME comments exist
- Issues were created programmatically using the GitHub CLI (`gh`)
- All issues are currently unassigned and available for team members to claim

---

**Generated by:** Automated codebase review  
**Review Method:** `git grep` and `grep` search for TODO/FIXME/XXX/HACK patterns  
**Issue Creation:** GitHub CLI (gh)


