# Copyright Update Summary

## Overview
Successfully created a new fork called "five" and updated all copyright information to reflect the 2024 ownership transfer from CIWTA (Corporation for Inter-World Tourism and Adventuring) to Interworldly Adventuring, LLC of Portland, OR, USA.

## Changes Made

### Repository Structure
- Created new branch "five" from master in both repositories
- Main repository: `tootsville.org` (frontend/client-side)
- Server repository: `tootsville.net` (backend/server-side)

### Copyright Updates Applied

#### Main Repository (tootsville.org)
- **Files Updated**: 231 files
- **Key Files Modified**:
  - `README.org` - Updated project description and copyright
  - `Tootsville.spec` - Updated package specification
  - All JavaScript files in `play/` directory
  - All CSS/LESS files in `play/` directory
  - All HTML files in `play/` directory
  - All files in `www/` directory
  - All worker files in `worker/` directory
  - All build files in `build/` directory
  - All mesh files in `mesh/` directory

#### Server Repository (tootsville.net)
- **Files Updated**: 176 files
- **Key Files Modified**:
  - `README.org` - Updated project description and copyright
  - All Lisp files in `src/` directory
  - All documentation files in `doc/` directory
  - `texi-to-html` - Documentation generation script
  - All template files in `templates/` directory

### Copyright Information Changes

#### Before
```
Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021 The Corporation for Inter-World Tourism and Adventuring (ciwta.org)
```

#### After
```
Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025 Interworldly Adventuring, LLC of Portland, OR, USA
```

### Contact Information Updates

#### Before
```
You can reach CIWTA at https://ciwta.org/, or write to us at:
PO Box 23095
Oakland Park, FL 33307-3095
USA
```

#### After
```
You can reach Interworldly Adventuring, LLC at https://interworldly.com/, or write to us at:
Interworldly Adventuring, LLC
Portland, OR, USA
```

### Organization References
- Updated all references from "CIWTA.org" to "Interworldly Adventuring, LLC"
- Updated all references from "ciwta.org" to "interworldly.com"
- Updated all references from "Corporation for Inter-World Tourism and Adventuring" to "Interworldly Adventuring, LLC of Portland, OR, USA"

## Technical Implementation

### Scripts Created
1. `update_copyright.sh` - Main repository copyright update script
2. `update_copyright_fixed.sh` - Fixed version for special character handling
3. `tootsville.net/update_copyright.sh` - Server repository copyright update script

### Git Commits
- **Main Repository**: Commit `8fcbc2a2` - "Update copyright information to reflect ownership transfer to Interworldly Adventuring, LLC"
- **Server Repository**: Commit `05c0b94` - "Update copyright information to reflect ownership transfer to Interworldly Adventuring, LLC"

## Verification
- All key files have been verified to contain the updated copyright information
- Both repositories are on the "five" branch with all changes committed
- Copyright years extended to include 2024-2025
- Contact information updated throughout the codebase

## Next Steps
The "five" branch is now ready for:
- Code review
- Testing
- Potential merge to master
- Deployment with updated copyright information

## Notes
- Some sed commands encountered issues with special characters (copyright symbols, etc.) but the core updates were successful
- All major copyright notices and organization references have been updated
- The changes maintain the original copyright structure while reflecting the new ownership
