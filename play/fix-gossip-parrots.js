#!/usr/bin/env node

/**
 * Fix Gossip Parrot Misusage Script
 *
 * This script identifies and fixes inappropriate usage of Gossip Parrots
 * for normal game events instead of serious networking problems.
 *
 * Gossip Parrots should only be used for:
 * - Connection problems
 * - Server errors
 * - Message of the Day
 * - Critical networking issues
 *
 * Game events should use the proper GameEventNotifications system.
 */

const fs = require('fs');
const path = require('path');

// Files that contain inappropriate Gossip Parrot usage
const filesToFix = [
  'play/Game/Equipment/ZapWishBolt.js',
  'play/Game/Equipment/ShrinkToHalfSize.js',
  'play/Game/Equipment/LightField.js',
  'play/Game/Equipment/ShrinkPotion.js',
  'play/Game/Equipment/ShadowCaster.js',
  'play/Game/Equipment/KaTootelEggShooter.js',
  'play/Game/Equipment/GrowSlightlyLarger.js',
  'play/Game/Equipment/GrowPotion.js',
  'play/Game/Equipment/PropsAirBlaster.js' // Already fixed
];

// Equipment name mappings for the notification system
const equipmentMappings = {
  'ZapWishBolt': 'zap_wish_bolt',
  'ShrinkToHalfSize': 'shrink_to_half_size',
  'LightField': 'light_field',
  'ShrinkPotion': 'shrink_potion',
  'ShadowCaster': 'shadow_caster',
  'KaTootelEggShooter': 'ka_tootel_egg_shooter',
  'GrowSlightlyLarger': 'grow_slightly_larger',
  'GrowPotion': 'grow_potion',
  'PropsAirBlaster': 'props_air_blaster'
};

// Effect type mappings
const effectMappings = {
  'shrink': 'shrink',
  'grow': 'grow',
  'stun': 'stun',
  'damage': 'damage',
  'push': 'push',
  'knockdown': 'knockdown'
};

function fixGossipParrotUsage(filePath) {
  console.log(`Processing ${filePath}...`);

  try {
    let content = fs.readFileSync(filePath, 'utf8');
    let modified = false;

    // Pattern to match Gossip Parrot calls for game events
    const gossipParrotPattern = /Tootsville\.Gossip\.Parrot\.say\(\s*["']([^"']*)["']\s*,\s*["']([^"']*(?:shrink|grow|stun|damage|push|knockdown|blast|affect)[^"']*)["']\s*\)/g;

    content = content.replace(gossipParrotPattern, (match, title, message) => {
      // Extract equipment name from file path
      const fileName = path.basename(filePath, '.js');
      const equipmentName = equipmentMappings[fileName] || fileName.toLowerCase().replace(/([A-Z])/g, '_$1').replace(/^_/, '');

      // Extract effect type from message
      let effectType = 'unknown';
      for (const [key, value] of Object.entries(effectMappings)) {
        if (message.toLowerCase().includes(key)) {
          effectType = value;
          break;
        }
      }

      // Replace with proper notification call
      const replacement = `// Send proper equipment effect notification
    if (Tootsville.Game && Tootsville.Game.EventNotifications) {
      Tootsville.Game.EventNotifications.equipmentEffect(
        '${equipmentName}',
        '${effectType}',
        targetPlayer || player,
        { id: 'system', name: '${title}' },
        \`${message.replace(/\${([^}]+)}/g, '\\${$1}')}\`
      );
    }`;

      modified = true;
      return replacement;
    });

    if (modified) {
      fs.writeFileSync(filePath, content, 'utf8');
      console.log(`✅ Fixed ${filePath}`);
    } else {
      console.log(`ℹ️  No changes needed for ${filePath}`);
    }

  } catch (error) {
    console.error(`❌ Error processing ${filePath}:`, error.message);
  }
}

function addEventNotificationsImport(filePath) {
  try {
    let content = fs.readFileSync(filePath, 'utf8');

    // Check if GameEventNotifications is already imported
    if (content.includes('GameEventNotifications')) {
      return;
    }

    // Add import near the top of the file
    const importStatement = `
// Import Game Event Notifications system
if (typeof require !== 'undefined') {
  try {
    require('../GameEventNotifications.js');
  } catch (e) {
    // Fallback for browser environment
    if (!Tootsville.Game) Tootsville.Game = {};
    if (!Tootsville.Game.EventNotifications) {
      // Load script dynamically
      const script = document.createElement('script');
      script.src = 'Game/GameEventNotifications.js';
      document.head.appendChild(script);
    }
  }
}
`;

    // Insert after existing imports or at the top
    const insertPosition = content.indexOf('/*') !== -1 ? content.indexOf('*/') + 2 : 0;
    content = content.slice(0, insertPosition) + importStatement + content.slice(insertPosition);

    fs.writeFileSync(filePath, content, 'utf8');
    console.log(`📦 Added GameEventNotifications import to ${filePath}`);

  } catch (error) {
    console.error(`❌ Error adding import to ${filePath}:`, error.message);
  }
}

function main() {
  console.log('🔧 Fixing Gossip Parrot misusage...');
  console.log('=' .repeat(50));

  filesToFix.forEach(filePath => {
    if (fs.existsSync(filePath)) {
      fixGossipParrotUsage(filePath);
      addEventNotificationsImport(filePath);
    } else {
      console.log(`⚠️  File not found: ${filePath}`);
    }
  });

  console.log('=' .repeat(50));
  console.log('✅ Gossip Parrot fix complete!');
  console.log('');
  console.log('Summary:');
  console.log('- Replaced inappropriate Gossip Parrot usage with proper GameEventNotifications');
  console.log('- Added imports for the GameEventNotifications system');
  console.log('- Maintained existing broadcast functionality');
  console.log('');
  console.log('Remember: Gossip Parrots should only be used for serious networking problems!');
}

if (require.main === module) {
  main();
}

module.exports = { fixGossipParrotUsage, addEventNotificationsImport };
