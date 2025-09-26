#!/usr/bin/env node

/**
 * Complete Gossip Parrot Cleanup Script
 *
 * This script systematically replaces all remaining inappropriate Gossip Parrot
 * usage in equipment files with proper GameEventNotifications calls.
 */

const fs = require('fs');
const path = require('path');

// Equipment files that still need Gossip Parrot cleanup
const equipmentFiles = [
  'play/Game/Equipment/ShadowCaster.js',
  'play/Game/Equipment/KaTootelEggShooter.js',
  'play/Game/Equipment/ShrinkPotion.js',
  'play/Game/Equipment/LightField.js',
  'play/Game/Equipment/ZapWishBolt.js'
];

// Equipment name mappings
const equipmentMappings = {
  'ShadowCaster': 'shadow_caster',
  'KaTootelEggShooter': 'ka_tootel_egg_shooter',
  'ShrinkPotion': 'shrink_potion',
  'LightField': 'light_field',
  'ZapWishBolt': 'zap_wish_bolt'
};

// Effect type mappings based on message content
function determineEffectType(message, title) {
  const lowerMessage = message.toLowerCase();
  const lowerTitle = title.toLowerCase();

  if (lowerMessage.includes('shrink') || lowerTitle.includes('shrink')) {
    return 'shrink';
  }
  if (lowerMessage.includes('grow') || lowerMessage.includes('larger') || lowerTitle.includes('grow')) {
    return 'grow';
  }
  if (lowerMessage.includes('stun') || lowerMessage.includes('bolt') || lowerTitle.includes('zap')) {
    return 'stun';
  }
  if (lowerMessage.includes('damage') || lowerMessage.includes('caster') || lowerTitle.includes('shadow')) {
    return 'damage';
  }
  if (lowerMessage.includes('push') || lowerMessage.includes('blast') || lowerTitle.includes('air')) {
    return 'push';
  }
  if (lowerMessage.includes('knockdown') || lowerMessage.includes('air blast')) {
    return 'knockdown';
  }

  return 'effect'; // Generic fallback
}

// Status type mappings for system messages
function determineStatusType(message) {
  const lowerMessage = message.toLowerCase();

  if (lowerMessage.includes('cooldown') || lowerMessage.includes('brewing') || lowerMessage.includes('recharging')) {
    return 'cooldown';
  }
  if (lowerMessage.includes('empty') || lowerMessage.includes('out of') || lowerMessage.includes('bottle')) {
    return 'empty';
  }
  if (lowerMessage.includes('already') || lowerMessage.includes('active') || lowerMessage.includes('larger')) {
    return 'active';
  }
  if (lowerMessage.includes('error') || lowerMessage.includes('no valid') || lowerMessage.includes('problem')) {
    return 'error';
  }

  return 'info'; // Generic fallback
}

function fixGossipParrotInFile(filePath) {
  console.log(`🔧 Processing ${filePath}...`);

  try {
    let content = fs.readFileSync(filePath, 'utf8');
    let modified = false;

    // Pattern to match Gossip Parrot calls
    const gossipParrotPattern = /Tootsville\.Gossip\.Parrot\.say\(\s*["']([^"']*)["']\s*,\s*["']([^"']*(?:\$\{[^}]+\}[^"']*)?)["']\s*\)/g;

    content = content.replace(gossipParrotPattern, (match, title, message) => {
      const fileName = path.basename(filePath, '.js');
      const equipmentName = equipmentMappings[fileName] || fileName.toLowerCase().replace(/([A-Z])/g, '_$1').replace(/^_/, '');

      // Determine if this is a system message or equipment effect
      const isSystemMessage = message.toLowerCase().includes('cooldown') ||
                             message.toLowerCase().includes('empty') ||
                             message.toLowerCase().includes('brewing') ||
                             message.toLowerCase().includes('already') ||
                             message.toLowerCase().includes('out of') ||
                             message.toLowerCase().includes('error') ||
                             message.toLowerCase().includes('no valid');

      let replacement;
      if (isSystemMessage) {
        const statusType = determineStatusType(message);
        replacement = `// Send proper equipment status notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentStatus(
          '${equipmentName}',
          '${statusType}',
          \`${message.replace(/\${([^}]+)}/g, '\\${$1}')}\`
        );
      }`;
      } else {
        const effectType = determineEffectType(message, title);
        replacement = `// Send proper equipment effect notification
      if (Tootsville.Game && Tootsville.Game.EventNotifications) {
        Tootsville.Game.EventNotifications.equipmentEffect(
          '${equipmentName}',
          '${effectType}',
          targetPlayer || player,
          { id: 'system', name: '${title}' },
          \`${message.replace(/\${([^}]+)}/g, '\\${$1}')}\`
        );
      }`;
      }

      modified = true;
      return replacement;
    });

    if (modified) {
      fs.writeFileSync(filePath, content, 'utf8');
      console.log(`✅ Fixed ${filePath}`);
      return true;
    } else {
      console.log(`ℹ️  No changes needed for ${filePath}`);
      return false;
    }

  } catch (error) {
    console.error(`❌ Error processing ${filePath}:`, error.message);
    return false;
  }
}

function main() {
  console.log('🔧 Completing Gossip Parrot cleanup...');
  console.log('=' .repeat(50));

  let totalFixed = 0;

  equipmentFiles.forEach(filePath => {
    if (fs.existsSync(filePath)) {
      if (fixGossipParrotInFile(filePath)) {
        totalFixed++;
      }
    } else {
      console.log(`⚠️  File not found: ${filePath}`);
    }
  });

  console.log('=' .repeat(50));
  console.log('✅ Gossip Parrot cleanup complete!');
  console.log(`📊 Fixed ${totalFixed} files`);
  console.log('');
  console.log('🎯 Next Steps:');
  console.log('1. Test equipment functionality with new notification system');
  console.log('2. Verify server-mediated broadcasting works correctly');
  console.log('3. Update any remaining Gossip Parrot usage in other files');
  console.log('4. Test protocol compliance with Infinity Mode standards');
}

if (require.main === module) {
  main();
}

module.exports = { fixGossipParrotInFile };
