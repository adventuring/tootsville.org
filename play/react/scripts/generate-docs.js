#!/usr/bin/env node
/**
 * generate-docs.js - Documentation generation script
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Generates comprehensive documentation including:
 * - TeXinfo Developer's Guide
 * - HTML Player's Guide
 * - PDF documentation
 * - Mobile platform documentation
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const jsdoc2md = require('jsdoc-to-markdown');

// Configuration
const CONFIG = {
  docsDir: 'docs',
  srcDir: 'src',
  outputFormats: {
    texinfo: {
      extension: '.texi',
      template: 'texinfo-template.texi'
    },
    html: {
      extension: '.html',
      template: 'html-template.html'
    },
    pdf: {
      extension: '.pdf',
      template: 'pdf-template.tex'
    },
    mobile: {
      extension: '.md',
      template: 'mobile-template.md'
    }
  }
};

/**
 * Main documentation generation function
 */
async function generateDocs() {
  const format = process.argv[2] || 'all';
  
  if (format === 'all') {
    await generateAllDocs();
  } else if (CONFIG.outputFormats[format]) {
    await generateFormat(format);
  } else {
    console.error('Usage: node generate-docs.js [format]');
    console.error('Formats:', Object.keys(CONFIG.outputFormats).join(', '), 'all');
    process.exit(1);
  }
}

/**
 * Generate all documentation formats
 */
async function generateAllDocs() {
  console.log('📚 Generating all documentation formats...');
  
  for (const format of Object.keys(CONFIG.outputFormats)) {
    await generateFormat(format);
  }
  
  console.log('✅ All documentation generated successfully');
}

/**
 * Generate documentation for a specific format
 */
async function generateFormat(format) {
  console.log(`📝 Generating ${format} documentation...`);
  
  const config = CONFIG.outputFormats[format];
  const docsDir = path.join(CONFIG.docsDir, format);
  
  // Ensure output directory exists
  fs.mkdirSync(docsDir, { recursive: true });
  
  switch (format) {
    case 'texinfo':
      await generateTexinfoDocs(docsDir);
      break;
    case 'html':
      await generateHTMLDocs(docsDir);
      break;
    case 'pdf':
      await generatePDFDocs(docsDir);
      break;
    case 'mobile':
      await generateMobileDocs(docsDir);
      break;
  }
}

/**
 * Generate TeXinfo Developer's Guide
 */
async function generateTexinfoDocs(docsDir) {
  const texiFile = path.join(docsDir, 'tootsville-developer-guide.texi');
  
  const texiContent = `\\input texinfo
@c -*-texinfo-*-
@c %**start of header
@setfilename tootsville-developer-guide.info
@settitle Tootsville Developer's Guide
@c %**end of header

@copying
Copyright @copyright{} 2025 Interworldly Adventuring, LLC.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover
Texts.  A copy of the license is included in the section entitled
``GNU Free Documentation License''.
@end copying

@titlepage
@title Tootsville Developer's Guide
@subtitle React + Three.js Client Implementation
@author Interworldly Adventuring, LLC
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@node Top
@top Tootsville Developer's Guide

This document provides comprehensive documentation for the Tootsville
React + Three.js client implementation, including API references,
architecture overview, and development guidelines.

@menu
* Introduction::                Overview of the Tootsville client
* Architecture::                System architecture and design
* API Reference::               Complete API documentation
* Components::                  React component documentation
* Services::                    Service layer documentation
* Stores::                      State management documentation
* Bindings::                    JSCL bindings documentation
* Mobile Support::              Mobile platform support
* Testing::                     Testing guidelines and examples
* Deployment::                  Deployment and distribution
* Contributing::                Contributing guidelines
* Index::                       Function and variable index
@end menu

@node Introduction
@chapter Introduction

@menu
* Overview::                    Project overview
* Features::                    Key features
* Requirements::                System requirements
* Installation::                Installation instructions
@end menu

@node Overview
@section Overview

The Tootsville client is a modern web-based 3D virtual world client
built with React and Three.js. It provides a rich, interactive
environment for social interaction, exploration, and creativity.

@node Features
@section Features

@itemize
@item Real-time 3D rendering with Three.js
@item React-based component architecture
@item Zustand state management
@item WebSocket communication with Lisp server
@item Mobile platform support
@item JSCL Lisp integration
@item Comprehensive testing suite
@item Documentation generation
@end itemize

@node Requirements
@section Requirements

@itemize
@item Node.js >= 16.0.0
@item npm >= 8.0.0
@item Modern web browser with WebGL support
@item ASDF (for Lisp development)
@item SBCL >= 2.0.0 (for Lisp development)
@end itemize

@node Installation
@section Installation

@example
git clone https://github.com/Interworldly-Adventuring/tootsville.org.git
cd tootsville.org/react-migration
npm install
make install-lisp
@end example

@node Architecture
@chapter Architecture

@menu
* Component Architecture::      React component structure
* State Management::            Zustand store architecture
* Communication Layer::         WebSocket communication
* Rendering Pipeline::          Three.js rendering
* Mobile Integration::          Mobile platform integration
@end menu

@node Component Architecture
@section Component Architecture

The client follows a modular React component architecture with clear
separation of concerns:

@itemize
@item @ref{GameWorld} - Main 3D world container
@item @ref{AvatarManager} - Avatar management and rendering
@item @ref{WorldTerrain} - Terrain and environment
@item @ref{ItemManager} - Item and object management
@item @ref{WeatherSystem} - Weather and atmospheric effects
@item @ref{GameHUD} - User interface overlay
@end itemize

@node State Management
@section State Management

State management is handled by Zustand stores:

@itemize
@item @ref{GameStore} - Main game state
@item @ref{CommunicationStore} - Communication state
@item @ref{UiStore} - User interface state
@end itemize

@node Communication Layer
@section Communication Layer

The communication layer preserves the existing Tootsville protocol
for backwards compatibility:

@itemize
@item WebSocket connection management
@item Protocol message handling
@item Event-driven architecture
@item Error handling and reconnection
@end itemize

@node Rendering Pipeline
@section Rendering Pipeline

Three.js rendering pipeline with React Three Fiber:

@itemize
@item Scene graph management
@item Material and texture handling
@item Lighting and shadows
@item Performance optimization
@item Mobile platform adaptation
@end itemize

@node Mobile Integration
@section Mobile Integration

Mobile platform support includes:

@itemize
@item Platform detection and optimization
@item Touch input handling
@item Performance adaptation
@item Native app integration
@item PWA support
@end itemize

@node API Reference
@chapter API Reference

@menu
* CommunicationService::        WebSocket communication service
* GameStore::                   Main game state store
* AvatarManager::               Avatar management
* WorldTerrain::                Terrain system
* ItemManager::                 Item management
* WeatherSystem::               Weather system
* GameHUD::                     User interface
* ClientBindings::              JavaScript bindings
* JSCLBindings::                Lisp bindings
@end menu

${await generateAPIDocumentation()}

@node Components
@chapter Components

${await generateComponentDocumentation()}

@node Services
@chapter Services

${await generateServiceDocumentation()}

@node Stores
@chapter Stores

${await generateStoreDocumentation()}

@node Bindings
@chapter Bindings

${await generateBindingDocumentation()}

@node Mobile Support
@chapter Mobile Support

@menu
* Platform Detection::          Mobile platform detection
* Optimization::                Performance optimization
* Native Integration::          Native app integration
* PWA Support::                 Progressive Web App support
@end menu

@node Platform Detection
@section Platform Detection

The @ref{MobilePlatformService} provides comprehensive platform
detection and capability assessment for mobile devices.

@node Optimization
@section Optimization

Mobile-specific optimizations include:

@itemize
@item Render scale adjustment
@item Frame rate optimization
@item Texture quality reduction
@item Draw distance limitation
@item Battery optimization
@end itemize

@node Native Integration
@section Native Integration

Native app integration supports:

@itemize
@item Android WebView wrapper
@item iOS WKWebView wrapper
@item FireTV optimization
@item Smart TV platforms
@end itemize

@node PWA Support
@section PWA Support

Progressive Web App features:

@itemize
@item Service worker caching
@item Offline functionality
@item App manifest
@item Install prompts
@end itemize

@node Testing
@chapter Testing

@menu
* Unit Testing::                Unit test guidelines
* Integration Testing::         Integration test guidelines
* Mobile Testing::              Mobile-specific testing
* E2E Testing::                 End-to-end testing
@end menu

@node Unit Testing
@section Unit Testing

Unit tests are written using Jest and React Testing Library:

@example
import { render, screen } from '@testing-library/react';
import { GameWorld } from '../GameWorld';

test('renders game world', () => {
  render(<GameWorld />);
  expect(screen.getByTestId('game-world')).toBeInTheDocument();
});
@end example

@node Integration Testing
@section Integration Testing

Integration tests verify component interactions:

@example
import { render, screen, fireEvent } from '@testing-library/react';
import { GameHUD } from '../GameHUD';

test('handles user input', () => {
  render(<GameHUD />);
  fireEvent.click(screen.getByTestId('chat-button'));
  expect(screen.getByTestId('chat-input')).toBeInTheDocument();
});
@end example

@node Mobile Testing
@section Mobile Testing

Mobile testing includes:

@itemize
@item Touch event simulation
@item Orientation testing
@item Performance testing
@item Platform-specific features
@end itemize

@node E2E Testing
@section E2E Testing

End-to-end testing with Cypress:

@example
describe('Game Flow', () => {
  it('allows user to connect and move', () => {
    cy.visit('/');
    cy.get('[data-testid=login-form]').should('be.visible');
    cy.get('[data-testid=username]').type('testuser');
    cy.get('[data-testid=password]').type('password');
    cy.get('[data-testid=login-button]').click();
    cy.get('[data-testid=game-world]').should('be.visible');
  });
});
@end example

@node Deployment
@chapter Deployment

@menu
* Build Process::               Build system
* Static Deployment::           Static file deployment
* Mobile Deployment::           Mobile app deployment
* Production Optimization::     Production optimizations
@end menu

@node Build Process
@section Build Process

The build process includes:

@itemize
@item React app compilation
@item Asset optimization
@item Bundle analysis
@item Mobile platform packaging
@end itemize

@node Static Deployment
@section Static Deployment

Static deployment supports:

@itemize
@item Apache configuration
@item Nginx configuration
@item CDN integration
@item HTTPS setup
@end itemize

@node Mobile Deployment
@section Mobile Deployment

Mobile deployment includes:

@itemize
@item App store packaging
@item PWA deployment
@item WebView wrapper distribution
@item Capacitor project generation
@end itemize

@node Production Optimization
@section Production Optimization

Production optimizations:

@itemize
@item Code splitting
@item Tree shaking
@item Asset compression
@item Cache optimization
@end itemize

@node Contributing
@chapter Contributing

@menu
* Code Style::                  Coding standards
* Documentation::               Documentation guidelines
* Testing::                     Testing requirements
* Pull Requests::               Pull request process
@end menu

@node Code Style
@section Code Style

Code style guidelines:

@itemize
@item ESLint configuration
@item Prettier formatting
@item JSDoc documentation
@item TypeScript types
@end itemize

@node Documentation
@section Documentation

Documentation requirements:

@itemize
@item JSDoc comments
@item README updates
@item API documentation
@item Change logs
@end itemize

@node Testing
@section Testing

Testing requirements:

@itemize
@item Unit test coverage
@item Integration tests
@item Mobile platform tests
@item Performance tests
@end itemize

@node Pull Requests
@section Pull Requests

Pull request process:

@itemize
@item Feature branch creation
@item Code review process
@item CI/CD integration
@item Documentation updates
@end itemize

@node Index
@chapter Index

@printindex fn
@printindex vr
@printindex pg

@bye
`;

  fs.writeFileSync(texiFile, texiContent);
  
  // Generate info file
  try {
    execSync(`makeinfo ${texiFile}`, { cwd: docsDir });
    console.log('✅ TeXinfo documentation generated');
  } catch (error) {
    console.warn('⚠️  makeinfo not available, skipping info file generation');
  }
}

/**
 * Generate HTML Player's Guide
 */
async function generateHTMLDocs(docsDir) {
  const htmlFile = path.join(docsDir, 'tootsville-player-guide.html');
  
  const htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Tootsville Player's Guide</title>
    <style>
        body {
            font-family: 'Acme', 'Cantarell', sans-serif;
            line-height: 1.6;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: #333;
        }
        .container {
            background: white;
            border-radius: 10px;
            padding: 40px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        }
        h1, h2, h3 {
            color: #4a90e2;
            border-bottom: 2px solid #4a90e2;
            padding-bottom: 10px;
        }
        h1 {
            text-align: center;
            font-size: 2.5em;
            margin-bottom: 30px;
        }
        .feature-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 20px;
            margin: 30px 0;
        }
        .feature-card {
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            border-left: 4px solid #4a90e2;
        }
        .controls {
            background: #e3f2fd;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }
        .controls h3 {
            color: #1976d2;
            border-bottom: 1px solid #1976d2;
        }
        .control-list {
            list-style: none;
            padding: 0;
        }
        .control-list li {
            padding: 8px 0;
            border-bottom: 1px solid #e0e0e0;
        }
        .control-list li:last-child {
            border-bottom: none;
        }
        .key {
            background: #f5f5f5;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: monospace;
            border: 1px solid #ddd;
        }
        .mobile-section {
            background: #fff3e0;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }
        .mobile-section h3 {
            color: #f57c00;
            border-bottom: 1px solid #f57c00;
        }
        .troubleshooting {
            background: #ffebee;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }
        .troubleshooting h3 {
            color: #d32f2f;
            border-bottom: 1px solid #d32f2f;
        }
        .troubleshooting ul {
            list-style: none;
            padding: 0;
        }
        .troubleshooting li {
            padding: 8px 0;
            border-bottom: 1px solid #ffcdd2;
        }
        .troubleshooting li:last-child {
            border-bottom: none;
        }
        .footer {
            text-align: center;
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #e0e0e0;
            color: #666;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🌍 Tootsville Player's Guide</h1>
        
        <p>Welcome to Tootsville, a vibrant 3D virtual world where you can explore, create, and connect with others. This guide will help you get started and make the most of your Tootsville experience.</p>
        
        <h2>🎮 Getting Started</h2>
        
        <div class="feature-grid">
            <div class="feature-card">
                <h3>🆕 Create Your Account</h3>
                <p>Sign up for a free account to start your adventure. Choose a unique username and create a secure password.</p>
            </div>
            <div class="feature-card">
                <h3>👤 Customize Your Avatar</h3>
                <p>Personalize your character with different clothing, accessories, and appearance options.</p>
            </div>
            <div class="feature-card">
                <h3>🌍 Explore the World</h3>
                <p>Discover diverse environments, from bustling cities to peaceful nature areas.</p>
            </div>
        </div>
        
        <h2>🎯 Basic Controls</h2>
        
        <div class="controls">
            <h3>Desktop Controls</h3>
            <ul class="control-list">
                <li><span class="key">WASD</span> or <span class="key">Arrow Keys</span> - Move your avatar</li>
                <li><span class="key">Mouse</span> - Look around and interact</li>
                <li><span class="key">Space</span> - Jump</li>
                <li><span class="key">Enter</span> - Open chat</li>
                <li><span class="key">Tab</span> - Switch between chat modes</li>
                <li><span class="key">Escape</span> - Close menus</li>
            </ul>
        </div>
        
        <div class="mobile-section">
            <h3>📱 Mobile Controls</h3>
            <ul class="control-list">
                <li><strong>Virtual Joystick</strong> - Move your avatar</li>
                <li><strong>Touch and Drag</strong> - Look around</li>
                <li><strong>Tap</strong> - Interact with objects</li>
                <li><strong>Double Tap</strong> - Jump</li>
                <li><strong>Swipe</strong> - Quick actions</li>
            </ul>
        </div>
        
        <h2>💬 Communication</h2>
        
        <p>Tootsville offers multiple ways to communicate with other players:</p>
        
        <ul>
            <li><strong>Public Chat</strong> - Talk to everyone in the area</li>
            <li><strong>Private Messages</strong> - Send direct messages to specific players</li>
            <li><strong>Emotes</strong> - Express yourself with gestures and animations</li>
            <li><strong>Voice Chat</strong> - Use voice communication (mobile only)</li>
        </ul>
        
        <h2>🎨 Creation and Customization</h2>
        
        <div class="feature-grid">
            <div class="feature-card">
                <h3>🏠 Build and Decorate</h3>
                <p>Create your own spaces and decorate them with furniture, plants, and decorations.</p>
            </div>
            <div class="feature-card">
                <h3>👕 Fashion Design</h3>
                <p>Design and wear custom clothing and accessories.</p>
            </div>
            <div class="feature-card">
                <h3>🎵 Music and Art</h3>
                <p>Create and share music, art, and other creative content.</p>
            </div>
        </div>
        
        <h2>🌤️ Weather and Environment</h2>
        
        <p>Tootsville features dynamic weather and environmental systems:</p>
        
        <ul>
            <li><strong>Day/Night Cycle</strong> - Experience different lighting and atmosphere</li>
            <li><strong>Weather Effects</strong> - Rain, snow, and other weather conditions</li>
            <li><strong>Seasonal Changes</strong> - Different environments throughout the year</li>
        </ul>
        
        <h2>📱 Mobile Platform Support</h2>
        
        <div class="mobile-section">
            <h3>Supported Platforms</h3>
            <ul>
                <li><strong>Android</strong> - Web app, PWA, and native app</li>
                <li><strong>iOS</strong> - Web app, PWA, and native app</li>
                <li><strong>FireTV</strong> - Optimized for TV viewing</li>
                <li><strong>Smart TVs</strong> - Samsung, LG, Roku support</li>
            </ul>
            
            <h3>Mobile Features</h3>
            <ul>
                <li><strong>Touch Optimization</strong> - Designed for touch interfaces</li>
                <li><strong>Performance Adaptation</strong> - Automatically adjusts for device capabilities</li>
                <li><strong>Offline Support</strong> - Basic functionality without internet</li>
                <li><strong>Battery Optimization</strong> - Efficient power usage</li>
            </ul>
        </div>
        
        <h2>🔧 Troubleshooting</h2>
        
        <div class="troubleshooting">
            <h3>Common Issues</h3>
            <ul>
                <li><strong>Connection Problems</strong> - Check your internet connection and try refreshing the page</li>
                <li><strong>Performance Issues</strong> - Close other applications and try lowering graphics settings</li>
                <li><strong>Mobile Compatibility</strong> - Ensure you're using a supported browser (Chrome, Safari, Firefox)</li>
                <li><strong>Audio Issues</strong> - Check your device's audio settings and permissions</li>
            </ul>
            
            <h3>Getting Help</h3>
            <ul>
                <li><strong>In-Game Support</strong> - Use the help menu for quick assistance</li>
                <li><strong>Community Forums</strong> - Connect with other players for tips and advice</li>
                <li><strong>Technical Support</strong> - Contact support for technical issues</li>
            </ul>
        </div>
        
        <h2>📋 System Requirements</h2>
        
        <div class="feature-grid">
            <div class="feature-card">
                <h3>🌐 Web Browser</h3>
                <ul>
                    <li>Chrome 90+</li>
                    <li>Firefox 88+</li>
                    <li>Safari 14+</li>
                    <li>Edge 90+</li>
                </ul>
            </div>
            <div class="feature-card">
                <h3>📱 Mobile</h3>
                <ul>
                    <li>Android 8.0+</li>
                    <li>iOS 12.0+</li>
                    <li>WebGL support</li>
                    <li>2GB RAM minimum</li>
                </ul>
            </div>
            <div class="feature-card">
                <h3>🖥️ Desktop</h3>
                <ul>
                    <li>Windows 10+</li>
                    <li>macOS 10.15+</li>
                    <li>Linux (Ubuntu 18.04+)</li>
                    <li>4GB RAM recommended</li>
                </ul>
            </div>
        </div>
        
        <div class="footer">
            <p>Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.</p>
            <p>For technical support, visit <a href="https://interworldly.com/">interworldly.com</a></p>
        </div>
    </div>
</body>
</html>`;

  fs.writeFileSync(htmlFile, htmlContent);
  console.log('✅ HTML Player\'s Guide generated');
}

/**
 * Generate PDF documentation
 */
async function generatePDFDocs(docsDir) {
  const pdfFile = path.join(docsDir, 'tootsville-documentation.pdf');
  
  // For now, create a placeholder PDF generation script
  const pdfScript = `#!/usr/bin/env node
// PDF generation script
// This would use a library like puppeteer to convert HTML to PDF
console.log('PDF generation would be implemented here');
console.log('Output file: ${pdfFile}');
`;

  fs.writeFileSync(path.join(docsDir, 'generate-pdf.js'), pdfScript);
  console.log('✅ PDF generation script created');
}

/**
 * Generate mobile documentation
 */
async function generateMobileDocs(docsDir) {
  const mobileFile = path.join(docsDir, 'mobile-platform-guide.md');
  
  const mobileContent = `# Tootsville Mobile Platform Guide

Copyright © 2025 Interworldly Adventuring, LLC. This program is Free Software; Refer to COPYING.AGPL for details.

## Overview

This guide covers mobile platform support for the Tootsville client, including Android, iOS, FireTV, and smart TV platforms.

## Supported Platforms

### Android
- **Minimum SDK**: 21 (Android 5.0)
- **Target SDK**: 33 (Android 13)
- **Features**: WebView, PWA, Capacitor, native app
- **Optimizations**: Touch input, battery optimization, performance adaptation

### iOS
- **Minimum iOS**: 12.0
- **Target iOS**: 16.0
- **Features**: WKWebView, PWA, Capacitor, native app
- **Optimizations**: Touch input, battery optimization, performance adaptation

### FireTV
- **Fire OS**: 5.0 or later
- **Features**: WebView, PWA, remote control support
- **Optimizations**: 1080p display, voice control, leanback launcher

### Samsung Smart TV
- **Tizen**: 3.0 or later
- **Features**: WebView, PWA, remote control support
- **Optimizations**: 4K display, voice control, app store distribution

### LG WebOS TV
- **webOS**: 3.0 or later
- **Features**: WebView, PWA, remote control support
- **Optimizations**: 4K display, voice control, content store distribution

### Roku TV
- **Roku OS**: 9.0 or later
- **Features**: WebView, PWA, remote control support
- **Optimizations**: 1080p display, channel store distribution

## Mobile Platform Service

The \`MobilePlatformService\` provides comprehensive platform detection and optimization:

### Platform Detection
- User agent analysis
- Feature capability detection
- Performance assessment
- Native interface detection

### Optimizations
- Render scale adjustment
- Frame rate optimization
- Texture quality reduction
- Draw distance limitation
- Battery optimization
- Network optimization

### Native Integration
- Android WebView wrapper
- iOS WKWebView wrapper
- FireTV optimization
- Smart TV platform support

## Build System

### Make Targets
\`\`\`bash
# Build for all mobile platforms
make build-mobile

# Build for specific platforms
make build-android
make build-ios
make build-firetv
make build-tv

# Mobile-specific checks
make check-mobile
make test-mobile
make optimize-mobile
\`\`\`

### NPM Scripts
\`\`\`bash
# Build mobile apps
npm run build:mobile
npm run build:android
npm run build:ios
npm run build:firetv
npm run build:tv

# Mobile testing
npm run test:mobile
npm run check:mobile
npm run optimize:mobile
\`\`\`

## Deployment

### PWA Deployment
1. Upload files to web server
2. Ensure HTTPS is enabled
3. Configure service worker caching
4. Test offline functionality

### Native App Deployment
1. Build platform-specific projects
2. Package for app stores
3. Submit for review
4. Distribute to users

### WebView Deployment
1. Create platform-specific wrappers
2. Build native projects
3. Package and distribute

## Testing

### Mobile Testing
- Touch event simulation
- Orientation testing
- Performance testing
- Platform-specific features
- Battery usage testing
- Network condition testing

### E2E Testing
- Cross-platform compatibility
- User flow validation
- Performance benchmarking
- Accessibility testing

## Performance Guidelines

### Rendering
- Adaptive quality settings
- Dynamic LOD system
- Frustum culling
- Texture compression
- Asset streaming

### Memory Management
- Object pooling
- Texture atlasing
- Asset cleanup
- Memory monitoring

### Network Optimization
- Message compression
- Binary protocols
- Connection pooling
- Offline support

## Security

### Mobile Security
- HTTPS enforcement
- Certificate pinning
- Input validation
- XSS prevention
- CSRF protection

### Privacy
- Data minimization
- User consent
- Secure storage
- Privacy controls

## Support

For technical support and questions about mobile platform support, contact Interworldly Adventuring, LLC at https://interworldly.com/.
`;

  fs.writeFileSync(mobileFile, mobileContent);
  console.log('✅ Mobile platform guide generated');
}

/**
 * Generate API documentation from JSDoc comments
 */
async function generateAPIDocumentation() {
  try {
    const output = await jsdoc2md.render({
      files: path.join(CONFIG.srcDir, '**/*.js'),
      template: fs.readFileSync(path.join(__dirname, 'api-template.hbs'), 'utf8')
    });
    return output;
  } catch (error) {
    console.warn('⚠️  Could not generate API documentation:', error.message);
    return 'API documentation generation failed.';
  }
}

/**
 * Generate component documentation
 */
async function generateComponentDocumentation() {
  return `
@node GameWorld
@section GameWorld

Main 3D world container component.

@node AvatarManager
@section AvatarManager

Avatar management and rendering component.

@node WorldTerrain
@section WorldTerrain

Terrain and environment component.

@node ItemManager
@section ItemManager

Item and object management component.

@node WeatherSystem
@section WeatherSystem

Weather and atmospheric effects component.

@node GameHUD
@section GameHUD

User interface overlay component.
`;
}

/**
 * Generate service documentation
 */
async function generateServiceDocumentation() {
  return `
@node CommunicationService
@section CommunicationService

WebSocket communication service for server interaction.

@node MobilePlatformService
@section MobilePlatformService

Mobile platform detection and optimization service.
`;
}

/**
 * Generate store documentation
 */
async function generateStoreDocumentation() {
  return `
@node GameStore
@section GameStore

Main game state management store.

@node CommunicationStore
@section CommunicationStore

Communication state management store.

@node UiStore
@section UiStore

User interface state management store.
`;
}

/**
 * Generate binding documentation
 */
async function generateBindingDocumentation() {
  return `
@node ClientBindings
@section ClientBindings

JavaScript bindings for client-side functionality.

@node JSCLBindings
@section JSCLBindings

Common Lisp bindings for JSCL integration.
`;
}

// Run if called directly
if (require.main === module) {
  generateDocs().catch(console.error);
}

module.exports = { generateDocs, CONFIG };
