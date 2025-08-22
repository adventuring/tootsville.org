#!/usr/bin/env node

/**
 * build-static.js - Static build script for Tootsville React client
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

/**
 * @function buildStaticClient
 * @description Build the React client as static files only
 * 
 * This script ensures that the Tootsville client builds as completely
 * static files that can be served over HTTPS without any additional
 * runtime server dependencies beyond the tootsville.net Lisp server.
 */
function buildStaticClient() {
  console.log('Building Tootsville React client as static files...');
  
  try {
    // Clean previous build
    if (fs.existsSync('build')) {
      fs.rmSync('build', { recursive: true, force: true });
      console.log('✓ Cleaned previous build');
    }

    // Set environment variables for static build
    process.env.GENERATE_SOURCEMAP = 'false';
    process.env.INLINE_RUNTIME_CHUNK = 'false';
    process.env.SKIP_PREFLIGHT_CHECK = 'true';

    // Build the React application
    console.log('✓ Building React application...');
    execSync('npx react-scripts build', { 
      stdio: 'inherit',
      env: { ...process.env }
    });

    // Verify build output
    const buildPath = path.join(__dirname, '..', 'build');
    if (!fs.existsSync(buildPath)) {
      throw new Error('Build directory not created');
    }

    // Check for static files
    const staticFiles = [
      'index.html',
      'static/js',
      'static/css',
      'static/media'
    ];

    for (const file of staticFiles) {
      const filePath = path.join(buildPath, file);
      if (!fs.existsSync(filePath)) {
        throw new Error(`Required static file missing: ${file}`);
      }
    }

    // Create deployment manifest
    const manifest = {
      name: 'Tootsville React Client',
      version: '1.0.0',
      description: 'Static build of Tootsville React client',
      buildTime: new Date().toISOString(),
      staticFiles: true,
      serverDependencies: ['tootsville.net'],
      deployment: {
        type: 'static',
        server: 'HTTPS static file server',
        websocket: 'tootsville.net Lisp server'
      }
    };

    fs.writeFileSync(
      path.join(buildPath, 'deployment.json'),
      JSON.stringify(manifest, null, 2)
    );

    // Create .htaccess for Apache (if needed)
    const htaccess = `
# Tootsville React Client - Apache Configuration
# Serve static files and handle client-side routing

RewriteEngine On

# Handle client-side routing
RewriteCond %{REQUEST_FILENAME} !-f
RewriteCond %{REQUEST_FILENAME} !-d
RewriteRule ^(.*)$ /index.html [QSA,L]

# Security headers
Header always set X-Content-Type-Options nosniff
Header always set X-Frame-Options DENY
Header always set X-XSS-Protection "1; mode=block"

# Cache static assets
<FilesMatch "\\.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$">
  ExpiresActive On
  ExpiresDefault "access plus 1 year"
  Header set Cache-Control "public, immutable"
</FilesMatch>

# No cache for HTML files
<FilesMatch "\\.html$">
  Header set Cache-Control "no-cache, no-store, must-revalidate"
  Header set Pragma "no-cache"
  Header set Expires "0"
</FilesMatch>
`;

    fs.writeFileSync(path.join(buildPath, '.htaccess'), htaccess.trim());

    // Create nginx.conf for Nginx (if needed)
    const nginxConf = `
# Tootsville React Client - Nginx Configuration
server {
    listen 80;
    listen [::]:80;
    server_name _;
    
    root /var/www/tootsville-client;
    index index.html;
    
    # Security headers
    add_header X-Content-Type-Options nosniff always;
    add_header X-Frame-Options DENY always;
    add_header X-XSS-Protection "1; mode=block" always;
    
    # Handle client-side routing
    location / {
        try_files $uri $uri/ /index.html;
    }
    
    # Cache static assets
    location ~* \\.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
    
    # No cache for HTML files
    location ~* \\.html$ {
        add_header Cache-Control "no-cache, no-store, must-revalidate";
        add_header Pragma "no-cache";
        add_header Expires "0";
    }
    
    # WebSocket proxy to tootsville.net server
    location /websocket {
        proxy_pass http://localhost:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
`;

    fs.writeFileSync(path.join(buildPath, 'nginx.conf'), nginxConf.trim());

    console.log('✓ Build completed successfully!');
    console.log('✓ Static files ready for deployment');
    console.log('✓ No runtime server dependencies required');
    console.log('');
    console.log('Deployment files created:');
    console.log('  - build/index.html (main entry point)');
    console.log('  - build/static/ (compiled assets)');
    console.log('  - build/.htaccess (Apache configuration)');
    console.log('  - build/nginx.conf (Nginx configuration)');
    console.log('  - build/deployment.json (deployment manifest)');
    console.log('');
    console.log('To deploy:');
    console.log('  1. Copy build/ contents to your web server');
    console.log('  2. Ensure tootsville.net server is running on port 5000');
    console.log('  3. Configure WebSocket proxy if needed');
    console.log('  4. Serve over HTTPS for production');

  } catch (error) {
    console.error('✗ Build failed:', error.message);
    process.exit(1);
  }
}

// Run build if called directly
if (require.main === module) {
  buildStaticClient();
}

module.exports = { buildStaticClient };
