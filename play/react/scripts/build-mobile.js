#!/usr/bin/env node
/**
 * build-mobile.js - Mobile platform build script
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Builds Tootsville client for mobile platforms including:
 * - Android (WebView, PWA, Capacitor)
 * - iOS (WebView, PWA, Capacitor)
 * - FireTV (WebView, PWA)
 * - Samsung Smart TV (WebView, PWA)
 * - LG WebOS TV (WebView, PWA)
 * - Roku TV (WebView, PWA)
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const archiver = require('archiver');

// Configuration
const CONFIG = {
  platforms: {
    android: {
      name: 'Android',
      buildDir: 'build/android',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 320, height: 568 },
        { width: 375, height: 667 },
        { width: 414, height: 736 },
        { width: 768, height: 1024 },
        { width: 1024, height: 768 }
      ]
    },
    ios: {
      name: 'iOS',
      buildDir: 'build/ios',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [20, 29, 40, 58, 60, 76, 80, 87, 120, 152, 167, 180],
      splashSizes: [
        { width: 320, height: 568 },
        { width: 375, height: 667 },
        { width: 414, height: 736 },
        { width: 768, height: 1024 },
        { width: 1024, height: 768 }
      ]
    },
    ipad: {
      name: 'iPad',
      buildDir: 'build/ipad',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [20, 29, 40, 58, 60, 76, 80, 87, 120, 152, 167, 180, 1024],
      splashSizes: [
        { width: 768, height: 1024 },
        { width: 834, height: 1112 },
        { width: 834, height: 1194 },
        { width: 1024, height: 1366 },
        { width: 1024, height: 768 }
      ]
    },
    firetv: {
      name: 'Amazon Fire TV',
      buildDir: 'build/firetv',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1280, height: 720 }
      ]
    },
    samsungtv: {
      name: 'Samsung Smart TV',
      buildDir: 'build/samsungtv',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1280, height: 720 }
      ]
    },
    lgwebos: {
      name: 'LG WebOS TV',
      buildDir: 'build/lgwebos',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1280, height: 720 }
      ]
    },
    webtv: {
      name: 'Web TV',
      buildDir: 'build/webtv',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1280, height: 720 },
        { width: 1366, height: 768 }
      ]
    },
    roku: {
      name: 'Roku TV',
      buildDir: 'build/roku',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [36, 48, 72, 96, 144, 192, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1280, height: 720 }
      ]
    },
    windows8: {
      name: 'Windows 8',
      buildDir: 'build/windows8',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256],
      splashSizes: [
        { width: 1366, height: 768 },
        { width: 1024, height: 768 },
        { width: 800, height: 600 }
      ]
    },
    windows10: {
      name: 'Windows 10',
      buildDir: 'build/windows10',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1366, height: 768 },
        { width: 1024, height: 768 }
      ]
    },
    windows11: {
      name: 'Windows 11',
      buildDir: 'build/windows11',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1366, height: 768 },
        { width: 1024, height: 768 }
      ]
    },
    symbian: {
      name: 'Symbian',
      buildDir: 'build/symbian',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [32, 48, 64, 128],
      splashSizes: [
        { width: 240, height: 320 },
        { width: 320, height: 240 },
        { width: 360, height: 640 }
      ]
    },
    // Desktop platforms that can use PWA/WebView
    macos: {
      name: 'macOS',
      buildDir: 'build/macos',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1440, height: 900 },
        { width: 1280, height: 800 }
      ]
    },
    linux: {
      name: 'Linux',
      buildDir: 'build/linux',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1366, height: 768 },
        { width: 1024, height: 768 }
      ]
    },
    // Gaming platforms
    steam: {
      name: 'Steam',
      buildDir: 'build/steam',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1366, height: 768 },
        { width: 1024, height: 768 }
      ]
    },
    // Smart home platforms
    alexa: {
      name: 'Amazon Alexa',
      buildDir: 'build/alexa',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [108, 512],
      splashSizes: [
        { width: 1024, height: 600 }
      ]
    },
    googlehome: {
      name: 'Google Home',
      buildDir: 'build/googlehome',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [108, 512],
      splashSizes: [
        { width: 1024, height: 600 }
      ]
    },
    // Wearable platforms
    applewatch: {
      name: 'Apple Watch',
      buildDir: 'build/applewatch',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [20, 29, 40, 58, 60, 76, 80, 87, 120, 152, 167, 180],
      splashSizes: [
        { width: 272, height: 340 },
        { width: 312, height: 390 }
      ]
    },
    androidwear: {
      name: 'Android Wear',
      buildDir: 'build/androidwear',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [24, 36, 48, 72, 96],
      splashSizes: [
        { width: 240, height: 240 },
        { width: 280, height: 280 }
      ]
    },
    // VR/AR platforms
    oculus: {
      name: 'Oculus Quest',
      buildDir: 'build/oculus',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1440, height: 1600 }
      ]
    },
    // Embedded platforms
    raspberrypi: {
      name: 'Raspberry Pi',
      buildDir: 'build/raspberrypi',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 1366, height: 768 },
        { width: 1024, height: 768 }
      ]
    },
    // Kiosk/Public displays
    kiosk: {
      name: 'Digital Kiosk',
      buildDir: 'build/kiosk',
      manifest: 'public/manifest.json',
      serviceWorker: 'public/sw.js',
      iconSizes: [16, 32, 48, 64, 128, 256, 512],
      splashSizes: [
        { width: 1920, height: 1080 },
        { width: 2560, height: 1440 },
        { width: 3840, height: 2160 }
      ]
    }
  }
};

/**
 * Main build function
 */
async function buildMobile() {
  const platform = process.argv[2];
  const buildType = process.argv[3] || 'pwa';
  
  if (!platform || !CONFIG.platforms[platform]) {
    console.error('Usage: node build-mobile.js <platform> [buildType]');
    console.error('Platforms:', Object.keys(CONFIG.platforms).join(', '));
    console.error('Build types: pwa, webview, capacitor');
    process.exit(1);
  }

  const config = CONFIG.platforms[platform];
  console.log(`Building for ${config.name} (${buildType})...`);

  try {
    // Clean build directory
    cleanBuildDir(config.buildDir);
    
    // Build React app
    await buildReactApp();
    
    // Generate platform-specific assets
    await generatePlatformAssets(platform, config);
    
    // Create PWA manifest and service worker
    if (buildType === 'pwa') {
      await createPWA(platform, config);
    }
    
    // Create WebView wrapper
    if (buildType === 'webview') {
      await createWebViewWrapper(platform, config);
    }
    
    // Create Capacitor project
    if (buildType === 'capacitor') {
      await createCapacitorProject(platform, config);
    }
    
    // Create deployment package
    await createDeploymentPackage(platform, config, buildType);
    
    console.log(`✅ Build completed for ${config.name}`);
    console.log(`📁 Output: ${config.buildDir}`);
    
  } catch (error) {
    console.error('❌ Build failed:', error.message);
    process.exit(1);
  }
}

/**
 * Clean build directory
 */
function cleanBuildDir(buildDir) {
  console.log('🧹 Cleaning build directory...');
  if (fs.existsSync(buildDir)) {
    fs.rmSync(buildDir, { recursive: true, force: true });
  }
  fs.mkdirSync(buildDir, { recursive: true });
}

/**
 * Build React app
 */
async function buildReactApp() {
  console.log('⚛️  Building React app...');
  execSync('npm run build', { stdio: 'inherit' });
}

/**
 * Generate platform-specific assets
 */
async function generatePlatformAssets(platform, config) {
  console.log('🎨 Generating platform assets...');
  
  const assetsDir = path.join(config.buildDir, 'assets');
  fs.mkdirSync(assetsDir, { recursive: true });
  
  // Generate icons
  await generateIcons(platform, config, assetsDir);
  
  // Generate splash screens
  await generateSplashScreens(platform, config, assetsDir);
  
  // Copy existing assets
  copyAssets(assetsDir);
}

/**
 * Generate platform-specific icons
 */
async function generateIcons(platform, config, assetsDir) {
  const iconsDir = path.join(assetsDir, 'icons');
  fs.mkdirSync(iconsDir, { recursive: true });
  
  // For now, copy existing icons and resize them
  // In production, use a proper image processing library like sharp
  const sourceIcon = path.join('public', 'favicon.ico');
  if (fs.existsSync(sourceIcon)) {
    config.iconSizes.forEach(size => {
      const iconPath = path.join(iconsDir, `icon-${size}x${size}.png`);
      // TODO: Resize icon using sharp or similar
      fs.copyFileSync(sourceIcon, iconPath);
    });
  }
}

/**
 * Generate platform-specific splash screens
 */
async function generateSplashScreens(platform, config, assetsDir) {
  const splashDir = path.join(assetsDir, 'splash');
  fs.mkdirSync(splashDir, { recursive: true });
  
  // For now, create placeholder splash screens
  // In production, use a proper image processing library
  config.splashSizes.forEach(size => {
    const splashPath = path.join(splashDir, `splash-${size.width}x${size.height}.png`);
    // TODO: Generate splash screen using sharp or similar
    createPlaceholderImage(splashPath, size.width, size.height);
  });
}

/**
 * Create placeholder image (temporary)
 */
function createPlaceholderImage(path, width, height) {
  // Create a simple SVG placeholder
  const svg = `<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
    <rect width="100%" height="100%" fill="#4a90e2"/>
    <text x="50%" y="50%" text-anchor="middle" dy=".3em" fill="white" font-size="24">
      Tootsville ${width}x${height}
    </text>
  </svg>`;
  
  fs.writeFileSync(path.replace('.png', '.svg'), svg);
}

/**
 * Copy existing assets
 */
function copyAssets(assetsDir) {
  const publicDir = 'public';
  if (fs.existsSync(publicDir)) {
    const files = fs.readdirSync(publicDir);
    files.forEach(file => {
      if (file !== 'index.html' && !file.endsWith('.json')) {
        const sourcePath = path.join(publicDir, file);
        const destPath = path.join(assetsDir, file);
        if (fs.statSync(sourcePath).isDirectory()) {
          fs.cpSync(sourcePath, destPath, { recursive: true });
        } else {
          fs.copyFileSync(sourcePath, destPath);
        }
      }
    });
  }
}

/**
 * Create PWA manifest and service worker
 */
async function createPWA(platform, config) {
  console.log('📱 Creating PWA...');
  
  // Create manifest.json
  const manifest = {
    name: 'Tootsville',
    short_name: 'Tootsville',
    description: 'A 3D virtual world for exploration and social interaction',
    start_url: '/',
    display: 'standalone',
    background_color: '#4a90e2',
    theme_color: '#4a90e2',
    orientation: platform === 'firetv' || platform === 'samsungtv' || platform === 'lgwebos' || platform === 'roku' ? 'landscape' : 'portrait',
    icons: config.iconSizes.map(size => ({
      src: `/assets/icons/icon-${size}x${size}.png`,
      sizes: `${size}x${size}`,
      type: 'image/png'
    })),
    screenshots: config.splashSizes.map(size => ({
      src: `/assets/splash/splash-${size.width}x${size.height}.png`,
      sizes: `${size.width}x${size.height}`,
      type: 'image/png',
      form_factor: size.width > size.height ? 'wide' : 'narrow'
    }))
  };
  
  fs.writeFileSync(
    path.join(config.buildDir, 'manifest.json'),
    JSON.stringify(manifest, null, 2)
  );
  
  // Create service worker
  const serviceWorker = `
// Tootsville Service Worker
// Copyright © 2025 Interworldly Adventuring, LLC.

const CACHE_NAME = 'tootsville-v1.0.0';
const urlsToCache = [
  '/',
  '/static/js/bundle.js',
  '/static/css/main.css',
  '/manifest.json'
];

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => cache.addAll(urlsToCache))
  );
});

self.addEventListener('fetch', event => {
  event.respondWith(
    caches.match(event.request)
      .then(response => response || fetch(event.request))
  );
});
  `.trim();
  
  fs.writeFileSync(
    path.join(config.buildDir, 'sw.js'),
    serviceWorker
  );
}

/**
 * Create WebView wrapper
 */
async function createWebViewWrapper(platform, config) {
  console.log('📱 Creating WebView wrapper...');
  
  const wrapperDir = path.join(config.buildDir, 'webview');
  fs.mkdirSync(wrapperDir, { recursive: true });
  
  if (platform === 'android') {
    createAndroidWebView(wrapperDir);
  } else if (platform === 'ios') {
    createIOSWebView(wrapperDir);
  } else if (platform === 'firetv') {
    createFireTVWebView(wrapperDir);
  }
}

/**
 * Create Android WebView wrapper
 */
function createAndroidWebView(wrapperDir) {
  const androidDir = path.join(wrapperDir, 'android');
  fs.mkdirSync(androidDir, { recursive: true });
  
  // Create Android project structure
  const projectStructure = {
    'app/src/main/java/com/tootsville/app/MainActivity.java': `
package com.tootsville.app;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebSettings;

public class MainActivity extends Activity {
    private WebView webView;
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        webView = new WebView(this);
        setContentView(webView);
        
        WebSettings webSettings = webView.getSettings();
        webSettings.setJavaScriptEnabled(true);
        webSettings.setDomStorageEnabled(true);
        webSettings.setAllowFileAccess(true);
        webSettings.setAllowContentAccess(true);
        webSettings.setMediaPlaybackRequiresUserGesture(false);
        
        webView.setWebViewClient(new WebViewClient());
        webView.loadUrl("file:///android_asset/index.html");
    }
    
    @Override
    public void onBackPressed() {
        if (webView.canGoBack()) {
            webView.goBack();
        } else {
            super.onBackPressed();
        }
    }
}
    `.trim(),
    
    'app/src/main/AndroidManifest.xml': `
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.tootsville.app">
    
    <uses-permission android:name="android.permission.INTERNET" />
    <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />
    <uses-permission android:name="android.permission.VIBRATE" />
    
    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:theme="@style/AppTheme">
        
        <activity
            android:name=".MainActivity"
            android:exported="true"
            android:screenOrientation="landscape"
            android:configChanges="orientation|keyboardHidden|screenSize">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>
    </application>
</manifest>
    `.trim(),
    
    'build.gradle': `
apply plugin: 'com.android.application'

android {
    compileSdkVersion 33
    defaultConfig {
        applicationId "com.tootsville.app"
        minSdkVersion 21
        targetSdkVersion 33
        versionCode 1
        versionName "1.0.0"
    }
    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    implementation 'androidx.appcompat:appcompat:1.6.1'
}
    `.trim()
  };
  
  Object.entries(projectStructure).forEach(([filePath, content]) => {
    const fullPath = path.join(androidDir, filePath);
    fs.mkdirSync(path.dirname(fullPath), { recursive: true });
    fs.writeFileSync(fullPath, content);
  });
}

/**
 * Create iOS WebView wrapper
 */
function createIOSWebView(wrapperDir) {
  const iosDir = path.join(wrapperDir, 'ios');
  fs.mkdirSync(iosDir, { recursive: true });
  
  // Create iOS project structure
  const projectStructure = {
    'TootsvilleApp/AppDelegate.swift': `
import UIKit
import WebKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        window = UIWindow(frame: UIScreen.main.bounds)
        
        let webView = WKWebView()
        let viewController = UIViewController()
        viewController.view = webView
        
        window?.rootViewController = viewController
        window?.makeKeyAndVisible()
        
        if let url = Bundle.main.url(forResource: "index", withExtension: "html") {
            webView.loadFileURL(url, allowingReadAccessTo: url.deletingLastPathComponent())
        }
        
        return true
    }
}
    `.trim(),
    
    'TootsvilleApp/Info.plist': `
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>$(DEVELOPMENT_LANGUAGE)</string>
    <key>CFBundleExecutable</key>
    <string>$(EXECUTABLE_NAME)</string>
    <key>CFBundleIdentifier</key>
    <string>$(PRODUCT_BUNDLE_IDENTIFIER)</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>$(PRODUCT_NAME)</string>
    <key>CFBundlePackageType</key>
    <string>$(PRODUCT_BUNDLE_PACKAGE_TYPE)</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>LSRequiresIPhoneOS</key>
    <true/>
    <key>UIRequiredDeviceCapabilities</key>
    <array>
        <string>armv7</string>
    </array>
    <key>UISupportedInterfaceOrientations</key>
    <array>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
    </array>
    <key>UISupportedInterfaceOrientations~ipad</key>
    <array>
        <string>UIInterfaceOrientationPortrait</string>
        <string>UIInterfaceOrientationPortraitUpsideDown</string>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
    </array>
</dict>
</plist>
    `.trim()
  };
  
  Object.entries(projectStructure).forEach(([filePath, content]) => {
    const fullPath = path.join(iosDir, filePath);
    fs.mkdirSync(path.dirname(fullPath), { recursive: true });
    fs.writeFileSync(fullPath, content);
  });
}

/**
 * Create FireTV WebView wrapper
 */
function createFireTVWebView(wrapperDir) {
  const firetvDir = path.join(wrapperDir, 'firetv');
  fs.mkdirSync(firetvDir, { recursive: true });
  
  // FireTV uses Android WebView with specific optimizations
  createAndroidWebView(firetvDir);
  
  // Add FireTV-specific manifest modifications
  const manifestPath = path.join(firetvDir, 'android/app/src/main/AndroidManifest.xml');
  if (fs.existsSync(manifestPath)) {
    let manifest = fs.readFileSync(manifestPath, 'utf8');
    manifest = manifest.replace(
      '<category android:name="android.intent.category.LAUNCHER" />',
      '<category android:name="android.intent.category.LAUNCHER" />\n                <category android:name="android.intent.category.LEANBACK_LAUNCHER" />'
    );
    fs.writeFileSync(manifestPath, manifest);
  }
}

/**
 * Create Capacitor project
 */
async function createCapacitorProject(platform, config) {
  console.log('📱 Creating Capacitor project...');
  
  const capacitorDir = path.join(config.buildDir, 'capacitor');
  fs.mkdirSync(capacitorDir, { recursive: true });
  
  // Create capacitor.config.json
  const capacitorConfig = {
    appId: 'com.tootsville.app',
    appName: 'Tootsville',
    webDir: 'build',
    bundledWebRuntime: false,
    plugins: {
      SplashScreen: {
        launchShowDuration: 3000,
        backgroundColor: '#4a90e2',
        showSpinner: true,
        spinnerColor: '#ffffff'
      },
      StatusBar: {
        style: 'dark'
      }
    }
  };
  
  fs.writeFileSync(
    path.join(capacitorDir, 'capacitor.config.json'),
    JSON.stringify(capacitorConfig, null, 2)
  );
  
  // Create package.json for Capacitor
  const packageJson = {
    name: 'tootsville-capacitor',
    version: '1.0.0',
    description: 'Tootsville Capacitor App',
    main: 'index.js',
    scripts: {
      'build': 'npm run build:web && npx cap sync',
      'build:web': 'cd .. && npm run build',
      'ios': 'npx cap open ios',
      'android': 'npx cap open android',
      'sync': 'npx cap sync'
    },
    dependencies: {
      '@capacitor/core': '^5.0.0',
      '@capacitor/ios': '^5.0.0',
      '@capacitor/android': '^5.0.0',
      '@capacitor/splash-screen': '^5.0.0',
      '@capacitor/status-bar': '^5.0.0'
    }
  };
  
  fs.writeFileSync(
    path.join(capacitorDir, 'package.json'),
    JSON.stringify(packageJson, null, 2)
  );
}

/**
 * Create deployment package
 */
async function createDeploymentPackage(platform, config, buildType) {
  console.log('📦 Creating deployment package...');
  
  const packageName = `tootsville-${platform}-${buildType}-${Date.now()}.zip`;
  const packagePath = path.join(config.buildDir, packageName);
  
  const output = fs.createWriteStream(packagePath);
  const archive = archiver('zip', { zlib: { level: 9 } });
  
  output.on('close', () => {
    console.log(`📦 Package created: ${packageName} (${archive.pointer()} bytes)`);
  });
  
  archive.on('error', (err) => {
    throw err;
  });
  
  archive.pipe(output);
  
  // Add build files
  archive.directory(config.buildDir, false);
  
  // Add deployment instructions
  const deploymentInstructions = `
# Tootsville ${config.name} Deployment Instructions

## Build Type: ${buildType.toUpperCase()}

### PWA Deployment
1. Upload all files to your web server
2. Ensure HTTPS is enabled
3. Configure service worker caching
4. Test offline functionality

### WebView Deployment
1. Follow platform-specific instructions in the webview/ directory
2. Build native app using platform SDK
3. Package and distribute through app stores

### Capacitor Deployment
1. Run 'npm install' in the capacitor/ directory
2. Run 'npm run sync' to sync web assets
3. Run 'npm run ${platform}' to open in platform IDE
4. Build and deploy through platform-specific process

## Platform-Specific Notes

${getPlatformNotes(platform)}

## Support

For technical support, contact Interworldly Adventuring, LLC at https://interworldly.com/
  `.trim();
  
  archive.append(deploymentInstructions, { name: 'DEPLOYMENT.md' });
  
  await archive.finalize();
}

/**
 * Get platform-specific deployment notes
 */
function getPlatformNotes(platform) {
  const notes = {
    android: `
- Minimum SDK: 21 (Android 5.0)
- Target SDK: 33 (Android 13)
- Requires internet permission for multiplayer features
- Optimized for landscape orientation
- Supports gamepad input
- Haptic feedback support
    `,
    ios: `
- Minimum iOS: 12.0
- Target iOS: 16.0
- Requires camera and microphone permissions for voice chat
- Optimized for landscape orientation
- Supports Apple TV remote and gamepad input
- Haptic feedback support
    `,
    firetv: `
- Fire OS 5.0 or later
- Optimized for 1080p displays
- Supports Fire TV remote and gamepad input
- Voice control integration
- Leanback launcher support
    `,
    samsungtv: `
- Tizen 3.0 or later
- Optimized for 4K displays
- Supports Samsung remote and gamepad input
- Voice control integration
- Samsung App Store distribution
    `,
    lgwebos: `
- webOS 3.0 or later
- Optimized for 4K displays
- Supports LG remote and gamepad input
- Voice control integration
- LG Content Store distribution
    `,
    webtv: `
- Optimized for 1080p displays
- Supports Roku remote and gamepad input
- Roku Channel Store distribution
    `,
    roku: `
- Roku OS 9.0 or later
- Optimized for 1080p displays
- Supports Roku remote and gamepad input
- Roku Channel Store distribution
    `
  };
  
  return notes[platform] || 'No specific notes for this platform.';
}

// Run build if called directly
if (require.main === module) {
  buildMobile().catch(console.error);
}

module.exports = { buildMobile, CONFIG };
