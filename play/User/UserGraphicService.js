/**
 * UserGraphicService.js - User Graphic Service
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Service for fetching user profile graphics from various login services
 * and falling back to Gravatar when not available.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

Tootsville.User.UserGraphicService = {
  
  /**
   * Cache for user graphics to avoid repeated requests
   */
  graphicCache: new Map(),
  
  /**
   * Default avatar to use when no graphic is available
   */
  defaultAvatar: '/images/default-avatar.png',
  
  /**
   * Fetch user graphic from login service or Gravatar
   * @param {Object} user - User object with authentication info
   * @param {number} size - Size of the graphic (default: 128)
   * @returns {Promise<string>} URL of the user graphic
   */
  fetchUserGraphic: async function(user, size = 128) {
    if (!user) {
      return this.defaultAvatar;
    }
    
    // Check cache first
    const cacheKey = `${user.id}_${size}`;
    if (this.graphicCache.has(cacheKey)) {
      return this.graphicCache.get(cacheKey);
    }
    
    try {
      // Try to get graphic from login service first
      const loginServiceGraphic = await this.fetchFromLoginService(user, size);
      if (loginServiceGraphic) {
        this.graphicCache.set(cacheKey, loginServiceGraphic);
        return loginServiceGraphic;
      }
      
      // Fall back to Gravatar
      const gravatarGraphic = await this.fetchFromGravatar(user, size);
      if (gravatarGraphic) {
        this.graphicCache.set(cacheKey, gravatarGraphic);
        return gravatarGraphic;
      }
      
      // Use default avatar
      this.graphicCache.set(cacheKey, this.defaultAvatar);
      return this.defaultAvatar;
      
    } catch (error) {
      console.error("UserGraphicService: Error fetching user graphic:", error);
      this.graphicCache.set(cacheKey, this.defaultAvatar);
      return this.defaultAvatar;
    }
  },
  
  /**
   * Fetch user graphic from the login service that was used
   * @param {Object} user - User object
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic or null if not available
   */
  fetchFromLoginService: async function(user, size) {
    if (!user.providerData || user.providerData.length === 0) {
      return null;
    }
    
    const provider = user.providerData[0];
    
    switch (provider.providerId) {
      case 'google.com':
        return this.fetchGoogleGraphic(provider, size);
      case 'github.com':
        return this.fetchGitHubGraphic(provider, size);
      case 'microsoft.com':
        return this.fetchMicrosoftGraphic(provider, size);
      case 'yahoo.com':
        return this.fetchYahooGraphic(provider, size);
      case 'apple.com':
        return this.fetchAppleGraphic(provider, size);
      case 'bluesky.social':
        return this.fetchBlueskyGraphic(provider, size);
      default:
        return null;
    }
  },
  
  /**
   * Fetch Google profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchGoogleGraphic: async function(provider, size) {
    if (provider.photoURL) {
      // Google provides photoURL directly
      return provider.photoURL;
    }
    
    // Try to fetch from Google People API if we have access token
    if (provider.accessToken) {
      try {
        const response = await fetch(
          `https://people.googleapis.com/v1/people/me?personFields=photos&key=${this.getGoogleAPIKey()}`,
          {
            headers: {
              'Authorization': `Bearer ${provider.accessToken}`
            }
          }
        );
        
        if (response.ok) {
          const data = await response.json();
          if (data.photos && data.photos.length > 0) {
            return data.photos[0].url;
          }
        }
      } catch (error) {
        console.warn("UserGraphicService: Could not fetch from Google People API:", error);
      }
    }
    
    return null;
  },
  
  /**
   * Fetch GitHub profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchGitHubGraphic: async function(provider, size) {
    if (provider.photoURL) {
      return provider.photoURL;
    }
    
    // Try to fetch from GitHub API if we have username
    if (provider.screenName || provider.email) {
      try {
        const username = provider.screenName || this.extractGitHubUsername(provider.email);
        if (username) {
          const response = await fetch(`https://api.github.com/users/${username}`);
          if (response.ok) {
            const data = await response.json();
            if (data.avatar_url) {
              return data.avatar_url;
            }
          }
        }
      } catch (error) {
        console.warn("UserGraphicService: Could not fetch from GitHub API:", error);
      }
    }
    
    return null;
  },
  
  /**
   * Fetch Microsoft profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchMicrosoftGraphic: async function(provider, size) {
    if (provider.photoURL) {
      return provider.photoURL;
    }
    
    // Try to fetch from Microsoft Graph API if we have access token
    if (provider.accessToken) {
      try {
        const response = await fetch(
          'https://graph.microsoft.com/v1.0/me/photo/$value',
          {
            headers: {
              'Authorization': `Bearer ${provider.accessToken}`
            }
          }
        );
        
        if (response.ok) {
          // Convert blob to data URL
          const blob = await response.blob();
          return URL.createObjectURL(blob);
        }
      } catch (error) {
        console.warn("UserGraphicService: Could not fetch from Microsoft Graph API:", error);
      }
    }
    
    return null;
  },
  
  /**
   * Fetch Yahoo profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchYahooGraphic: async function(provider, size) {
    if (provider.photoURL) {
      return provider.photoURL;
    }
    
    // Yahoo doesn't provide easy API access, rely on photoURL
    return null;
  },
  
  /**
   * Fetch Apple profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchAppleGraphic: async function(provider, size) {
    if (provider.photoURL) {
      return provider.photoURL;
    }
    
    // Apple doesn't provide easy API access, rely on photoURL
    return null;
  },
  
  /**
   * Fetch Bluesky profile picture
   * @param {Object} provider - Provider data
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchBlueskyGraphic: async function(provider, size) {
    if (provider.photoURL) {
      return provider.photoURL;
    }
    
    // Try to fetch from Bluesky API if we have username
    if (provider.screenName) {
      try {
        const response = await fetch(
          `https://bsky.social/xrpc/app.bsky.actor.getProfile?actor=${provider.screenName}`
        );
        
        if (response.ok) {
          const data = await response.json();
          if (data.data && data.data.avatar) {
            return data.data.avatar;
          }
        }
      } catch (error) {
        console.warn("UserGraphicService: Could not fetch from Bluesky API:", error);
      }
    }
    
    return null;
  },
  
  /**
   * Fetch user graphic from Gravatar
   * @param {Object} user - User object
   * @param {number} size - Size of the graphic
   * @returns {Promise<string|null>} URL of the graphic
   */
  fetchFromGravatar: async function(user, size) {
    if (!user.email) {
      return null;
    }
    
    try {
      // Create MD5 hash of email (lowercase, trimmed)
      const email = user.email.toLowerCase().trim();
      const hash = await this.md5Hash(email);
      
      // Construct Gravatar URL
      const gravatarUrl = `https://www.gravatar.com/avatar/${hash}?s=${size}&d=404`;
      
      // Check if Gravatar exists
      const response = await fetch(gravatarUrl, { method: 'HEAD' });
      if (response.ok) {
        return gravatarUrl;
      }
      
      return null;
      
    } catch (error) {
      console.warn("UserGraphicService: Could not fetch from Gravatar:", error);
      return null;
    }
  },
  
  /**
   * Create MD5 hash of string (for Gravatar)
   * @param {string} str - String to hash
   * @returns {Promise<string>} MD5 hash
   */
  md5Hash: async function(str) {
    // Use Web Crypto API if available
    if (window.crypto && window.crypto.subtle) {
      const encoder = new TextEncoder();
      const data = encoder.encode(str);
      const hashBuffer = await window.crypto.subtle.digest('MD5', data);
      const hashArray = Array.from(new Uint8Array(hashBuffer));
      return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
    }
    
    // Fallback to simple hash (not cryptographically secure, but works for Gravatar)
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return Math.abs(hash).toString(16);
  },
  
  /**
   * Extract GitHub username from email (if it's a GitHub email)
   * @param {string} email - Email address
   * @returns {string|null} GitHub username or null
   */
  extractGitHubUsername: function(email) {
    // GitHub emails are often in format: username@users.noreply.github.com
    if (email.includes('@users.noreply.github.com')) {
      return email.split('@')[0];
    }
    return null;
  },
  
  /**
   * Get Google API key from configuration
   * @returns {string} Google API key
   */
  getGoogleAPIKey: function() {
    // This should be configured in your Firebase config or environment
    return window.TootsvilleConfig?.googleAPIKey || '';
  },
  
  /**
   * Clear the graphic cache
   */
  clearCache: function() {
    this.graphicCache.clear();
  },
  
  /**
   * Remove a specific user's graphics from cache
   * @param {string} userId - User ID to remove from cache
   */
  removeFromCache: function(userId) {
    for (const [key] of this.graphicCache) {
      if (key.startsWith(`${userId}_`)) {
        this.graphicCache.delete(key);
      }
    }
  },
  
  /**
   * Preload user graphics for better performance
   * @param {Array} users - Array of user objects
   * @param {number} size - Size of the graphics
   */
  preloadGraphics: async function(users, size = 128) {
    const promises = users.map(user => this.fetchUserGraphic(user, size));
    await Promise.allSettled(promises);
  }
};
