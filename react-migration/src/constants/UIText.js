/**
 * UIText.js - UI text constants from existing Tootsville client
 * Preserves all existing UI text to maintain consistency during React migration
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

/**
 * @constant UIText
 * @description All UI text constants extracted from existing play/ directory
 * 
 * This object contains all the UI text strings used throughout the application,
 * ensuring that the React migration preserves the exact same text as the original
 * Babylon.js client.
 */
export const UIText = {
  // Login and Authentication
  LOGIN: {
    LOADING: 'Loading…',
    PICK_TOOT_TITLE: 'Pick a Toot Character',
    PICK_TOOT_DESCRIPTION: "Click or tap a character's name to play now.",
    EDIT_TOOT_TITLE: 'Edit Toot Characters',
    EDIT_TOOT_DESCRIPTION: 'Set up Child options here. Set a 6-to-12-letter code for child logins (like a password). <A TARGET="_blank"  HREF="https://wiki.tootsville.org/wiki/Child_Sign-in_Code">More info…</A>',
    CHILD_ACCOUNT: 'Child Account',
    ALLOWED_FOR_HOURS: 'Allowed for {hours, plural, =1 {1 hour} other {# hours}}, with',
    REQUESTED_TO_PLAY: 'Requested to play.',
    RESTARTING_LOGIN: 'Restarting login…',
    START_LOGIN: 'Start login (build crypto keys)',
    OR_LOGIN_WITH: 'Or sign in with',
    GOOGLE_LOGIN: 'Sign in with Google',
    GITHUB_LOGIN: 'Sign in with GitHub',
    MICROSOFT_LOGIN: 'Sign in with Microsoft',
    YAHOO_LOGIN: 'Sign in with Yahoo!',
    APPLE_LOGIN: 'Sign in with Apple',
    BLUESKY_LOGIN: 'Sign in with Bluesky',
    TERMS_NOTICE: 'By signing in, you agree to our Terms of Service and Privacy Policy.',
    SUPPORT_INFO: 'Need help? Contact support at help@tootsville.org'
  },

  // Signup and Registration
  SIGNUP: {
    TERMS_NOTICE: 'By creating an account, you agree to our Terms of Service and Privacy Policy.',
    CREATE_ACCOUNT: 'Create Account',
    ALREADY_HAVE_ACCOUNT: 'Already have an account? Sign in',
    PASSWORD_REQUIREMENTS: 'Password must be at least 8 characters long',
    EMAIL_REQUIRED: 'Email is required for account recovery',
    CONFIRM_PASSWORD: 'Confirm Password',
    PASSWORDS_MUST_MATCH: 'Passwords must match'
  },

  // Loading and Status
  LOADING: {
    SPINNER: '<i class="fa fa-spinner fa-spin fa-5x"></i>',
    LIFE_RING: '<i class="fa fa-life-ring fa-spin fa-5x"></i>'
  },

  // Wallet and Currency
  WALLET: {
    PEANUTS: 'Peanuts',
    FAIRY_DUST: 'Fairy Dust',
    INVENTORY: 'Inventory'
  },

  // Game Interface
  GAME: {
    CONNECTED: 'Connected',
    DISCONNECTED: 'Disconnected',
    CONNECTION_ERROR: 'Connection Error',
    RETRY_CONNECTION: 'Retry Connection',
    PLAY_NOW: 'Play Now',
    PLAYTEST_NOW: 'Playtest Now',
    DO_QA_NOW: 'Do QA Now',
    NEWS_BLOGS: 'News & Blogs',
    LEARN_MORE: 'Learn More',
    TOOTSBOOK_FEED: 'Tootsbook has the latest news…'
  },

  // Error Messages
  ERROR: {
    CONNECTION_FAILED: 'Failed to initialize application',
    UNEXPECTED_ERROR: 'An unexpected error occurred',
    INVALID_MESSAGE: 'Invalid message format',
    CONNECTION_ERROR: 'Connection error',
    FAILED_TO_ESTABLISH: 'Failed to establish connection'
  },

  // Debug and Development
  DEBUG: {
    DEBUG_INFORMATION: 'Debug Information',
    CONNECTION_STATUS: 'Connection:',
    CLUSTER: 'Cluster:',
    RECONNECT_ATTEMPTS: 'Reconnect Attempts:',
    DISCONNECT: 'Disconnect',
    RECONNECT: 'Reconnect'
  },

  // Character and Avatar
  CHARACTER: {
    AVATAR_IMAGE: '<IMG HEIGHT=256 WIDTH=256>',
    CHILD_ACCOUNT_ICON: '<I CLASS="fas fa-child fa-fw"></I>'
  },

  // Time and Clock
  TIME: {
    TOOTSVILLE_TIME: 'Tootsville Time',
    TOOTSVILLE_DATE: 'Tootsville Date'
  },

  // Footer and Legal
  FOOTER: {
    TERMS_OF_SERVICE: 'Terms of Service',
    PRIVACY: 'Privacy',
    PRIVACY_DESCRIPTION: 'We protect your Privacy in meaningful ways.',
    USE_SUBJECT_TO: 'Use is subject to our'
  }
};

/**
 * @function getUIText
 * @description Get UI text by category and key
 * @param {string} category - Text category (e.g., 'LOGIN', 'GAME')
 * @param {string} key - Text key within category
 * @returns {string} The UI text string
 * 
 * @example
 * const loadingText = getUIText('LOADING', 'SPINNER');
 * const loginTitle = getUIText('LOGIN', 'PICK_TOOT_TITLE');
 */
export const getUIText = (category, key) => {
  if (!UIText[category]) {
    console.warn(`UI text category '${category}' not found`);
    return '';
  }
  
  if (!UIText[category][key]) {
    console.warn(`UI text key '${key}' not found in category '${category}'`);
    return '';
  }
  
  return UIText[category][key];
};

/**
 * @function getLoginText
 * @description Get login-related UI text
 * @param {string} key - Login text key
 * @returns {string} Login UI text
 */
export const getLoginText = (key) => getUIText('LOGIN', key);

/**
 * @function getGameText
 * @description Get game-related UI text
 * @param {string} key - Game text key
 * @returns {string} Game UI text
 */
export const getGameText = (key) => getUIText('GAME', key);

/**
 * @function getErrorText
 * @description Get error-related UI text
 * @param {string} key - Error text key
 * @returns {string} Error UI text
 */
export const getErrorText = (key) => getUIText('ERROR', key);

export default UIText;



