/**
 * User Store - Vue Composable
 *
 * Equivalent to original ActionScript UserManager
 * Manages user authentication, profile, and session data
 */

import { ref, computed, reactive, readonly } from 'vue'
import { useAMF, APIEndpoint } from '../InfinityMode/AMFManager'
import { useEventSystem, EventFactory, EventType } from '../InfinityMode/EventSystem'

// User data interfaces matching original ActionScript structures
export interface UserProfile {
  id: string
  username: string
  email: string
  displayName: string
  avatarId?: string
  isModerator: boolean
  isAdmin: boolean
  createdAt: string
  lastLoginAt?: string
  preferences: UserPreferences
  statistics: UserStatistics
}

export interface UserPreferences {
  soundEnabled: boolean
  musicEnabled: boolean
  chatEnabled: boolean
  friendRequestsEnabled: boolean
  language: string
  timezone: string
  theme: 'light' | 'dark' | 'auto'
}

export interface UserStatistics {
  totalPlayTime: number
  gamesPlayed: number
  achievements: number
  friendsCount: number
  level: number
  experience: number
  peanuts: number
  lastUpdated: string
}

export interface LoginCredentials {
  username: string
  password: string
  rememberMe?: boolean
}

export interface RegistrationData {
  username: string
  email: string
  password: string
  confirmPassword: string
  parentEmail?: string
  acceptTerms: boolean
}

// Reactive state
const isAuthenticated = ref(false)
const isLoading = ref(false)
const currentUser = ref<UserProfile | null>(null)
const sessionToken = ref<string | null>(null)
const loginError = ref<string | null>(null)

// Preferences with defaults
const defaultPreferences: UserPreferences = {
  soundEnabled: true,
  musicEnabled: true,
  chatEnabled: true,
  friendRequestsEnabled: true,
  language: 'en',
  timezone: 'UTC',
  theme: 'auto'
}

// Statistics defaults
const defaultStatistics: UserStatistics = {
  totalPlayTime: 0,
  gamesPlayed: 0,
  achievements: 0,
  friendsCount: 0,
  level: 1,
  experience: 0,
  peanuts: 0,
  lastUpdated: new Date().toISOString()
}

// Event system integration
const { emit } = useEventSystem()

/**
 * User Store Composable
 *
 * Provides reactive user management with authentication, profile updates, and session handling
 */
export function useUserStore() {
  const amf = useAMF()

  // Computed properties
  const userId = computed(() => currentUser.value?.id || null)
  const username = computed(() => currentUser.value?.username || null)
  const displayName = computed(() => currentUser.value?.displayName || currentUser.value?.username || null)
  const isModerator = computed(() => currentUser.value?.isModerator || false)
  const isAdmin = computed(() => currentUser.value?.isAdmin || false)
  const userLevel = computed(() => currentUser.value?.statistics?.level || 1)
  const userPeanuts = computed(() => currentUser.value?.statistics?.peanuts || 0)

  // Readonly reactive state
  const state = readonly({
    isAuthenticated,
    isLoading,
    currentUser,
    sessionToken,
    loginError
  })

  /**
   * Login user
   */
  const login = async (credentials: LoginCredentials): Promise<boolean> => {
    try {
      isLoading.value = true
      loginError.value = null

      const response = await amf.login(credentials.username, credentials.password)

      if (response.success && response.data) {
        // Store session data
        sessionToken.value = response.data.token
        isAuthenticated.value = true

        // Load full user profile
        await loadUserProfile()

        // Store credentials if remember me is enabled
        if (credentials.rememberMe) {
          localStorage.setItem('tootsville_credentials', JSON.stringify({
            username: credentials.username,
            rememberMe: true
          }))
        }

        // Emit login success event
        emit(EventFactory.loginSuccess({
          user: currentUser.value,
          token: sessionToken.value
        }))

        return true
      } else {
        loginError.value = response.error?.message || 'Login failed'
        emit(EventFactory.loginFailure(new Error(loginError.value)))
        return false
      }
    } catch (error: any) {
      loginError.value = error.message || 'Login failed'
      emit(EventFactory.loginFailure(error))
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Register new user
   */
  const register = async (registrationData: RegistrationData): Promise<boolean> => {
    try {
      isLoading.value = true
      loginError.value = null

      // Validate passwords match
      if (registrationData.password !== registrationData.confirmPassword) {
        loginError.value = 'Passwords do not match'
        return false
      }

      // Validate terms acceptance
      if (!registrationData.acceptTerms) {
        loginError.value = 'You must accept the terms and conditions'
        return false
      }

      const response = await amf.register({
        username: registrationData.username,
        email: registrationData.email,
        password: registrationData.password,
        parentEmail: registrationData.parentEmail
      })

      if (response.success) {
        // Automatically login after successful registration
        return await login({
          username: registrationData.username,
          password: registrationData.password
        })
      } else {
        loginError.value = response.error?.message || 'Registration failed'
        return false
      }
    } catch (error: any) {
      loginError.value = error.message || 'Registration failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Logout user
   */
  const logout = async (): Promise<void> => {
    try {
      isLoading.value = true

      // Call logout API
      await amf.logout()

      // Clear local state
      clearUserData()

      // Clear stored credentials
      localStorage.removeItem('tootsville_credentials')

      // Emit logout event
      emit(EventFactory.logout())

    } catch (error) {
      console.error('Logout error:', error)
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Load user profile from server
   */
  const loadUserProfile = async (): Promise<void> => {
    try {
      const response = await amf.getUserProfile()

      if (response.success && response.data) {
        const profileData = response.data

        // Merge with existing data or create new profile
        currentUser.value = {
          id: profileData.id,
          username: profileData.username,
          email: profileData.email,
          displayName: profileData.displayName || profileData.username,
          avatarId: profileData.avatarId,
          isModerator: profileData.isModerator || false,
          isAdmin: profileData.isAdmin || false,
          createdAt: profileData.createdAt,
          lastLoginAt: profileData.lastLoginAt,
          preferences: {
            ...defaultPreferences,
            ...profileData.preferences
          },
          statistics: {
            ...defaultStatistics,
            ...profileData.statistics
          }
        }
      }
    } catch (error) {
      console.error('Failed to load user profile:', error)
    }
  }

  /**
   * Update user profile
   */
  const updateProfile = async (updates: Partial<UserProfile>): Promise<boolean> => {
    try {
      isLoading.value = true

      const response = await amf.updateUserProfile(updates)

      if (response.success) {
        // Update local profile data
        if (currentUser.value) {
          currentUser.value = {
            ...currentUser.value,
            ...updates
          }
        }
        return true
      } else {
        loginError.value = response.error?.message || 'Profile update failed'
        return false
      }
    } catch (error: any) {
      loginError.value = error.message || 'Profile update failed'
      return false
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Update user preferences
   */
  const updatePreferences = async (preferences: Partial<UserPreferences>): Promise<boolean> => {
    if (!currentUser.value) return false

    try {
      const updatedPreferences = {
        ...currentUser.value.preferences,
        ...preferences
      }

      const success = await updateProfile({
        preferences: updatedPreferences
      })

      if (success) {
        // Apply preference changes locally
        applyPreferences(updatedPreferences)
      }

      return success
    } catch (error) {
      console.error('Failed to update preferences:', error)
      return false
    }
  }

  /**
   * Update user statistics
   */
  const updateStatistics = async (statistics: Partial<UserStatistics>): Promise<boolean> => {
    if (!currentUser.value) return false

    try {
      const updatedStatistics = {
        ...currentUser.value.statistics,
        ...statistics,
        lastUpdated: new Date().toISOString()
      }

      return await updateProfile({
        statistics: updatedStatistics
      })
    } catch (error) {
      console.error('Failed to update statistics:', error)
      return false
    }
  }

  /**
   * Add experience points and handle level ups
   */
  const addExperience = async (xp: number): Promise<void> => {
    if (!currentUser.value) return

    const newExp = currentUser.value.statistics.experience + xp
    const currentLevel = currentUser.value.statistics.level
    const newLevel = Math.floor(newExp / 1000) + 1 // Simple leveling formula

    await updateStatistics({
      experience: newExp,
      level: Math.max(newLevel, currentLevel)
    })

    // Emit level up event if leveled up
    if (newLevel > currentLevel) {
      emit(new CustomEvent(EventType.CUSTOM, {
        type: 'levelUp',
        oldLevel: currentLevel,
        newLevel,
        experience: newExp
      }))
    }
  }

  /**
   * Add peanuts to user balance
   */
  const addPeanuts = async (amount: number): Promise<void> => {
    if (!currentUser.value) return

    const newBalance = currentUser.value.statistics.peanuts + amount

    await updateStatistics({
      peanuts: Math.max(0, newBalance) // Ensure balance doesn't go negative
    })

    emit(new CustomEvent(EventType.BALANCE_UPDATE, {
      oldBalance: currentUser.value.statistics.peanuts,
      newBalance,
      change: amount
    }))
  }

  /**
   * Refresh user data from server
   */
  const refreshUserData = async (): Promise<void> => {
    if (!isAuthenticated.value) return

    try {
      isLoading.value = true
      await loadUserProfile()
    } catch (error) {
      console.error('Failed to refresh user data:', error)
    } finally {
      isLoading.value = false
    }
  }

  /**
   * Check if user has permission for action
   */
  const hasPermission = (permission: string): boolean => {
    if (!currentUser.value) return false

    // Admin has all permissions
    if (currentUser.value.isAdmin) return true

    // Moderator permissions
    if (currentUser.value.isModerator) {
      const moderatorPermissions = ['moderate_chat', 'kick_users', 'ban_users']
      return moderatorPermissions.includes(permission)
    }

    // Regular user permissions
    const userPermissions = ['play_games', 'chat', 'trade_items', 'customize_avatar']
    return userPermissions.includes(permission)
  }

  /**
   * Apply preferences to application
   */
  const applyPreferences = (preferences: UserPreferences): void => {
    // Apply theme
    document.documentElement.setAttribute('data-theme', preferences.theme)

    // Apply language
    // Note: This would integrate with i18n system

    // Store preferences in localStorage for persistence
    localStorage.setItem('tootsville_preferences', JSON.stringify(preferences))
  }

  /**
   * Clear all user data
   */
  const clearUserData = (): void => {
    isAuthenticated.value = false
    currentUser.value = null
    sessionToken.value = null
    loginError.value = null

    // Clear stored preferences
    localStorage.removeItem('tootsville_preferences')
  }

  /**
   * Initialize store
   */
  const initialize = async (): Promise<void> => {
    // Load stored preferences
    const storedPrefs = localStorage.getItem('tootsville_preferences')
    if (storedPrefs) {
      try {
        const preferences = JSON.parse(storedPrefs)
        applyPreferences({ ...defaultPreferences, ...preferences })
      } catch (error) {
        console.error('Failed to load stored preferences:', error)
      }
    }

    // Check for stored credentials and auto-login
    const storedCredentials = localStorage.getItem('tootsville_credentials')
    if (storedCredentials) {
      try {
        const credentials = JSON.parse(storedCredentials)
        if (credentials.rememberMe && credentials.username) {
          // Note: In a real implementation, you'd need to securely store and retrieve passwords
          // This is just for demonstration
          console.log('Auto-login available for:', credentials.username)
        }
      } catch (error) {
        console.error('Failed to load stored credentials:', error)
      }
    }
  }

  // Initialize on first use
  initialize()

  return {
    // State
    state,
    userId,
    username,
    displayName,
    isModerator,
    isAdmin,
    userLevel,
    userPeanuts,

    // Methods
    login,
    register,
    logout,
    loadUserProfile,
    updateProfile,
    updatePreferences,
    updateStatistics,
    addExperience,
    addPeanuts,
    refreshUserData,
    hasPermission,
    clearUserData
  }
}

// Singleton instance
let userStoreInstance: ReturnType<typeof useUserStore> | null = null

/**
 * Get singleton user store instance
 */
export function getUserStore(): ReturnType<typeof useUserStore> {
  if (!userStoreInstance) {
    userStoreInstance = useUserStore()
  }
  return userStoreInstance
}
