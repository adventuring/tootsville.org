/**
 * LoginPanel.js - Authentication component for Tootsville client
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Maintains existing login/signup behaviors from the original Tootsville client
 * while providing modern React-based interface with mobile platform support.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useEffect, useRef } from 'react';
import { useGameStore } from '../../stores/GameStore';
import { CommunicationService } from '../../services/CommunicationService';
import { UIText } from '../../constants/UIText';
import mobilePlatformService from '../../services/MobilePlatformService';
import './LoginPanel.css';

/**
 * LoginPanel component for user authentication
 * 
 * @component
 * @description Handles user login, signup, and authentication state management
 * @param {Object} props - Component props
 * @param {Function} props.onLoginSuccess - Callback when login succeeds
 * @param {Function} props.onLoginError - Callback when login fails
 * @param {boolean} props.showSignup - Whether to show signup form
 * @param {Object} props.config - Configuration options
 * @returns {JSX.Element} Login panel component
 */
export const LoginPanel = ({ 
  onLoginSuccess, 
  onLoginError, 
  showSignup = false, 
  config = {} 
}) => {
  const [isSignup, setIsSignup] = useState(showSignup);
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');
  const [email, setEmail] = useState('');
  const [confirmPassword, setConfirmPassword] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState('');
  const [showPassword, setShowPassword] = useState(false);
  const [rememberMe, setRememberMe] = useState(false);
  const [captchaToken, setCaptchaToken] = useState('');
  
  const usernameRef = useRef(null);
  const passwordRef = useRef(null);
  const emailRef = useRef(null);
  
  const gameStore = useGameStore();
  const communicationService = useRef(new CommunicationService());
  
  // Platform-specific configuration
  const platform = mobilePlatformService.platform;
  const uiConfig = mobilePlatformService.getUIConfig();
  const inputConfig = mobilePlatformService.getInputConfig();

  useEffect(() => {
    // Focus username field on mount
    if (usernameRef.current) {
      usernameRef.current.focus();
    }

    // Load saved credentials if remember me was checked
    const savedCredentials = localStorage.getItem('tootsville_credentials');
    if (savedCredentials && config.allowRememberMe !== false) {
      try {
        const { username: savedUsername, rememberMe: savedRememberMe } = JSON.parse(savedCredentials);
        if (savedRememberMe) {
          setUsername(savedUsername);
          setRememberMe(true);
        }
      } catch (error) {
        console.warn('Failed to load saved credentials:', error);
      }
    }

    // Initialize captcha if required
    if (config.requireCaptcha) {
      initializeCaptcha();
    }

    // Platform-specific initialization
    if (platform.isMobile) {
      initializeMobileFeatures();
    }

    return () => {
      // Cleanup
      if (communicationService.current) {
        communicationService.current.disconnect();
      }
    };
  }, [config.requireCaptcha, config.allowRememberMe, platform.isMobile]);

  /**
   * Initialize captcha for bot protection
   */
  const initializeCaptcha = () => {
    // Implementation would integrate with captcha service
    // For now, generate a simple token
    setCaptchaToken(`captcha_${Date.now()}`);
  };

  /**
   * Initialize mobile-specific features
   */
  const initializeMobileFeatures = () => {
    // Lock orientation to portrait for mobile
    mobilePlatformService.lockOrientation('portrait');
    
    // Enable touch optimizations
    if (inputConfig.enableTouchOptimization) {
      // Add touch-specific event handlers
    }
  };

  /**
   * Handle form submission
   * @param {Event} e - Form submission event
   */
  const handleSubmit = async (e) => {
    e.preventDefault();
    setError('');
    setIsLoading(true);

    try {
      // Validate form
      if (!validateForm()) {
        return;
      }

      // Prepare authentication data
      const authData = {
        username: username.trim(),
        password: password,
        email: isSignup ? email.trim() : undefined,
        captchaToken: config.requireCaptcha ? captchaToken : undefined,
        platform: platform.type,
        deviceInfo: {
          userAgent: navigator.userAgent,
          screenSize: `${window.screen.width}x${window.screen.height}`,
          timezone: Intl.DateTimeFormat().resolvedOptions().timeZone
        }
      };

      // Perform authentication
      const result = await performAuthentication(authData);

      if (result.success) {
        // Save credentials if remember me is checked
        if (rememberMe && config.allowRememberMe !== false) {
          localStorage.setItem('tootsville_credentials', JSON.stringify({
            username: username.trim(),
            rememberMe: true
          }));
        } else {
          localStorage.removeItem('tootsville_credentials');
        }

        // Call success callback
        if (onLoginSuccess) {
          onLoginSuccess(result.user, result.session);
        }

        // Update game store
        gameStore.setUser(result.user);
        gameStore.setSession(result.session);
        gameStore.setConnected(true);

        // Initialize communication
        await initializeCommunication(result.session);

      } else {
        throw new Error(result.error || 'Authentication failed');
      }

    } catch (error) {
      console.error('Authentication error:', error);
      setError(error.message || 'Authentication failed. Please try again.');
      
      if (onLoginError) {
        onLoginError(error);
      }

      // Reset captcha if required
      if (config.requireCaptcha) {
        initializeCaptcha();
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Validate form data
   * @returns {boolean} True if form is valid
   */
  const validateForm = () => {
    // Username validation
    if (!username.trim()) {
      setError(UIText.LOGIN.USERNAME_REQUIRED);
      usernameRef.current?.focus();
      return false;
    }

    if (username.trim().length < 3) {
      setError(UIText.LOGIN.USERNAME_TOO_SHORT);
      usernameRef.current?.focus();
      return false;
    }

    if (username.trim().length > 20) {
      setError(UIText.LOGIN.USERNAME_TOO_LONG);
      usernameRef.current?.focus();
      return false;
    }

    // Password validation
    if (!password) {
      setError(UIText.LOGIN.PASSWORD_REQUIRED);
      passwordRef.current?.focus();
      return false;
    }

    if (password.length < 6) {
      setError(UIText.LOGIN.PASSWORD_TOO_SHORT);
      passwordRef.current?.focus();
      return false;
    }

    // Signup-specific validation
    if (isSignup) {
      // Email validation
      if (!email.trim()) {
        setError(UIText.SIGNUP.EMAIL_REQUIRED);
        emailRef.current?.focus();
        return false;
      }

      const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
      if (!emailRegex.test(email.trim())) {
        setError(UIText.SIGNUP.EMAIL_INVALID);
        emailRef.current?.focus();
        return false;
      }

      // Confirm password validation
      if (password !== confirmPassword) {
        setError(UIText.SIGNUP.PASSWORDS_DONT_MATCH);
        setConfirmPassword('');
        return false;
      }

      // Terms acceptance
      if (config.requireTermsAcceptance && !config.termsAccepted) {
        setError(UIText.SIGNUP.TERMS_REQUIRED);
        return false;
      }
    }

    // Captcha validation
    if (config.requireCaptcha && !captchaToken) {
      setError(UIText.LOGIN.CAPTCHA_REQUIRED);
      return false;
    }

    return true;
  };

  /**
   * Perform authentication with server
   * @param {Object} authData - Authentication data
   * @returns {Promise<Object>} Authentication result
   */
  const performAuthentication = async (authData) => {
    // Connect to communication service
    await communicationService.current.connect();

    // Send authentication request
    const response = await communicationService.current.send('authenticate', authData);

    if (response.success) {
      return {
        success: true,
        user: response.user,
        session: response.session
      };
    } else {
      return {
        success: false,
        error: response.error || 'Authentication failed'
      };
    }
  };

  /**
   * Initialize communication after successful authentication
   * @param {Object} session - User session data
   */
  const initializeCommunication = async (session) => {
    try {
      // Set up event listeners
      communicationService.current.on('avatarUpdate', (data) => {
        gameStore.updateAvatar(data);
      });

      communicationService.current.on('message', (data) => {
        gameStore.addMessage(data);
      });

      communicationService.current.on('itemUpdate', (data) => {
        gameStore.updateItem(data);
      });

      communicationService.current.on('weatherUpdate', (data) => {
        gameStore.updateWeather(data);
      });

      // Join world
      await communicationService.current.send('joinWorld', {
        sessionId: session.id,
        world: config.defaultWorld || 'main'
      });

    } catch (error) {
      console.error('Failed to initialize communication:', error);
      throw error;
    }
  };

  /**
   * Toggle between login and signup modes
   */
  const toggleMode = () => {
    setIsSignup(!isSignup);
    setError('');
    setPassword('');
    setConfirmPassword('');
    setEmail('');
  };

  /**
   * Handle forgot password
   */
  const handleForgotPassword = () => {
    // Implementation for password reset
    setError('Password reset functionality not yet implemented');
  };

  /**
   * Handle guest login
   */
  const handleGuestLogin = async () => {
    if (!config.allowGuestLogin) {
      setError('Guest login is not allowed');
      return;
    }

    setIsLoading(true);
    setError('');

    try {
      const guestData = {
        username: `Guest_${Math.random().toString(36).substr(2, 6)}`,
        isGuest: true,
        platform: platform.type
      };

      const result = await performAuthentication(guestData);

      if (result.success) {
        if (onLoginSuccess) {
          onLoginSuccess(result.user, result.session);
        }
        gameStore.setUser(result.user);
        gameStore.setSession(result.session);
        gameStore.setConnected(true);
      } else {
        throw new Error(result.error);
      }

    } catch (error) {
      setError(error.message);
      if (onLoginError) {
        onLoginError(error);
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Handle social login
   * @param {string} provider - Social login provider
   */
  const handleSocialLogin = async (provider) => {
    if (!config.socialLoginProviders?.includes(provider)) {
      setError(`${provider} login is not available`);
      return;
    }

    setIsLoading(true);
    setError('');

    try {
      // Implementation would integrate with social login providers
      setError(`${provider} login not yet implemented`);
    } catch (error) {
      setError(error.message);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div 
      className={`login-panel ${uiConfig.layout} ${platform.isMobile ? 'mobile' : 'desktop'}`}
      data-testid="login-panel"
    >
      <div className="login-container">
        <div className="login-header">
          <h1 className="login-title">
            {isSignup ? UIText.SIGNUP.TITLE : UIText.LOGIN.TITLE}
          </h1>
          <p className="login-subtitle">
            {isSignup ? UIText.SIGNUP.SUBTITLE : UIText.LOGIN.SUBTITLE}
          </p>
        </div>

        <form onSubmit={handleSubmit} className="login-form">
          {error && (
            <div className="error-message" data-testid="error-message">
              {error}
            </div>
          )}

          <div className="form-group">
            <label htmlFor="username" className="form-label">
              {UIText.LOGIN.USERNAME_LABEL}
            </label>
            <input
              ref={usernameRef}
              id="username"
              type="text"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
              className="form-input"
              placeholder={UIText.LOGIN.USERNAME_PLACEHOLDER}
              disabled={isLoading}
              autoComplete="username"
              data-testid="username-input"
            />
          </div>

          {isSignup && (
            <div className="form-group">
              <label htmlFor="email" className="form-label">
                {UIText.SIGNUP.EMAIL_LABEL}
              </label>
              <input
                ref={emailRef}
                id="email"
                type="email"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                className="form-input"
                placeholder={UIText.SIGNUP.EMAIL_PLACEHOLDER}
                disabled={isLoading}
                autoComplete="email"
                data-testid="email-input"
              />
            </div>
          )}

          <div className="form-group">
            <label htmlFor="password" className="form-label">
              {UIText.LOGIN.PASSWORD_LABEL}
            </label>
            <div className="password-input-container">
              <input
                ref={passwordRef}
                id="password"
                type={showPassword ? 'text' : 'password'}
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                className="form-input password-input"
                placeholder={UIText.LOGIN.PASSWORD_PLACEHOLDER}
                disabled={isLoading}
                autoComplete={isSignup ? 'new-password' : 'current-password'}
                data-testid="password-input"
              />
              <button
                type="button"
                className="password-toggle"
                onClick={() => setShowPassword(!showPassword)}
                disabled={isLoading}
                data-testid="password-toggle"
              >
                {showPassword ? '👁️' : '👁️‍🗨️'}
              </button>
            </div>
          </div>

          {isSignup && (
            <div className="form-group">
              <label htmlFor="confirmPassword" className="form-label">
                {UIText.SIGNUP.CONFIRM_PASSWORD_LABEL}
              </label>
              <input
                id="confirmPassword"
                type="password"
                value={confirmPassword}
                onChange={(e) => setConfirmPassword(e.target.value)}
                className="form-input"
                placeholder={UIText.SIGNUP.CONFIRM_PASSWORD_PLACEHOLDER}
                disabled={isLoading}
                autoComplete="new-password"
                data-testid="confirm-password-input"
              />
            </div>
          )}

          {config.requireCaptcha && (
            <div className="form-group">
              <div className="captcha-container">
                {/* Captcha implementation would go here */}
                <div className="captcha-placeholder">
                  {UIText.LOGIN.CAPTCHA_PLACEHOLDER}
                </div>
              </div>
            </div>
          )}

          {!isSignup && config.allowRememberMe !== false && (
            <div className="form-group checkbox-group">
              <label className="checkbox-label">
                <input
                  type="checkbox"
                  checked={rememberMe}
                  onChange={(e) => setRememberMe(e.target.checked)}
                  disabled={isLoading}
                  data-testid="remember-me-checkbox"
                />
                <span className="checkbox-text">
                  {UIText.LOGIN.REMEMBER_ME}
                </span>
              </label>
            </div>
          )}

          <button
            type="submit"
            className="submit-button"
            disabled={isLoading}
            data-testid="submit-button"
          >
            {isLoading ? (
              <span className="loading-spinner">⏳</span>
            ) : (
              isSignup ? UIText.SIGNUP.SUBMIT : UIText.LOGIN.SUBMIT
            )}
          </button>
        </form>

        <div className="login-actions">
          <button
            type="button"
            className="mode-toggle-button"
            onClick={toggleMode}
            disabled={isLoading}
            data-testid="mode-toggle-button"
          >
            {isSignup ? UIText.LOGIN.SWITCH_TO_LOGIN : UIText.SIGNUP.SWITCH_TO_SIGNUP}
          </button>

          {!isSignup && (
            <button
              type="button"
              className="forgot-password-button"
              onClick={handleForgotPassword}
              disabled={isLoading}
              data-testid="forgot-password-button"
            >
              {UIText.LOGIN.FORGOT_PASSWORD}
            </button>
          )}
        </div>

        {config.allowGuestLogin && (
          <div className="guest-login-section">
            <button
              type="button"
              className="guest-login-button"
              onClick={handleGuestLogin}
              disabled={isLoading}
              data-testid="guest-login-button"
            >
              {UIText.LOGIN.GUEST_LOGIN}
            </button>
          </div>
        )}

        {config.socialLoginProviders && config.socialLoginProviders.length > 0 && (
          <div className="social-login-section">
            <div className="social-login-divider">
              <span>{UIText.LOGIN.OR_LOGIN_WITH}</span>
            </div>
            <div className="social-login-buttons">
              {config.socialLoginProviders.map(provider => (
                <button
                  key={provider}
                  type="button"
                  className={`social-login-button ${provider}`}
                  onClick={() => handleSocialLogin(provider)}
                  disabled={isLoading}
                  data-testid={`social-login-${provider}`}
                >
                  {UIText.LOGIN[`${provider.toUpperCase()}_LOGIN`] || `${provider} Login`}
                </button>
              ))}
            </div>
          </div>
        )}

        <div className="login-footer">
          <p className="terms-notice">
            {isSignup ? UIText.SIGNUP.TERMS_NOTICE : UIText.LOGIN.TERMS_NOTICE}
          </p>
          <p className="support-info">
            {UIText.LOGIN.SUPPORT_INFO}
          </p>
        </div>
      </div>
    </div>
  );
};

export default LoginPanel;



