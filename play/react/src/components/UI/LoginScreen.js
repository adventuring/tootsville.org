/**
 * LoginScreen.js - User authentication screen
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides user login and registration functionality.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React, { useState } from 'react';
import './LoginScreen.css';

/**
 * Login screen component
 * @param {Object} props - Component props
 * @param {Function} props.onLogin - Login callback function
 * @returns {JSX.Element} LoginScreen component
 */
const LoginScreen = ({ onLogin }) => {
  const [isLogin, setIsLogin] = useState(true);
  const [credentials, setCredentials] = useState({
    username: '',
    password: '',
    email: ''
  });
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState('');

  const handleSubmit = async (e) => {
    e.preventDefault();
    setError('');
    setIsLoading(true);

    try {
      await onLogin(credentials);
    } catch (err) {
      setError(err.message || 'Login failed');
    } finally {
      setIsLoading(false);
    }
  };

  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setCredentials(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const toggleMode = () => {
    setIsLogin(!isLogin);
    setError('');
  };

  return (
    <div className="login-screen">
      <div className="login-container">
        <div className="login-header">
          <h1>Tootsville</h1>
          <p>Welcome to the world of adventure!</p>
        </div>

        <form onSubmit={handleSubmit} className="login-form">
          {error && <div className="error-message">{error}</div>}
          
          <div className="form-group">
            <label htmlFor="username">Username</label>
            <input
              type="text"
              id="username"
              name="username"
              value={credentials.username}
              onChange={handleInputChange}
              required
              disabled={isLoading}
            />
          </div>

          {!isLogin && (
            <div className="form-group">
              <label htmlFor="email">Email</label>
              <input
                type="email"
                id="email"
                name="email"
                value={credentials.email}
                onChange={handleInputChange}
                required
                disabled={isLoading}
              />
            </div>
          )}

          <div className="form-group">
            <label htmlFor="password">Password</label>
            <input
              type="password"
              id="password"
              name="password"
              value={credentials.password}
              onChange={handleInputChange}
              required
              disabled={isLoading}
            />
          </div>

          <button 
            type="submit" 
            className="submit-button"
            disabled={isLoading}
          >
            {isLoading ? 'Loading...' : (isLogin ? 'Login' : 'Register')}
          </button>
        </form>

        <div className="login-footer">
          <button 
            type="button" 
            className="toggle-button"
            onClick={toggleMode}
            disabled={isLoading}
          >
            {isLogin ? 'Need an account? Register' : 'Have an account? Login'}
          </button>
        </div>
      </div>
    </div>
  );
};

export default LoginScreen;
