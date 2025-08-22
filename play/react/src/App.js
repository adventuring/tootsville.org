/**
 * App.js - Main React application for Tootsville
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Main application component that integrates all game systems and UI components.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React, { useState, useEffect } from 'react';
import useGameStore from './stores/GameStore';
import GameWorld from './components/GameWorld';
import GameHUD from './components/UI/GameHUD';
import LoginScreen from './components/UI/LoginScreen';
import LoadingScreen from './components/UI/LoadingScreen';
import ErrorBoundary from './components/UI/ErrorBoundary';
import ParrotErrorHandler from './components/UI/ParrotErrorHandler';
import './styles/App.css';

/**
 * Main application component
 * @returns {JSX.Element} App component
 */
const App = () => {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState(null);
  const [communicationService, setCommunicationService] = useState(null);

  useEffect(() => {
    // Check for existing authentication
    const checkAuth = async () => {
      try {
        const token = localStorage.getItem('tootsville_token');
        if (token) {
          // Validate token with server
          // For now, assume valid if exists
          setIsAuthenticated(true);
        }
      } catch (err) {
        console.error('Auth check failed:', err);
      } finally {
        setIsLoading(false);
      }
    };

    checkAuth();
  }, []);

  const handleLogin = async (credentials) => {
    try {
      setIsLoading(true);
      
      // TODO: Implement actual login logic with server
      // For now, simulate successful login
      localStorage.setItem('tootsville_token', 'demo_token');
      setIsAuthenticated(true);
      
    } catch (err) {
      setError(err.message);
    } finally {
      setIsLoading(false);
    }
  };

  const handleLogout = () => {
    localStorage.removeItem('tootsville_token');
    setIsAuthenticated(false);
    setCommunicationService(null);
  };

  const handleWorldReady = (commService) => {
    setCommunicationService(commService);
  };

  if (isLoading) {
    return <LoadingScreen />;
  }

  if (error) {
    return (
      <ErrorBoundary>
        <ParrotErrorHandler 
          error={error} 
          onDismiss={() => setError(null)} 
        />
      </ErrorBoundary>
    );
  }

  if (!isAuthenticated) {
    return (
      <ErrorBoundary>
        <LoginScreen onLogin={handleLogin} />
      </ErrorBoundary>
    );
  }

  return (
    <ErrorBoundary>
      <div className="app">
        <GameWorld onWorldReady={handleWorldReady} />
        <GameHUD 
          onLogout={handleLogout}
          communicationService={communicationService}
        />
      </div>
    </ErrorBoundary>
  );
};

export default App;
