/**
 * index.js - Main entry point for Tootsville React application
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Renders the main React application and sets up global configurations.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';

// Create root element
const root = ReactDOM.createRoot(document.getElementById('root'));

// Render the application
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
