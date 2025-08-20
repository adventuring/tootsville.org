/**
 * index.js - Main entry point for Tootsville React client
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';

// Initialize client bindings globally
import clientBindings from './bindings/ClientBindings';

// Make bindings available for JSCL
if (typeof window !== 'undefined') {
  window.TootsvilleBindings = clientBindings;
  window.Tootsville = {
    game: clientBindings.game,
    communication: clientBindings.communication,
    components: clientBindings.components,
    utils: clientBindings.utils
  };
}

// Create root element
const root = ReactDOM.createRoot(document.getElementById('root'));

// Render application
root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
