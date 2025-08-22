/**
 * ParrotErrorHandler.js - Error handling using the Tootsville parrot interface
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides error handling using the existing parrot interface for consistency
 * with the rest of the Tootsville application.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useEffect } from 'react';

/**
 * ParrotErrorHandler component for displaying errors using the parrot interface
 * 
 * @component
 * @param {Object} props - Component props
 * @param {Error} props.error - The error to display
 * @param {Function} props.onDismiss - Callback when error is dismissed
 * @returns {JSX.Element} Parrot error handler
 */
const ParrotErrorHandler = ({ error, onDismiss }) => {
  useEffect(() => {
    if (error && window.Tootsville?.Gossip?.Parrot) {
      // Use the existing parrot interface to show the error
      window.Tootsville.Gossip.Parrot.say(
        'Error',
        `An error occurred: ${error.message || error}`
      ).then(() => {
        if (onDismiss) {
          onDismiss();
        }
      });
    }
  }, [error, onDismiss]);

  // Return null since the parrot interface handles the display
  return null;
};

/**
 * Global error handler that uses the parrot interface
 * @param {Error} error - The error to handle
 * @param {Function} onDismiss - Callback when error is dismissed
 */
export const handleErrorWithParrot = (error, onDismiss) => {
  if (window.Tootsville?.Gossip?.Parrot) {
    window.Tootsville.Gossip.Parrot.say(
      'Error',
      `An error occurred: ${error.message || error}`
    ).then(() => {
      if (onDismiss) {
        onDismiss();
      }
    });
  } else {
    // Fallback to console if parrot is not available
    console.error('Error:', error);
    if (onDismiss) {
      onDismiss();
    }
  }
};

export default ParrotErrorHandler;
