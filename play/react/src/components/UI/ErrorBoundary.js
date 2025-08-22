/**
 * ErrorBoundary.js - React error boundary component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Catches JavaScript errors in child components and displays fallback UI.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */
import React from 'react';

/**
 * Error boundary component
 */
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = { hasError: false, error: null, errorInfo: null };
  }

  static getDerivedStateFromError(error) {
    // Update state so the next render will show the fallback UI
    return { hasError: true };
  }

  componentDidCatch(error, errorInfo) {
    // Log error to console and any error reporting service
    console.error('Error caught by boundary:', error, errorInfo);
    
    this.setState({
      error: error,
      errorInfo: errorInfo
    });

    // TODO: Send error to error reporting service (e.g., Rollbar)
    // if (window.rollbar) {
    //   window.rollbar.error(error, errorInfo);
    // }
  }

  render() {
    if (this.state.hasError) {
      return (
        <div className="error-boundary">
          <div className="error-content">
            <h1>Something went wrong</h1>
            <p>We're sorry, but something unexpected happened. Please try refreshing the page.</p>
            
            {process.env.NODE_ENV === 'development' && this.state.error && (
              <details style={{ whiteSpace: 'pre-wrap' }}>
                <summary>Error Details</summary>
                <p>{this.state.error.toString()}</p>
                <p>{this.state.errorInfo.componentStack}</p>
              </details>
            )}
            
            <button 
              onClick={() => window.location.reload()}
              className="retry-button"
            >
              Refresh Page
            </button>
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

export default ErrorBoundary;
