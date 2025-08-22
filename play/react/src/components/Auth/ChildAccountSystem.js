/**
 * ChildAccountSystem.js - Child account management and verification system
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Implements the child account system with parental verification,
 * waiting mode with curated content, and time limit management.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useEffect, useRef } from 'react';
import { useGameStore } from '../../stores/GameStore';
import { handleErrorWithParrot } from '../UI/ParrotErrorHandler';

const ChildAccountSystem = ({ onVerificationComplete, onVerificationDenied }) => {
  const [verificationStatus, setVerificationStatus] = useState('waiting'); // waiting, approved, denied, timeout
  const [timeRemaining, setTimeRemaining] = useState(null);
  const [requestId, setRequestId] = useState(null);
  const [curatedContent, setCuratedContent] = useState([]);
  const [currentContentIndex, setCurrentContentIndex] = useState(0);
  const [showTimeoutMessage, setShowTimeoutMessage] = useState(false);
  
  const gameStore = useGameStore();
  const timeoutRef = useRef(null);
  const contentIntervalRef = useRef(null);

  // Curated G-rated content for waiting children
  const waitingContent = [
    {
      type: 'video',
      title: 'Online Safety Tips',
      description: 'Learn how to stay safe while playing online games',
      url: '/content/safety-tips.mp4',
      duration: 120
    },
    {
      type: 'video',
      title: 'Tootsville Rules',
      description: 'Understanding the rules of Tootsville',
      url: '/content/rules.mp4',
      duration: 90
    },
    {
      type: 'interactive',
      title: 'Character Creation Tips',
      description: 'Learn how to create the perfect Toot character',
      content: 'Choose colors that make you happy!'
    },
    {
      type: 'video',
      title: 'Exploring Tootsville',
      description: 'See what adventures await you in Tootsville',
      url: '/content/exploring.mp4',
      duration: 180
    }
  ];

  useEffect(() => {
    // Initialize the verification process
    initiateVerification();
    
    // Start curated content rotation
    startContentRotation();
    
    // Set up timeout for verification (30 minutes)
    timeoutRef.current = setTimeout(() => {
      setVerificationStatus('timeout');
      setShowTimeoutMessage(true);
    }, 30 * 60 * 1000);

    return () => {
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current);
      }
      if (contentIntervalRef.current) {
        clearInterval(contentIntervalRef.current);
      }
    };
  }, []);

  useEffect(() => {
    if (verificationStatus === 'approved' && timeRemaining !== null) {
      // Start countdown timer
      const countdownInterval = setInterval(() => {
        setTimeRemaining(prev => {
          if (prev <= 1) {
            clearInterval(countdownInterval);
            // Time expired, log out child
            handleTimeExpired();
            return 0;
          }
          return prev - 1;
        });
      }, 1000);

      return () => clearInterval(countdownInterval);
    }
  }, [verificationStatus, timeRemaining]);

  const initiateVerification = async () => {
    try {
      const response = await fetch('/api/child-verification/request', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          childName: gameStore.character?.name,
          childCode: gameStore.character?.childCode,
          parentEmail: gameStore.player?.email
        })
      });

      if (!response.ok) {
        throw new Error('Failed to initiate verification');
      }

      const data = await response.json();
      setRequestId(data.requestId);
      
      // Start polling for verification status
      pollVerificationStatus(data.requestId);
      
    } catch (error) {
      handleErrorWithParrot(error);
      setVerificationStatus('error');
    }
  };

  const pollVerificationStatus = (requestId) => {
    const pollInterval = setInterval(async () => {
      try {
        const response = await fetch(`/api/child-verification/status/${requestId}`);
        
        if (response.ok) {
          const status = await response.json();
          
          if (status.status === 'approved') {
            clearInterval(pollInterval);
            setVerificationStatus('approved');
            setTimeRemaining(status.timeLimit * 60); // Convert hours to seconds
            if (onVerificationComplete) {
              onVerificationComplete(status);
            }
          } else if (status.status === 'denied') {
            clearInterval(pollInterval);
            setVerificationStatus('denied');
            if (onVerificationDenied) {
              onVerificationDenied(status.reason);
            }
          }
        }
      } catch (error) {
        console.error('Error polling verification status:', error);
      }
    }, 5000); // Poll every 5 seconds
  };

  const startContentRotation = () => {
    setCuratedContent(waitingContent);
    
    // Rotate content every 2 minutes
    contentIntervalRef.current = setInterval(() => {
      setCurrentContentIndex(prev => (prev + 1) % waitingContent.length);
    }, 2 * 60 * 1000);
  };

  const handleTimeExpired = () => {
    // Log out the child and show appropriate message
    gameStore.actions.logout();
    handleErrorWithParrot(new Error('Your play time has expired. Please ask your parent for more time.'));
  };

  const handleResendRequest = () => {
    setShowTimeoutMessage(false);
    setVerificationStatus('waiting');
    initiateVerification();
  };

  const formatTime = (seconds) => {
    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = seconds % 60;
    
    if (hours > 0) {
      return `${hours}:${minutes.toString().padStart(2, '0')}:${secs.toString().padStart(2, '0')}`;
    }
    return `${minutes}:${secs.toString().padStart(2, '0')}`;
  };

  const renderWaitingMode = () => (
    <div className="child-waiting-mode">
      <div className="waiting-header">
        <h2>Waiting for Parent Permission</h2>
        <p>Hi {gameStore.character?.name}! We've sent a request to your parent for permission to play.</p>
        <p>While you wait, enjoy some fun content about Tootsville!</p>
      </div>

      <div className="curated-content">
        <div className="content-player">
          {curatedContent[currentContentIndex] && (
            <div className="content-item">
              <h3>{curatedContent[currentContentIndex].title}</h3>
              <p>{curatedContent[currentContentIndex].description}</p>
              
              {curatedContent[currentContentIndex].type === 'video' && (
                <div className="video-placeholder">
                  <div className="video-player">
                    <span>🎬 {curatedContent[currentContentIndex].title}</span>
                    <p>Video content would play here</p>
                  </div>
                </div>
              )}
              
              {curatedContent[currentContentIndex].type === 'interactive' && (
                <div className="interactive-content">
                  <p>{curatedContent[currentContentIndex].content}</p>
                </div>
              )}
            </div>
          )}
        </div>
        
        <div className="content-navigation">
          <button 
            onClick={() => setCurrentContentIndex(prev => (prev - 1 + curatedContent.length) % curatedContent.length)}
            disabled={curatedContent.length <= 1}
          >
            Previous
          </button>
          <span>{currentContentIndex + 1} of {curatedContent.length}</span>
          <button 
            onClick={() => setCurrentContentIndex(prev => (prev + 1) % curatedContent.length)}
            disabled={curatedContent.length <= 1}
          >
            Next
          </button>
        </div>
      </div>

      <div className="waiting-status">
        <div className="status-indicator">
          <div className="spinner"></div>
          <p>Waiting for parent response...</p>
        </div>
        
        <div className="safety-reminder">
          <h4>Remember:</h4>
          <ul>
            <li>Never share personal information</li>
            <li>Be kind to other players</li>
            <li>Ask for help if you need it</li>
            <li>Take breaks when you're tired</li>
          </ul>
        </div>
      </div>
    </div>
  );

  const renderApprovedMode = () => (
    <div className="child-approved-mode">
      <div className="approval-header">
        <h2>Welcome to Tootsville, {gameStore.character?.name}!</h2>
        <p>Your parent has given you permission to play!</p>
      </div>

      <div className="time-remaining">
        <h3>Time Remaining</h3>
        <div className="countdown-timer">
          {timeRemaining !== null && (
            <div className={`timer ${timeRemaining <= 300 ? 'warning' : ''}`}>
              {formatTime(timeRemaining)}
            </div>
          )}
        </div>
        
        {timeRemaining <= 300 && (
          <div className="time-warning">
            <p>⚠️ Your play time will end soon. Please wrap up your activities!</p>
          </div>
        )}
      </div>

      <div className="game-access">
        <button 
          onClick={() => onVerificationComplete && onVerificationComplete()}
          className="btn-primary"
        >
          Start Playing Now!
        </button>
      </div>
    </div>
  );

  const renderDeniedMode = () => (
    <div className="child-denied-mode">
      <div className="denial-header">
        <h2>Permission Denied</h2>
        <p>Your parent has not given permission to play at this time.</p>
      </div>

      <div className="denial-message">
        <p>Please talk to your parent about when you can play Tootsville.</p>
        <p>You can try again later when they're available.</p>
      </div>

      <div className="denial-actions">
        <button onClick={() => window.location.href = '/'} className="btn-secondary">
          Return to Home
        </button>
      </div>
    </div>
  );

  const renderTimeoutMode = () => (
    <div className="child-timeout-mode">
      <div className="timeout-header">
        <h2>Request Timeout</h2>
        <p>We haven't heard back from your parent yet.</p>
      </div>

      <div className="timeout-message">
        <p>Your request for permission to play has timed out.</p>
        <p>This might happen if your parent is busy or hasn't checked their email.</p>
      </div>

      <div className="timeout-actions">
        <button onClick={handleResendRequest} className="btn-primary">
          Send Request Again
        </button>
        <button onClick={() => window.location.href = '/'} className="btn-secondary">
          Return to Home
        </button>
      </div>
    </div>
  );

  const renderErrorMode = () => (
    <div className="child-error-mode">
      <div className="error-header">
        <h2>Something Went Wrong</h2>
        <p>We encountered an error while setting up your account.</p>
      </div>

      <div className="error-actions">
        <button onClick={() => window.location.reload()} className="btn-primary">
          Try Again
        </button>
        <button onClick={() => window.location.href = '/'} className="btn-secondary">
          Return to Home
        </button>
      </div>
    </div>
  );

  const renderContent = () => {
    switch (verificationStatus) {
      case 'waiting':
        return renderWaitingMode();
      case 'approved':
        return renderApprovedMode();
      case 'denied':
        return renderDeniedMode();
      case 'timeout':
        return renderTimeoutMode();
      case 'error':
        return renderErrorMode();
      default:
        return <div>Loading...</div>;
    }
  };

  return (
    <div className="child-account-system">
      {renderContent()}
    </div>
  );
};

export default ChildAccountSystem;
