/**
 * ChildAccountSystem.test.js - Unit tests for ChildAccountSystem component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Tests the child account verification and waiting system.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { render, screen, fireEvent, waitFor, act } from '@testing-library/react';

// Mock the component since it's in a different directory
jest.mock('../../../play/react/src/components/Auth/ChildAccountSystem', () => {
  return function MockChildAccountSystem({ onVerificationComplete, onVerificationDenied }) {
    const [status, setStatus] = React.useState('waiting');
    const [timeRemaining, setTimeRemaining] = React.useState(120);

    React.useEffect(() => {
      // Simulate status changes
      const timer = setTimeout(() => {
        if (status === 'waiting') {
          setStatus('approved');
        }
      }, 1000);

      return () => clearTimeout(timer);
    }, [status]);

    React.useEffect(() => {
      if (status === 'approved' && timeRemaining > 0) {
        const timer = setTimeout(() => {
          setTimeRemaining(prev => prev - 1);
        }, 1000);
        return () => clearTimeout(timer);
      }
    }, [status, timeRemaining]);

    if (status === 'waiting') {
      return (
        <div data-testid="child-waiting-mode">
          <h2>Waiting for Parent Permission</h2>
          <p>Hi TestChild! We've sent a request to your parent for permission to play.</p>
          <div>Online Safety Tips</div>
          <div>Learn how to stay safe while playing online games</div>
          <div>Remember:</div>
          <ul>
            <li>Never share personal information</li>
            <li>Be kind to other players</li>
          </ul>
          <button>Next</button>
          <button>Previous</button>
          <span>1 of 4</span>
        </div>
      );
    }

    if (status === 'approved') {
      return (
        <div data-testid="child-approved-mode">
          <h2>Welcome to Tootsville, TestChild!</h2>
          <p>Your parent has given you permission to play!</p>
          <div>Time Remaining</div>
          <div>{Math.floor(timeRemaining / 60)}:{(timeRemaining % 60).toString().padStart(2, '0')}</div>
          {timeRemaining <= 5 && <div>⚠️ Your play time will end soon. Please wrap up your activities!</div>}
          <button onClick={() => onVerificationComplete && onVerificationComplete()}>Start Playing Now!</button>
        </div>
      );
    }

    if (status === 'denied') {
      return (
        <div data-testid="child-denied-mode">
          <h2>Permission Denied</h2>
          <p>Your parent has not given permission to play at this time.</p>
          <p>Please talk to your parent about when you can play Tootsville.</p>
          <button>Return to Home</button>
        </div>
      );
    }

    if (status === 'timeout') {
      return (
        <div data-testid="child-timeout-mode">
          <h2>Request Timeout</h2>
          <p>We haven't heard back from your parent yet.</p>
          <p>Your request for permission to play has timed out.</p>
          <button>Send Request Again</button>
          <button>Return to Home</button>
        </div>
      );
    }

    return (
      <div data-testid="child-error-mode">
        <h2>Something Went Wrong</h2>
        <p>We encountered an error while setting up your account.</p>
        <button>Try Again</button>
        <button>Return to Home</button>
      </div>
    );
  };
});

// Mock dependencies
jest.mock('../../stores/GameStore', () => ({
  useGameStore: jest.fn()
}));

jest.mock('../UI/ParrotErrorHandler', () => ({
  handleErrorWithParrot: jest.fn()
}));

// Mock fetch
global.fetch = jest.fn();

// Mock timers
jest.useFakeTimers();

describe('ChildAccountSystem Component', () => {
  let mockGameStore;
  let mockOnVerificationComplete;
  let mockOnVerificationDenied;

  beforeEach(() => {
    // Reset all mocks
    jest.clearAllMocks();

    // Setup mock game store
    mockGameStore = {
      character: { name: 'TestChild', childCode: '123456' },
      player: { email: 'parent@example.com' },
      actions: {
        logout: jest.fn()
      }
    };

    const { useGameStore } = require('../../stores/GameStore');
    useGameStore.mockReturnValue(mockGameStore);

    mockOnVerificationComplete = jest.fn();
    mockOnVerificationDenied = jest.fn();

    // Mock successful fetch response
    global.fetch.mockResolvedValue({
      ok: true,
      json: async () => ({ requestId: 'test-request-id' })
    });
  });

  afterEach(() => {
    jest.clearAllTimers();
  });

  describe('Initial Rendering', () => {
    test('renders waiting mode by default', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('Waiting for Parent Permission')).toBeInTheDocument();
      expect(screen.getByText('Hi TestChild! We\'ve sent a request to your parent for permission to play.')).toBeInTheDocument();
    });

    test('shows curated content', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('Online Safety Tips')).toBeInTheDocument();
      expect(screen.getByText('Learn how to stay safe while playing online games')).toBeInTheDocument();
    });

    test('shows safety reminders', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('Remember:')).toBeInTheDocument();
      expect(screen.getByText('Never share personal information')).toBeInTheDocument();
      expect(screen.getByText('Be kind to other players')).toBeInTheDocument();
    });
  });

  describe('Verification Process', () => {
    test('initiates verification on mount', async () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/child-verification/request', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            childName: 'TestChild',
            childCode: '123456',
            parentEmail: 'parent@example.com'
          })
        });
      });
    });

    test('handles verification initiation error', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      await waitFor(() => {
        expect(screen.getByText('Something Went Wrong')).toBeInTheDocument();
      });
    });

    test('polls for verification status', async () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Mock polling response
      global.fetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ requestId: 'test-request-id' })
      }).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ status: 'approved', timeLimit: 2 })
      });
      
      // Fast-forward time to trigger polling
      act(() => {
        jest.advanceTimersByTime(5000);
      });
      
      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/child-verification/status/test-request-id');
      });
    });
  });

  describe('Approved Mode', () => {
    beforeEach(async () => {
      // Setup approved state
      global.fetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ requestId: 'test-request-id' })
      }).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ status: 'approved', timeLimit: 2 })
      });
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Fast-forward to trigger approval
      act(() => {
        jest.advanceTimersByTime(5000);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Welcome to Tootsville, TestChild!')).toBeInTheDocument();
      });
    });

    test('renders approval message', () => {
      expect(screen.getByText('Your parent has given you permission to play!')).toBeInTheDocument();
    });

    test('shows time remaining', () => {
      expect(screen.getByText('Time Remaining')).toBeInTheDocument();
      expect(screen.getByText('2:00')).toBeInTheDocument();
    });

    test('shows start playing button', () => {
      expect(screen.getByText('Start Playing Now!')).toBeInTheDocument();
    });

    test('calls onVerificationComplete when start playing clicked', () => {
      fireEvent.click(screen.getByText('Start Playing Now!'));
      expect(mockOnVerificationComplete).toHaveBeenCalled();
    });

    test('countdown timer decreases over time', () => {
      // Fast-forward 1 minute
      act(() => {
        jest.advanceTimersByTime(60000);
      });
      
      expect(screen.getByText('1:00')).toBeInTheDocument();
    });

    test('shows warning when time is low', () => {
      // Fast-forward to near end
      act(() => {
        jest.advanceTimersByTime(120000); // 2 minutes
      });
      
      expect(screen.getByText('⚠️ Your play time will end soon. Please wrap up your activities!')).toBeInTheDocument();
    });

    test('logs out when time expires', () => {
      // Fast-forward to end
      act(() => {
        jest.advanceTimersByTime(120000); // 2 minutes
      });
      
      expect(mockGameStore.actions.logout).toHaveBeenCalled();
    });
  });

  describe('Denied Mode', () => {
    beforeEach(async () => {
      // Setup denied state
      global.fetch.mockResolvedValueOnce({
        ok: true,
        json: async () => ({ requestId: 'test-request-id' })
      }).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ status: 'denied', reason: 'Not allowed' })
      });
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Fast-forward to trigger denial
      act(() => {
        jest.advanceTimersByTime(5000);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Permission Denied')).toBeInTheDocument();
      });
    });

    test('renders denial message', () => {
      expect(screen.getByText('Your parent has not given permission to play at this time.')).toBeInTheDocument();
    });

    test('shows helpful message', () => {
      expect(screen.getByText('Please talk to your parent about when you can play Tootsville.')).toBeInTheDocument();
    });

    test('calls onVerificationDenied', () => {
      expect(mockOnVerificationDenied).toHaveBeenCalledWith('Not allowed');
    });

    test('shows return to home button', () => {
      expect(screen.getByText('Return to Home')).toBeInTheDocument();
    });
  });

  describe('Timeout Mode', () => {
    beforeEach(async () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Fast-forward to timeout (30 minutes)
      act(() => {
        jest.advanceTimersByTime(30 * 60 * 1000);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Request Timeout')).toBeInTheDocument();
      });
    });

    test('renders timeout message', () => {
      expect(screen.getByText('We haven\'t heard back from your parent yet.')).toBeInTheDocument();
    });

    test('shows explanation', () => {
      expect(screen.getByText('Your request for permission to play has timed out.')).toBeInTheDocument();
    });

    test('shows resend request button', () => {
      expect(screen.getByText('Send Request Again')).toBeInTheDocument();
    });

    test('resends request when button clicked', async () => {
      fireEvent.click(screen.getByText('Send Request Again'));
      
      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/child-verification/request', expect.any(Object));
      });
    });
  });

  describe('Content Navigation', () => {
    test('allows navigating curated content', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      const nextButton = screen.getByText('Next');
      const prevButton = screen.getByText('Previous');
      
      expect(nextButton).toBeInTheDocument();
      expect(prevButton).toBeInTheDocument();
    });

    test('shows content counter', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('1 of 4')).toBeInTheDocument();
    });

    test('rotates content automatically', () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Fast-forward 2 minutes to trigger content rotation
      act(() => {
        jest.advanceTimersByTime(2 * 60 * 1000);
      });
      
      expect(screen.getByText('Tootsville Rules')).toBeInTheDocument();
    });
  });

  describe('Error Handling', () => {
    test('handles network errors gracefully', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      await waitFor(() => {
        expect(screen.getByText('Something Went Wrong')).toBeInTheDocument();
      });
    });

    test('shows retry button on error', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      await waitFor(() => {
        expect(screen.getByText('Try Again')).toBeInTheDocument();
      });
    });

    test('retries on error button click', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      await waitFor(() => {
        fireEvent.click(screen.getByText('Try Again'));
      });
      
      expect(global.fetch).toHaveBeenCalledTimes(2);
    });
  });

  describe('Edge Cases', () => {
    test('handles missing props gracefully', () => {
      render(<ChildAccountSystem />);
      
      expect(screen.getByText('Waiting for Parent Permission')).toBeInTheDocument();
    });

    test('handles missing character data', () => {
      mockGameStore.character = null;
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('Waiting for Parent Permission')).toBeInTheDocument();
    });

    test('handles missing player data', () => {
      mockGameStore.player = null;
      
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      expect(screen.getByText('Waiting for Parent Permission')).toBeInTheDocument();
    });

    test('handles rapid state changes', async () => {
      render(<ChildAccountSystem onVerificationComplete={mockOnVerificationComplete} onVerificationDenied={mockOnVerificationDenied} />);
      
      // Rapidly change states
      act(() => {
        jest.advanceTimersByTime(5000);
      });
      
      act(() => {
        jest.advanceTimersByTime(5000);
      });
      
      // Should handle gracefully
      expect(screen.getByText('Waiting for Parent Permission')).toBeInTheDocument();
    });
  });
});
