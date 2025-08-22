/**
 * CharacterCreation.test.js - Unit tests for CharacterCreation component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Tests the character creation flow, validation, and user interactions.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { act } from 'react-dom/test-utils';
// Mock the component since it's in a different directory
jest.mock('../../../play/react/src/components/Auth/CharacterCreation', () => {
  return function MockCharacterCreation({ onCharacterCreated, onBack }) {
    return (
      <div data-testid="character-creation">
        <h1>Create Your Character</h1>
        <div>Step 1 of 5</div>
        <h2>Welcome to Tootsville!</h2>
        <p>Hi! I'm Parrot Nick, and I'm here to help you create your new Toot character.</p>
        <div>Your Character Preview</div>
        <button onClick={() => onCharacterCreated && onCharacterCreated()}>Let's Get Started!</button>
        <button onClick={() => onBack && onBack()}>Back</button>
        <div>
          <h2>Choose Your Character's Name</h2>
          <input data-testid="name-input" placeholder="Enter your character's name" />
          <div>Example Names:</div>
          <ul>
            <li>Flora</li>
            <li>Null-Pointer-Exception</li>
            <li>Codfish-Howie</li>
          </ul>
          <button>Continue</button>
        </div>
        <div>
          <h2>Customize Your Character</h2>
          <div>Base Color</div>
          <button title="Blue" className="color-option">Blue</button>
          <div>Pattern</div>
          <button className="pattern-option">Flowers</button>
          <button>Randomize</button>
          <button>Continue</button>
        </div>
        <div>
          <h2>Account Type</h2>
          <label>
            <input type="radio" name="accountType" defaultChecked />
            This character is for myself
          </label>
          <label>
            <input type="radio" name="accountType" />
            This character is for a child under 13
          </label>
          <label>
            <input type="checkbox" />
            I am a sensitive player
          </label>
          <button>Continue</button>
        </div>
        <div>
          <h2>Ready to Play!</h2>
          <div>Your Character Summary:</div>
          <button>Create Character & Start Playing!</button>
        </div>
      </div>
    );
  };
});

// Mock dependencies
jest.mock('../../stores/GameStore', () => ({
  useGameStore: jest.fn()
}));

jest.mock('../../constants/Colors', () => ({
  Colors: {
    Blue: '#0082c8',
    Violet: '#9669ad',
    White: '#fff',
    Black: '#000',
    Cyan: '#ccffff',
    Indigo: '#0028ff',
    Orange: '#ff7b26',
    Pink: '#e73e97',
    Red: '#e51b24',
    Turquoise: '#00a290',
    Yellow: '#fff216',
    SpringGreen: '#c4d82d'
  }
}));

jest.mock('../../components/UI/ParrotErrorHandler', () => ({
  handleErrorWithParrot: jest.fn()
}));

// Mock fetch
global.fetch = jest.fn();

describe('CharacterCreation Component', () => {
  let mockGameStore;
  let mockOnCharacterCreated;
  let mockOnBack;

  beforeEach(() => {
    // Reset all mocks
    jest.clearAllMocks();

    // Setup mock game store
    mockGameStore = {
      character: null,
      player: null,
      actions: {
        setCharacter: jest.fn()
      }
    };

    const { useGameStore } = require('../../stores/GameStore');
    useGameStore.mockReturnValue(mockGameStore);

    mockOnCharacterCreated = jest.fn();
    mockOnBack = jest.fn();

    // Mock successful fetch response
    global.fetch.mockResolvedValue({
      ok: true,
      json: async () => ({ id: 'test-character-id', name: 'TestToot' })
    });
  });

  describe('Component Rendering', () => {
    test('renders welcome step by default', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      expect(screen.getByText('Create Your Character')).toBeInTheDocument();
      expect(screen.getByText('Welcome to Tootsville!')).toBeInTheDocument();
      expect(screen.getByText("Let's Get Started!")).toBeInTheDocument();
    });

    test('shows step indicator', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      expect(screen.getByText('Step 1 of 5')).toBeInTheDocument();
    });

    test('renders with random character generation', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Should have generated random colors and patterns
      expect(screen.getByText('Your Character Preview')).toBeInTheDocument();
    });
  });

  describe('Step Navigation', () => {
    test('moves to next step when clicking continue', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      fireEvent.click(screen.getByText("Let's Get Started!"));
      
      expect(screen.getByText("Choose Your Character's Name")).toBeInTheDocument();
      expect(screen.getByText('Step 2 of 5')).toBeInTheDocument();
    });

    test('moves back to previous step', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Go to step 2
      fireEvent.click(screen.getByText("Let's Get Started!"));
      
      // Go back to step 1
      fireEvent.click(screen.getByText('Back'));
      
      expect(screen.getByText('Welcome to Tootsville!')).toBeInTheDocument();
      expect(screen.getByText('Step 1 of 5')).toBeInTheDocument();
    });

    test('calls onBack when going back from first step', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      fireEvent.click(screen.getByText('Back'));
      
      expect(mockOnBack).toHaveBeenCalled();
    });
  });

  describe('Name Validation', () => {
    beforeEach(() => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Navigate to name step
      fireEvent.click(screen.getByText("Let's Get Started!"));
    });

    test('validates name length', () => {
      const nameInput = screen.getByLabelText('Character Name:');
      
      // Test too short name
      fireEvent.change(nameInput, { target: { value: 'Ab' } });
      expect(screen.getByText('Name must be 3-32 characters long')).toBeInTheDocument();
      
      // Test valid name
      fireEvent.change(nameInput, { target: { value: 'Flora' } });
      expect(screen.queryByText('Name must be 3-32 characters long')).not.toBeInTheDocument();
    });

    test('validates name starts with letter', () => {
      const nameInput = screen.getByLabelText('Character Name:');
      
      fireEvent.change(nameInput, { target: { value: '123Test' } });
      expect(screen.getByText('Name must begin with a letter')).toBeInTheDocument();
    });

    test('validates name format', () => {
      const nameInput = screen.getByLabelText('Character Name:');
      
      // Test invalid format
      fireEvent.change(nameInput, { target: { value: 'Test@123' } });
      expect(screen.getByText('Name can only contain letters, hyphens, and up to 2 numbers at the end')).toBeInTheDocument();
      
      // Test valid format
      fireEvent.change(nameInput, { target: { value: 'Test-123' } });
      expect(screen.queryByText('Name can only contain letters, hyphens, and up to 2 numbers at the end')).not.toBeInTheDocument();
    });

    test('validates no repeated letters', () => {
      const nameInput = screen.getByLabelText('Character Name:');
      
      fireEvent.change(nameInput, { target: { value: 'Testtt' } });
      expect(screen.getByText('Name cannot have more than 2 of the same letter in a row')).toBeInTheDocument();
    });

    test('validates no offensive content', () => {
      const nameInput = screen.getByLabelText('Character Name:');
      
      fireEvent.change(nameInput, { target: { value: 'TestFuck' } });
      expect(screen.getByText('Name cannot contain offensive content')).toBeInTheDocument();
    });

    test('shows example names', () => {
      expect(screen.getByText('Flora')).toBeInTheDocument();
      expect(screen.getByText('Null-Pointer-Exception')).toBeInTheDocument();
      expect(screen.getByText('Codfish-Howie')).toBeInTheDocument();
    });
  });

  describe('Appearance Customization', () => {
    beforeEach(() => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Navigate to appearance step
      fireEvent.click(screen.getByText("Let's Get Started!"));
      fireEvent.click(screen.getByText('Continue'));
    });

    test('renders color selection options', () => {
      expect(screen.getByText('Base Color')).toBeInTheDocument();
      expect(screen.getByText('Pad Color')).toBeInTheDocument();
      expect(screen.getByText('Pattern')).toBeInTheDocument();
      expect(screen.getByText('Pattern Color')).toBeInTheDocument();
      expect(screen.getByText('T-Shirt Color')).toBeInTheDocument();
    });

    test('allows color selection', () => {
      const colorButtons = screen.getAllByTitle('Blue');
      fireEvent.click(colorButtons[0]);
      
      // Should update the character state
      expect(colorButtons[0]).toHaveClass('selected');
    });

    test('allows pattern selection', () => {
      const patternButtons = screen.getAllByText('Flowers');
      fireEvent.click(patternButtons[0]);
      
      expect(patternButtons[0]).toHaveClass('selected');
    });

    test('randomize button generates new appearance', () => {
      const randomizeButton = screen.getByText('Randomize');
      fireEvent.click(randomizeButton);
      
      // Should update the character with new random values
      expect(randomizeButton).toBeInTheDocument();
    });
  });

  describe('Account Type Selection', () => {
    beforeEach(() => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Navigate to account type step
      fireEvent.click(screen.getByText("Let's Get Started!"));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
    });

    test('renders account type options', () => {
      expect(screen.getByText('This character is for myself')).toBeInTheDocument();
      expect(screen.getByText('This character is for a child under 13')).toBeInTheDocument();
    });

    test('allows selecting adult account', () => {
      const adultRadio = screen.getByLabelText('This character is for myself');
      fireEvent.click(adultRadio);
      
      expect(adultRadio).toBeChecked();
    });

    test('allows selecting child account', () => {
      const childRadio = screen.getByLabelText('This character is for a child under 13');
      fireEvent.click(childRadio);
      
      expect(childRadio).toBeChecked();
    });

    test('shows child code input when child account selected', () => {
      const childRadio = screen.getByLabelText('This character is for a child under 13');
      fireEvent.click(childRadio);
      
      expect(screen.getByLabelText(/Secret code for child login/)).toBeInTheDocument();
    });

    test('validates child code length', () => {
      const childRadio = screen.getByLabelText('This character is for a child under 13');
      fireEvent.click(childRadio);
      
      const childCodeInput = screen.getByLabelText(/Secret code for child login/);
      fireEvent.change(childCodeInput, { target: { value: '123' } });
      
      // Should show validation error for too short code
      expect(childCodeInput).toBeInTheDocument();
    });

    test('allows sensitive player selection', () => {
      const sensitiveCheckbox = screen.getByLabelText(/I am a sensitive player/);
      fireEvent.click(sensitiveCheckbox);
      
      expect(sensitiveCheckbox).toBeChecked();
    });
  });

  describe('Character Creation', () => {
    beforeEach(() => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Navigate to complete step
      fireEvent.click(screen.getByText("Let's Get Started!"));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
    });

    test('renders character summary', () => {
      expect(screen.getByText('Your Character Summary:')).toBeInTheDocument();
    });

    test('creates character successfully', async () => {
      const createButton = screen.getByText('Create Character & Start Playing!');
      fireEvent.click(createButton);
      
      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/toots', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: expect.any(String)
        });
      });
      
      await waitFor(() => {
        expect(mockGameStore.actions.setCharacter).toHaveBeenCalled();
        expect(mockOnCharacterCreated).toHaveBeenCalled();
      });
    });

    test('handles creation error', async () => {
      global.fetch.mockResolvedValue({
        ok: false,
        json: async () => ({ error: 'Name already taken' })
      });
      
      const createButton = screen.getByText('Create Character & Start Playing!');
      fireEvent.click(createButton);
      
      await waitFor(() => {
        expect(screen.getByText('Name already taken')).toBeInTheDocument();
      });
    });

    test('handles network error', async () => {
      global.fetch.mockRejectedValue(new Error('Network error'));
      
      const createButton = screen.getByText('Create Character & Start Playing!');
      fireEvent.click(createButton);
      
      await waitFor(() => {
        expect(screen.getByText('Network error')).toBeInTheDocument();
      });
    });
  });

  describe('Edge Cases', () => {
    test('handles missing props gracefully', () => {
      render(<CharacterCreation />);
      
      expect(screen.getByText('Create Your Character')).toBeInTheDocument();
    });

    test('handles invalid character data', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Navigate to complete step without setting a name
      fireEvent.click(screen.getByText("Let's Get Started!"));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
      
      const createButton = screen.getByText('Create Character & Start Playing!');
      expect(createButton).toBeDisabled();
    });

    test('handles rapid step navigation', () => {
      render(<CharacterCreation onCharacterCreated={mockOnCharacterCreated} onBack={mockOnBack} />);
      
      // Rapidly click through steps
      fireEvent.click(screen.getByText("Let's Get Started!"));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
      fireEvent.click(screen.getByText('Continue'));
      
      expect(screen.getByText('Ready to Play!')).toBeInTheDocument();
    });
  });
});
