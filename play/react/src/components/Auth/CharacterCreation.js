/**
 * CharacterCreation.js - Character creation component for new users
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides a transparent character creation flow for new users,
 * guided by Parrot Nick and Parrot Jodie through the Tootangan Wish Mist.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useEffect, useRef } from 'react';
import { useGameStore } from '../../stores/GameStore';
import { Colors } from '../../constants/Colors';
import { handleErrorWithParrot } from '../UI/ParrotErrorHandler';

const CharacterCreation = ({ onCharacterCreated, onBack }) => {
  const [step, setStep] = useState(0);
  const [character, setCharacter] = useState({
    name: '',
    baseColor: '',
    padColor: '',
    pattern: '',
    patternColor: '',
    tShirtColor: '',
    isChild: false,
    childCode: '',
    sensitiveP: false
  });
  const [nameValidation, setNameValidation] = useState({
    isValid: false,
    errors: []
  });
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState('');
  
  const gameStore = useGameStore();
  const nameInputRef = useRef(null);

  // Available colors and patterns
  const availableColors = {
    base: ['Cyan', 'Indigo', 'Orange', 'Pink', 'Red', 'Turquoise', 'Violet', 'White', 'Yellow'],
    pad: ['Cyan', 'Indigo', 'Pink', 'Red', 'SpringGreen', 'Violet', 'White', 'Yellow'],
    pattern: ['Black', 'Cyan', 'Indigo', 'Orange', 'Pink', 'Rainbow', 'Turquoise', 'Violet', 'White', 'Yellow'],
    tShirt: ['Cyan', 'Indigo', 'Pink', 'Red', 'SpringGreen', 'Violet', 'White', 'Yellow']
  };

  const availablePatterns = [
    'Flowers', 'Horseshoes', 'Hearts', 'Lightning', 'Patches', 
    'Polka-Dots', 'Notes', 'Sparkles', 'Spots', 'Stars', 'Swirls'
  ];

  // Steps for the character creation flow
  const steps = [
    {
      title: "Welcome to Tootsville!",
      message: "Hi! I'm Parrot Nick, and I'm here to help you create your new Toot character. This will take about a minute, and you'll be able to customize your character's name, colors, and pattern.",
      type: 'welcome'
    },
    {
      title: "Choose Your Character's Name",
      message: "Your character's name must be unique and not offensive to other players. You can use letters and hyphens, with optional numbers at the end.",
      type: 'name',
      examples: ['Flora', 'Null-Pointer-Exception', 'Codfish-Howie']
    },
    {
      title: "Customize Your Character",
      message: "You can customize your character's appearance, or continue with the randomly-generated design. You'll be able to change it later with wish magic!",
      type: 'appearance'
    },
    {
      title: "Account Type",
      message: "Is this character for yourself, or for a child under 13?",
      type: 'account-type'
    },
    {
      title: "Ready to Play!",
      message: "Great! Your character is ready. Parrot Jodie and I will guide you through the Tootangan Wish Mist to make sure you know the rules and understand the controls.",
      type: 'complete'
    }
  ];

  useEffect(() => {
    // Auto-generate random character on mount
    generateRandomCharacter();
  }, []);

  useEffect(() => {
    // Focus name input when on name step
    if (step === 1 && nameInputRef.current) {
      nameInputRef.current.focus();
    }
  }, [step]);

  const generateRandomCharacter = () => {
    const randomColor = (colorArray) => colorArray[Math.floor(Math.random() * colorArray.length)];
    const randomPattern = availablePatterns[Math.floor(Math.random() * availablePatterns.length)];
    
    setCharacter({
      ...character,
      baseColor: randomColor(availableColors.base),
      padColor: randomColor(availableColors.pad),
      pattern: randomPattern,
      patternColor: randomColor(availableColors.pattern),
      tShirtColor: randomColor(availableColors.tShirt)
    });
  };

  const validateName = (name) => {
    const errors = [];
    
    if (name.length < 3 || name.length > 32) {
      errors.push('Name must be 3-32 characters long');
    }
    
    if (name.length === 0 || !/^[a-zA-Z]/.test(name)) {
      errors.push('Name must begin with a letter');
    }
    
    if (!/^[A-Za-z]-?([A-Za-z]+-?)*[A-Za-z]*-?[0-9]?[0-9]?$/.test(name)) {
      errors.push('Name can only contain letters, hyphens, and up to 2 numbers at the end');
    }
    
    if (/[a-z]\1\1/.test(name.toLowerCase())) {
      errors.push('Name cannot have more than 2 of the same letter in a row');
    }
    
    // Check for offensive content
    const offensivePattern = /(fuck|shit|sucker|nigger|nigga|\bfag\b|faggot|bitch|cunt|cunny|retard|penis|vagina|scrotum|testes|testicle|my-?balls|my-?nut|my-?dick|(suck|lick|eat)-?my|\bcock\b|stupid|dyke|faggy)/;
    if (offensivePattern.test(name.toLowerCase())) {
      errors.push('Name cannot contain offensive content');
    }
    
    return {
      isValid: errors.length === 0,
      errors
    };
  };

  const handleNameChange = (e) => {
    const name = e.target.value;
    setCharacter({ ...character, name });
    setNameValidation(validateName(name));
  };

  const handleColorChange = (type, color) => {
    setCharacter({ ...character, [type]: color });
  };

  const handlePatternChange = (pattern) => {
    setCharacter({ ...character, pattern });
  };

  const handleChildCodeChange = (e) => {
    setCharacter({ ...character, childCode: e.target.value });
  };

  const handleSensitiveChange = (e) => {
    setCharacter({ ...character, sensitiveP: e.target.checked });
  };

  const handleNext = () => {
    if (step < steps.length - 1) {
      setStep(step + 1);
    }
  };

  const handleBack = () => {
    if (step > 0) {
      setStep(step - 1);
    } else if (onBack) {
      onBack();
    }
  };

  const handleCreateCharacter = async () => {
    if (!nameValidation.isValid) {
      setError('Please fix the name validation errors before continuing.');
      return;
    }

    if (character.isChild && (!character.childCode || character.childCode.length < 6 || character.childCode.length > 12)) {
      setError('Child accounts must have a secret code between 6 and 12 characters long.');
      return;
    }

    setIsLoading(true);
    setError('');

    try {
      const response = await fetch('/api/toots', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          name: character.name,
          baseColor: character.baseColor,
          padColor: character.padColor,
          pattern: character.pattern,
          patternColor: character.patternColor,
          tShirtColor: character.tShirtColor,
          childP: character.isChild,
          childCode: character.childCode,
          sensitiveP: character.sensitiveP
        })
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Failed to create character');
      }

      const newCharacter = await response.json();
      
      // Update game store
      gameStore.actions.setCharacter(newCharacter);
      
      if (onCharacterCreated) {
        onCharacterCreated(newCharacter);
      }
    } catch (err) {
      setError(err.message);
      handleErrorWithParrot(err);
    } finally {
      setIsLoading(false);
    }
  };

  const renderWelcomeStep = () => (
    <div className="character-creation-step">
      <h2>{steps[step].title}</h2>
      <p>{steps[step].message}</p>
      <div className="character-preview">
        <div className="avatar-preview">
          {/* Avatar preview would go here */}
          <div className="avatar-placeholder">
            <span>Your Character Preview</span>
          </div>
        </div>
      </div>
      <div className="step-actions">
        <button onClick={handleNext} className="btn-primary">
          Let's Get Started!
        </button>
      </div>
    </div>
  );

  const renderNameStep = () => (
    <div className="character-creation-step">
      <h2>{steps[step].title}</h2>
      <p>{steps[step].message}</p>
      
      <div className="name-input-section">
        <label htmlFor="character-name">Character Name:</label>
        <input
          ref={nameInputRef}
          id="character-name"
          type="text"
          value={character.name}
          onChange={handleNameChange}
          placeholder="Enter your character's name"
          className={nameValidation.isValid ? 'valid' : 'invalid'}
        />
        
        {nameValidation.errors.length > 0 && (
          <div className="validation-errors">
            <ul>
              {nameValidation.errors.map((error, index) => (
                <li key={index} className="error">{error}</li>
              ))}
            </ul>
          </div>
        )}
        
        <div className="name-examples">
          <h4>Example Names:</h4>
          <ul>
            {steps[step].examples.map((example, index) => (
              <li key={index}>{example}</li>
            ))}
          </ul>
        </div>
      </div>
      
      <div className="step-actions">
        <button onClick={handleBack} className="btn-secondary">
          Back
        </button>
        <button 
          onClick={handleNext} 
          className="btn-primary"
          disabled={!nameValidation.isValid}
        >
          Continue
        </button>
      </div>
    </div>
  );

  const renderAppearanceStep = () => (
    <div className="character-creation-step">
      <h2>{steps[step].title}</h2>
      <p>{steps[step].message}</p>
      
      <div className="appearance-customization">
        <div className="color-selection">
          <h4>Base Color</h4>
          <div className="color-options">
            {availableColors.base.map(color => (
              <button
                key={color}
                className={`color-option ${character.baseColor === color ? 'selected' : ''}`}
                style={{ backgroundColor: Colors[color] }}
                onClick={() => handleColorChange('baseColor', color)}
                title={color}
              />
            ))}
          </div>
          
          <h4>Pad Color</h4>
          <div className="color-options">
            {availableColors.pad.map(color => (
              <button
                key={color}
                className={`color-option ${character.padColor === color ? 'selected' : ''}`}
                style={{ backgroundColor: Colors[color] }}
                onClick={() => handleColorChange('padColor', color)}
                title={color}
              />
            ))}
          </div>
          
          <h4>Pattern</h4>
          <div className="pattern-options">
            {availablePatterns.map(pattern => (
              <button
                key={pattern}
                className={`pattern-option ${character.pattern === pattern ? 'selected' : ''}`}
                onClick={() => handlePatternChange(pattern)}
              >
                {pattern}
              </button>
            ))}
          </div>
          
          <h4>Pattern Color</h4>
          <div className="color-options">
            {availableColors.pattern.map(color => (
              <button
                key={color}
                className={`color-option ${character.patternColor === color ? 'selected' : ''}`}
                style={{ backgroundColor: Colors[color] }}
                onClick={() => handleColorChange('patternColor', color)}
                title={color}
              />
            ))}
          </div>
          
          <h4>T-Shirt Color</h4>
          <div className="color-options">
            {availableColors.tShirt.map(color => (
              <button
                key={color}
                className={`color-option ${character.tShirtColor === color ? 'selected' : ''}`}
                style={{ backgroundColor: Colors[color] }}
                onClick={() => handleColorChange('tShirtColor', color)}
                title={color}
              />
            ))}
          </div>
        </div>
        
        <div className="character-preview">
          <h4>Preview</h4>
          <div className="avatar-preview">
            {/* Avatar preview would go here */}
            <div className="avatar-placeholder">
              <span>{character.name || 'Your Character'}</span>
            </div>
          </div>
        </div>
      </div>
      
      <div className="step-actions">
        <button onClick={handleBack} className="btn-secondary">
          Back
        </button>
        <button onClick={generateRandomCharacter} className="btn-secondary">
          Randomize
        </button>
        <button onClick={handleNext} className="btn-primary">
          Continue
        </button>
      </div>
    </div>
  );

  const renderAccountTypeStep = () => (
    <div className="character-creation-step">
      <h2>{steps[step].title}</h2>
      <p>{steps[step].message}</p>
      
      <div className="account-type-selection">
        <div className="account-option">
          <label>
            <input
              type="radio"
              name="accountType"
              checked={!character.isChild}
              onChange={() => setCharacter({ ...character, isChild: false })}
            />
            This character is for myself
          </label>
        </div>
        
        <div className="account-option">
          <label>
            <input
              type="radio"
              name="accountType"
              checked={character.isChild}
              onChange={() => setCharacter({ ...character, isChild: true })}
            />
            This character is for a child under 13
          </label>
          
          {character.isChild && (
            <div className="child-code-section">
              <label htmlFor="child-code">
                Secret code for child login (6-12 characters):
              </label>
              <input
                id="child-code"
                type="text"
                value={character.childCode}
                onChange={handleChildCodeChange}
                placeholder="Enter secret code"
                maxLength={12}
              />
              <p className="help-text">
                This code will be used by the child to sign in. Make sure they can remember it!
              </p>
            </div>
          )}
        </div>
        
        <div className="sensitive-player-option">
          <label>
            <input
              type="checkbox"
              checked={character.sensitiveP}
              onChange={handleSensitiveChange}
            />
            I am a sensitive player and would appreciate if others refrain from profanity or adult topics around me
          </label>
        </div>
      </div>
      
      <div className="step-actions">
        <button onClick={handleBack} className="btn-secondary">
          Back
        </button>
        <button onClick={handleNext} className="btn-primary">
          Continue
        </button>
      </div>
    </div>
  );

  const renderCompleteStep = () => (
    <div className="character-creation-step">
      <h2>{steps[step].title}</h2>
      <p>{steps[step].message}</p>
      
      <div className="character-summary">
        <h4>Your Character Summary:</h4>
        <ul>
          <li><strong>Name:</strong> {character.name}</li>
          <li><strong>Base Color:</strong> {character.baseColor}</li>
          <li><strong>Pad Color:</strong> {character.padColor}</li>
          <li><strong>Pattern:</strong> {character.pattern}</li>
          <li><strong>Pattern Color:</strong> {character.patternColor}</li>
          <li><strong>T-Shirt Color:</strong> {character.tShirtColor}</li>
          <li><strong>Account Type:</strong> {character.isChild ? 'Child Account' : 'Adult Account'}</li>
          {character.sensitiveP && <li><strong>Special:</strong> Sensitive Player</li>}
        </ul>
      </div>
      
      {error && (
        <div className="error-message">
          {error}
        </div>
      )}
      
      <div className="step-actions">
        <button onClick={handleBack} className="btn-secondary">
          Back
        </button>
        <button 
          onClick={handleCreateCharacter} 
          className="btn-primary"
          disabled={isLoading || !nameValidation.isValid}
        >
          {isLoading ? 'Creating Character...' : 'Create Character & Start Playing!'}
        </button>
      </div>
    </div>
  );

  const renderStep = () => {
    switch (steps[step].type) {
      case 'welcome':
        return renderWelcomeStep();
      case 'name':
        return renderNameStep();
      case 'appearance':
        return renderAppearanceStep();
      case 'account-type':
        return renderAccountTypeStep();
      case 'complete':
        return renderCompleteStep();
      default:
        return <div>Unknown step</div>;
    }
  };

  return (
    <div className="character-creation">
      <div className="character-creation-header">
        <h1>Create Your Character</h1>
        <div className="step-indicator">
          Step {step + 1} of {steps.length}
        </div>
      </div>
      
      {renderStep()}
    </div>
  );
};

export default CharacterCreation;
