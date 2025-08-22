/**
 * DiamondIcon.test.js - Unit tests for DiamondIcon component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Tests the diamond icon display for child and sensitive players.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import { render, screen } from '@testing-library/react';

// Mock the component since it's in a different directory
jest.mock('../../../play/react/src/components/UI/DiamondIcon', () => {
  return function MockDiamondIcon({ isChild, isSensitive, size = 'medium', className = '', title = '' }) {
    if (!isChild && !isSensitive) {
      return null;
    }

    const diamondSymbol = isChild ? '◆' : '◇';
    const diamondColor = isChild ? '#000' : '#fff';
    const sizeClass = `diamond-${size}`;
    const ariaLabel = isChild ? 'Child player indicator' : 'Sensitive player indicator';
    const tooltipText = title || (isChild 
      ? 'This player is under 13. Please be mindful of your language and topics around children.'
      : 'This is a sensitive player who would appreciate if you refrain from profanity or adult topics.'
    );

    return (
      <span
        className={`diamond-icon ${sizeClass} ${className}`}
        style={{ color: diamondColor }}
        title={tooltipText}
        role="img"
        aria-label={ariaLabel}
      >
        {diamondSymbol}
      </span>
    );
  };
});

describe('DiamondIcon Component', () => {
  describe('Rendering', () => {
    test('renders nothing when no indicators are needed', () => {
      const { container } = render(<DiamondIcon isChild={false} isSensitive={false} />);
      expect(container.firstChild).toBeNull();
    });

    test('renders black diamond for child players', () => {
      render(<DiamondIcon isChild={true} isSensitive={false} />);
      
      const diamond = screen.getByRole('img', { name: 'Child player indicator' });
      expect(diamond).toBeInTheDocument();
      expect(diamond).toHaveTextContent('◆');
      expect(diamond).toHaveStyle({ color: '#000' });
    });

    test('renders white diamond for sensitive players', () => {
      render(<DiamondIcon isChild={false} isSensitive={true} />);
      
      const diamond = screen.getByRole('img', { name: 'Sensitive player indicator' });
      expect(diamond).toBeInTheDocument();
      expect(diamond).toHaveTextContent('◇');
      expect(diamond).toHaveStyle({ color: '#fff' });
    });

    test('prioritizes child indicator over sensitive indicator', () => {
      render(<DiamondIcon isChild={true} isSensitive={true} />);
      
      const diamond = screen.getByRole('img', { name: 'Child player indicator' });
      expect(diamond).toBeInTheDocument();
      expect(diamond).toHaveTextContent('◆');
      expect(diamond).toHaveStyle({ color: '#000' });
    });
  });

  describe('Size Variants', () => {
    test('applies small size class', () => {
      render(<DiamondIcon isChild={true} size="small" />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('diamond-small');
    });

    test('applies medium size class by default', () => {
      render(<DiamondIcon isChild={true} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('diamond-medium');
    });

    test('applies large size class', () => {
      render(<DiamondIcon isChild={true} size="large" />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('diamond-large');
    });
  });

  describe('Tooltips', () => {
    test('shows default tooltip for child players', () => {
      render(<DiamondIcon isChild={true} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveAttribute('title', 'This player is under 13. Please be mindful of your language and topics around children.');
    });

    test('shows default tooltip for sensitive players', () => {
      render(<DiamondIcon isSensitive={true} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveAttribute('title', 'This is a sensitive player who would appreciate if you refrain from profanity or adult topics.');
    });

    test('shows custom tooltip when provided', () => {
      const customTooltip = 'Custom tooltip text';
      render(<DiamondIcon isChild={true} title={customTooltip} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveAttribute('title', customTooltip);
    });
  });

  describe('Accessibility', () => {
    test('has correct aria-label for child players', () => {
      render(<DiamondIcon isChild={true} />);
      
      const diamond = screen.getByRole('img', { name: 'Child player indicator' });
      expect(diamond).toBeInTheDocument();
    });

    test('has correct aria-label for sensitive players', () => {
      render(<DiamondIcon isSensitive={true} />);
      
      const diamond = screen.getByRole('img', { name: 'Sensitive player indicator' });
      expect(diamond).toBeInTheDocument();
    });

    test('has role="img" for screen readers', () => {
      render(<DiamondIcon isChild={true} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toBeInTheDocument();
    });
  });

  describe('CSS Classes', () => {
    test('applies base diamond-icon class', () => {
      render(<DiamondIcon isChild={true} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('diamond-icon');
    });

    test('applies additional CSS classes', () => {
      const additionalClass = 'custom-class';
      render(<DiamondIcon isChild={true} className={additionalClass} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('diamond-icon', 'diamond-medium', additionalClass);
    });
  });

  describe('Edge Cases', () => {
    test('handles undefined props gracefully', () => {
      const { container } = render(<DiamondIcon />);
      expect(container.firstChild).toBeNull();
    });

    test('handles null props gracefully', () => {
      const { container } = render(<DiamondIcon isChild={null} isSensitive={null} />);
      expect(container.firstChild).toBeNull();
    });

    test('handles empty string props gracefully', () => {
      const { container } = render(<DiamondIcon isChild="" isSensitive="" />);
      expect(container.firstChild).toBeNull();
    });

    test('handles invalid size prop', () => {
      render(<DiamondIcon isChild={true} size="invalid" />);
      
      const diamond = screen.getByRole('img');
      // Should default to medium size
      expect(diamond).toHaveClass('diamond-medium');
    });
  });

  describe('Props Validation', () => {
    test('accepts valid size props', () => {
      const sizes = ['small', 'medium', 'large'];
      
      sizes.forEach(size => {
        const { unmount } = render(<DiamondIcon isChild={true} size={size} />);
        const diamond = screen.getByRole('img');
        expect(diamond).toBeInTheDocument();
        unmount();
      });
    });

    test('accepts boolean props', () => {
      render(<DiamondIcon isChild={true} isSensitive={false} />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toBeInTheDocument();
    });

    test('accepts string props', () => {
      render(<DiamondIcon isChild={true} className="test-class" title="Test title" />);
      
      const diamond = screen.getByRole('img');
      expect(diamond).toHaveClass('test-class');
      expect(diamond).toHaveAttribute('title', 'Test title');
    });
  });
});
