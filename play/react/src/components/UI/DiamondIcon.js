/**
 * DiamondIcon.js - Diamond icon component for child and sensitive players
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Displays diamond icons to indicate child accounts (black diamond) and 
 * sensitive players (white diamond) according to Tootsville's community guidelines.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React from 'react';
import PropTypes from 'prop-types';

/**
 * DiamondIcon component for displaying player status indicators
 * 
 * @param {Object} props - Component props
 * @param {boolean} props.isChild - Whether the player is under 13
 * @param {boolean} props.isSensitive - Whether the player is a sensitive player
 * @param {string} props.size - Size of the diamond icon ('small', 'medium', 'large')
 * @param {string} props.className - Additional CSS classes
 * @param {string} props.title - Tooltip text
 * @returns {JSX.Element|null} Diamond icon or null if no indicators needed
 */
const DiamondIcon = ({ 
  isChild = false, 
  isSensitive = false, 
  size = 'medium', 
  className = '', 
  title = '' 
}) => {
  // Don't render anything if no indicators are needed
  if (!isChild && !isSensitive) {
    return null;
  }

  const getDiamondSymbol = () => {
    if (isChild) {
      return '◆'; // Black diamond for children under 13
    }
    if (isSensitive) {
      return '◇'; // White diamond for sensitive players
    }
    return null;
  };

  const getDiamondColor = () => {
    if (isChild) {
      return '#000'; // Black for children
    }
    if (isSensitive) {
      return '#fff'; // White for sensitive players
    }
    return '#000';
  };

  const getSizeClass = () => {
    switch (size) {
      case 'small':
        return 'diamond-small';
      case 'large':
        return 'diamond-large';
      default:
        return 'diamond-medium';
    }
  };

  const getTooltipText = () => {
    if (title) {
      return title;
    }
    
    if (isChild) {
      return 'This player is under 13. Please be mindful of your language and topics around children.';
    }
    if (isSensitive) {
      return 'This is a sensitive player who would appreciate if you refrain from profanity or adult topics.';
    }
    return '';
  };

  const diamondSymbol = getDiamondSymbol();
  if (!diamondSymbol) {
    return null;
  }

  return (
    <span
      className={`diamond-icon ${getSizeClass()} ${className}`}
      style={{ color: getDiamondColor() }}
      title={getTooltipText()}
      role="img"
      aria-label={isChild ? 'Child player indicator' : 'Sensitive player indicator'}
    >
      {diamondSymbol}
    </span>
  );
};

DiamondIcon.propTypes = {
  isChild: PropTypes.bool,
  isSensitive: PropTypes.bool,
  size: PropTypes.oneOf(['small', 'medium', 'large']),
  className: PropTypes.string,
  title: PropTypes.string
};

export default DiamondIcon;
