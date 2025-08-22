/**
 * Colors.js - Canonical color palette for Tootsville
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Defines the canonical color palette used throughout the application.
 * All colors should be accessed through this object to ensure consistency.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

/**
 * Canonical color palette for Tootsville
 * @type {Object.<string, string>}
 */
export const Colors = {
  // Canonical Tootsville palette colors
  Silver: '#ddd',
  Charcoal: '#333',
  White: '#fff',
  Black: '#000',
  
  DeepPurple: '#b117ff',
  Yellow: '#fff216',
  Pink: '#e73e97',
  Cyan: '#ccffff',
  Turquoise: '#00a290',
  Periwinkle: '#96b4de',
  
  Violet: '#9669ad',
  Gold: '#f7d023',
  Burgundy: '#9c0059',
  Green: '#7ac142',
  Blue: '#0082c8',
  
  Lavender: '#ba9dca',
  Tan: '#ffd2a0',
  Red: '#e51b24',
  SpringGreen: '#c4d82d',
  Indigo: '#0028ff',
  
  Orange: '#ff7b26',
  
  // Special colors
  TranslucentBlack: '#0008',
  TranslucentWhite: '#fff8',
  Transparent: 'transparent',
  CurrentColor: 'currentColor'
};

/**
 * Get a color by name
 * @param {string} colorName - The canonical name of the color
 * @returns {string} The hex color value
 * @throws {Error} If color name is not found
 */
export const getColor = (colorName) => {
  if (!(colorName in Colors)) {
    throw new Error(`Color '${colorName}' not found in palette`);
  }
  return Colors[colorName];
};

/**
 * Check if a color name exists in the palette
 * @param {string} colorName - The color name to check
 * @returns {boolean} True if the color exists
 */
export const hasColor = (colorName) => {
  return colorName in Colors;
};

/**
 * Get all available color names
 * @returns {string[]} Array of all color names
 */
export const getColorNames = () => {
  return Object.keys(Colors);
};

export default Colors;
