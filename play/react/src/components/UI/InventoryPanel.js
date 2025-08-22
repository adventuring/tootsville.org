/**
 * InventoryPanel.js - Player inventory management component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides inventory management functionality including item viewing,
 * equipping, using, and organizing items.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState } from 'react';
import { useGameStore } from '../../stores/GameStore';
import { Colors } from '../../constants/Colors';
import './InventoryPanel.css';

/**
 * InventoryPanel component for displaying and managing player inventory
 * 
 * @component
 * @returns {JSX.Element} Inventory panel interface
 */
const InventoryPanel = () => {
  const { inventory = [] } = useGameStore();
  const [isOpen, setIsOpen] = useState(false);
  const [filter, setFilter] = useState('all');
  const [selectedItem, setSelectedItem] = useState(null);

  const togglePanel = () => {
    setIsOpen(!isOpen);
    if (isOpen) {
      setSelectedItem(null);
    }
  };

  const handleItemClick = (item) => {
    setSelectedItem(selectedItem?.id === item.id ? null : item);
  };

  const handleUseItem = (item) => {
    // TODO: Implement item usage
    console.log('Using item:', item);
  };

  const handleDropItem = (item) => {
    // TODO: Implement item dropping
    console.log('Dropping item:', item);
  };

  const filteredItems = inventory.filter(item => {
    if (filter === 'all') return true;
    return item.type === filter;
  });
  
  const getItemIcon = (item) => {
    // Simple item icon based on type
    const icons = {
      clothing: '👕',
      accessory: '💍',
      toy: '🧸',
      food: '🍎',
      tool: '🔧',
      collectible: '⭐',
      misc: '📦'
    };
    return icons[item.type] || '📦';
  };
  
  const getItemRarityColor = (rarity) => {
    const colors = {
      common: Colors.White,
      uncommon: Colors.Green,
      rare: Colors.Blue,
      epic: Colors.Violet,
      legendary: Colors.Orange
    };
    return colors[rarity] || Colors.White;
  };
  
  return (
    <div className="inventory-panel">
      {/* Inventory Toggle Button */}
      <button 
        className="inventory-toggle"
        onClick={togglePanel}
        title={isOpen ? 'Close Inventory' : 'Open Inventory'}
      >
        🎒
        {inventory.length > 0 && (
          <span className="inventory-count">{inventory.length}</span>
        )}
      </button>
      
      {/* Inventory Window */}
      {isOpen && (
        <div className="inventory-window">
          <div className="inventory-header">
            <h3>Inventory</h3>
            <div className="inventory-controls">
              <select 
                value={filter} 
                onChange={(e) => setFilter(e.target.value)}
                className="inventory-filter"
              >
                <option value="all">All Items</option>
                <option value="clothing">Clothing</option>
                <option value="accessory">Accessories</option>
                <option value="toy">Toys</option>
                <option value="food">Food</option>
                <option value="tool">Tools</option>
                <option value="collectible">Collectibles</option>
                <option value="misc">Misc</option>
              </select>
              <button 
                className="inventory-close"
                onClick={togglePanel}
                title="Close Inventory"
              >
                ×
              </button>
            </div>
          </div>
          
          <div className="inventory-content">
            <div className="inventory-grid">
              {filteredItems.length === 0 ? (
                <div className="no-items">
                  No items found.
                </div>
              ) : (
                filteredItems.map(item => (
                  <div 
                    key={item.id}
                    className={`inventory-item ${selectedItem?.id === item.id ? 'selected' : ''}`}
                    onClick={() => handleItemClick(item)}
                    style={{ borderColor: getItemRarityColor(item.rarity) }}
                  >
                    <div className="item-icon">{getItemIcon(item)}</div>
                    <div className="item-name">{item.name}</div>
                    {item.quantity > 1 && (
                      <div className="item-quantity">{item.quantity}</div>
                    )}
                  </div>
                ))
              )}
            </div>
            
            {selectedItem && (
              <div className="item-details">
                <div className="item-header">
                  <span className="item-icon-large">{getItemIcon(selectedItem)}</span>
                  <div className="item-info">
                    <h4 
                      className="item-name-large"
                      style={{ color: getItemRarityColor(selectedItem.rarity) }}
                    >
                      {selectedItem.name}
                    </h4>
                    <span className="item-type">{selectedItem.type}</span>
                  </div>
                </div>
                
                <div className="item-description">
                  {selectedItem.description}
                </div>
                
                {selectedItem.stats && (
                  <div className="item-stats">
                    {Object.entries(selectedItem.stats).map(([stat, value]) => (
                      <div key={stat} className="stat-item">
                        <span className="stat-name">{stat}:</span>
                        <span className="stat-value">{value}</span>
                      </div>
                    ))}
                  </div>
                )}
                
                <div className="item-actions">
                  {(selectedItem.type === 'weapon' || selectedItem.type === 'armor') && (
                    <button 
                      className="action-button equip-button"
                      onClick={handleEquip}
                    >
                      Equip
                    </button>
                  )}
                  
                  {selectedItem.type === 'consumable' && (
                    <button 
                      className="action-button use-button"
                      onClick={handleUse}
                    >
                      Use
                    </button>
                  )}
                  
                  <button 
                    className="action-button drop-button"
                    onClick={handleDrop}
                  >
                    Drop
                  </button>
                </div>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
};

export default InventoryPanel;



