/**
 * ChatSystem.js - In-game chat system component
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 * 
 * Provides in-game chat functionality including text chat,
 * voice chat indicators, and message history.
 * 
 * @author Interworldly Adventuring, LLC
 * @version 1.0.0
 */

import React, { useState, useRef, useEffect } from 'react';
import { useGameStore } from '../../stores/GameStore';
import './ChatSystem.css';

/**
 * ChatSystem component for in-game communication
 * 
 * @component
 * @returns {JSX.Element} Chat system interface
 */
const ChatSystem = () => {
  const { messages = [], sendMessage, isConnected } = useGameStore();
  const [inputMessage, setInputMessage] = useState('');
  const [isChatOpen, setIsChatOpen] = useState(false);
  const [chatType, setChatType] = useState('global'); // global, local, private
  const messagesEndRef = useRef(null);
  
  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };
  
  useEffect(() => {
    scrollToBottom();
  }, [messages]);
  
  const handleSendMessage = (e) => {
    e.preventDefault();
    if (inputMessage.trim() && isConnected) {
      sendMessage(inputMessage, chatType);
      setInputMessage('');
    }
  };
  
  const handleKeyPress = (e) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      handleSendMessage(e);
    }
  };
  
  const toggleChat = () => {
    setIsChatOpen(!isChatOpen);
  };
  
  const formatMessage = (message) => {
    const timestamp = new Date(message.timestamp).toLocaleTimeString();
    return (
      <div key={message.id} className={`chat-message ${message.type}`}>
        <span className="message-timestamp">{timestamp}</span>
        <span className="message-sender">{message.sender}:</span>
        <span className="message-content">{message.content}</span>
      </div>
    );
  };
  
  return (
    <div className="chat-system">
      {/* Chat Toggle Button */}
      <button 
        className="chat-toggle"
        onClick={toggleChat}
        title={isChatOpen ? 'Close Chat' : 'Open Chat'}
      >
        💬
        {messages.length > 0 && (
          <span className="chat-notification">{messages.length}</span>
        )}
      </button>
      
      {/* Chat Window */}
      {isChatOpen && (
        <div className="chat-window">
          <div className="chat-header">
            <h3>Chat</h3>
            <div className="chat-controls">
              <select 
                value={chatType} 
                onChange={(e) => setChatType(e.target.value)}
                className="chat-type-selector"
              >
                <option value="global">Global</option>
                <option value="local">Local</option>
                <option value="private">Private</option>
              </select>
              <button 
                className="chat-close"
                onClick={toggleChat}
                title="Close Chat"
              >
                ×
              </button>
            </div>
          </div>
          
          <div className="chat-messages">
            {messages.length === 0 ? (
              <div className="no-messages">
                No messages yet. Start chatting!
              </div>
            ) : (
              messages.map(formatMessage)
            )}
            <div ref={messagesEndRef} />
          </div>
          
          <form className="chat-input-form" onSubmit={handleSendMessage}>
            <input
              type="text"
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              onKeyPress={handleKeyPress}
              placeholder={`Type a ${chatType} message...`}
              className="chat-input"
              disabled={!isConnected}
            />
            <button 
              type="submit" 
              className="chat-send"
              disabled={!inputMessage.trim() || !isConnected}
            >
              Send
            </button>
          </form>
        </div>
      )}
    </div>
  );
};

export default ChatSystem;



