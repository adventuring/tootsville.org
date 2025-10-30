-- Tootsville Database Initialization
-- This script runs when the MariaDB container starts for the first time

-- Create the database (this is handled by docker-compose environment variables)
-- But we'll ensure proper character set

ALTER DATABASE tootsville CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- Use the database
USE tootsville;

-- Users table
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(255) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL,
    display_name VARCHAR(255),
    avatar_url VARCHAR(500),
    is_active BOOLEAN DEFAULT TRUE,
    is_admin BOOLEAN DEFAULT FALSE,
    last_login TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_username (username),
    INDEX idx_email (email),
    INDEX idx_is_active (is_active),
    INDEX idx_last_login (last_login)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- User sessions
CREATE TABLE IF NOT EXISTS user_sessions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    session_token VARCHAR(255) NOT NULL UNIQUE,
    ip_address VARCHAR(45),
    user_agent TEXT,
    expires_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    INDEX idx_user_id (user_id),
    INDEX idx_session_token (session_token),
    INDEX idx_expires_at (expires_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Characters table
CREATE TABLE IF NOT EXISTS characters (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    name VARCHAR(255) NOT NULL,
    species VARCHAR(100) DEFAULT 'toot',
    avatar_data JSON,
    position_x DECIMAL(12,6) DEFAULT 0.0,
    position_y DECIMAL(12,6) DEFAULT 0.0,
    position_z DECIMAL(12,6) DEFAULT 0.0,
    rotation_x DECIMAL(8,6) DEFAULT 0.0,
    rotation_y DECIMAL(8,6) DEFAULT 0.0,
    rotation_z DECIMAL(8,6) DEFAULT 0.0,
    scale_x DECIMAL(6,3) DEFAULT 1.0,
    scale_y DECIMAL(6,3) DEFAULT 1.0,
    scale_z DECIMAL(6,3) DEFAULT 1.0,
    is_online BOOLEAN DEFAULT FALSE,
    last_seen TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    INDEX idx_user_id (user_id),
    INDEX idx_name (name),
    INDEX idx_species (species),
    INDEX idx_is_online (is_online),
    INDEX idx_position (position_x, position_y, position_z)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Items table
CREATE TABLE IF NOT EXISTS items (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    display_name VARCHAR(255) NOT NULL,
    type VARCHAR(100) NOT NULL,
    subtype VARCHAR(100),
    rarity VARCHAR(50) DEFAULT 'common',
    properties JSON,
    description TEXT,
    icon_url VARCHAR(500),
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_type (type),
    INDEX idx_subtype (subtype),
    INDEX idx_rarity (rarity),
    INDEX idx_name (name),
    INDEX idx_is_active (is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Character items (inventory)
CREATE TABLE IF NOT EXISTS character_items (
    id INT AUTO_INCREMENT PRIMARY KEY,
    character_id INT NOT NULL,
    item_id INT NOT NULL,
    quantity INT DEFAULT 1,
    is_equipped BOOLEAN DEFAULT FALSE,
    equipped_slot VARCHAR(100),
    durability INT DEFAULT 100,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    FOREIGN KEY (item_id) REFERENCES items(id) ON DELETE CASCADE,
    INDEX idx_character_id (character_id),
    INDEX idx_item_id (item_id),
    INDEX idx_is_equipped (is_equipped),
    INDEX idx_equipped_slot (equipped_slot),
    UNIQUE KEY unique_character_item_slot (character_id, item_id, equipped_slot)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Chat messages
CREATE TABLE IF NOT EXISTS chat_messages (
    id INT AUTO_INCREMENT PRIMARY KEY,
    character_id INT NOT NULL,
    message TEXT NOT NULL,
    message_type ENUM('chat', 'emote', 'whisper', 'shout', 'system') DEFAULT 'chat',
    recipient_id INT NULL,
    room_name VARCHAR(255) DEFAULT 'world',
    position_x DECIMAL(12,6),
    position_y DECIMAL(12,6),
    position_z DECIMAL(12,6),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    FOREIGN KEY (recipient_id) REFERENCES characters(id) ON DELETE CASCADE,
    INDEX idx_character_id (character_id),
    INDEX idx_recipient_id (recipient_id),
    INDEX idx_message_type (message_type),
    INDEX idx_room_name (room_name),
    INDEX idx_created_at (created_at),
    INDEX idx_position (position_x, position_y, position_z)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- World events
CREATE TABLE IF NOT EXISTS world_events (
    id INT AUTO_INCREMENT PRIMARY KEY,
    event_type VARCHAR(100) NOT NULL,
    event_data JSON,
    position_x DECIMAL(12,6),
    position_y DECIMAL(12,6),
    position_z DECIMAL(12,6),
    area_name VARCHAR(255),
    character_id INT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    INDEX idx_event_type (event_type),
    INDEX idx_area_name (area_name),
    INDEX idx_character_id (character_id),
    INDEX idx_created_at (created_at),
    INDEX idx_position (position_x, position_y, position_z)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Game statistics
CREATE TABLE IF NOT EXISTS game_stats (
    id INT AUTO_INCREMENT PRIMARY KEY,
    character_id INT NOT NULL,
    stat_name VARCHAR(100) NOT NULL,
    stat_value INT DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    INDEX idx_character_id (character_id),
    INDEX idx_stat_name (stat_name),
    UNIQUE KEY unique_character_stat (character_id, stat_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Friends/relationships
CREATE TABLE IF NOT EXISTS character_relationships (
    id INT AUTO_INCREMENT PRIMARY KEY,
    character_id INT NOT NULL,
    target_character_id INT NOT NULL,
    relationship_type ENUM('friend', 'blocked', 'ignored') DEFAULT 'friend',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    FOREIGN KEY (target_character_id) REFERENCES characters(id) ON DELETE CASCADE,
    INDEX idx_character_id (character_id),
    INDEX idx_target_character_id (target_character_id),
    INDEX idx_relationship_type (relationship_type),
    UNIQUE KEY unique_relationship (character_id, target_character_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Achievements
CREATE TABLE IF NOT EXISTS achievements (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE,
    display_name VARCHAR(255) NOT NULL,
    description TEXT,
    icon_url VARCHAR(500),
    points INT DEFAULT 0,
    category VARCHAR(100),
    requirements JSON,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_category (category),
    INDEX idx_is_active (is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Character achievements
CREATE TABLE IF NOT EXISTS character_achievements (
    id INT AUTO_INCREMENT PRIMARY KEY,
    character_id INT NOT NULL,
    achievement_id INT NOT NULL,
    unlocked_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE,
    FOREIGN KEY (achievement_id) REFERENCES achievements(id) ON DELETE CASCADE,
    INDEX idx_character_id (character_id),
    INDEX idx_achievement_id (achievement_id),
    UNIQUE KEY unique_character_achievement (character_id, achievement_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Insert some default data
INSERT IGNORE INTO items (name, display_name, type, rarity, properties, description) VALUES
('grow_potion', 'Grow Potion', 'potion', 'common', '{"effect": "grow", "duration": 300}', 'Makes you grow bigger for a few minutes'),
('shadow_caster', 'Shadow Caster', 'equipment', 'rare', '{"effect": "shadow", "range": 50}', 'Creates shadows around you'),
('katootel_egg_shooter', 'Ka-Tootel Egg Shooter', 'weapon', 'epic', '{"damage": 10, "range": 100}', 'A powerful egg-shooting weapon'),
('magic_wand', 'Magic Wand', 'equipment', 'legendary', '{"effect": "magic", "power": 100}', 'A wand full of magical power');

INSERT IGNORE INTO achievements (name, display_name, description, points, category) VALUES
('first_login', 'Welcome to Tootsville!', 'Successfully logged in for the first time', 10, 'general'),
('first_chat', 'Chatty Toot', 'Sent your first chat message', 15, 'social'),
('item_collector', 'Item Collector', 'Collected 10 different items', 25, 'collection'),
('social_butterfly', 'Social Butterfly', 'Made 5 friends', 30, 'social');

-- Create a default test user and character
INSERT IGNORE INTO users (username, email, password_hash, display_name, is_admin) VALUES
('admin', 'admin@tootsville.org', '$2b$10$dummy.hash.for.docker', 'Administrator', TRUE),
('testuser', 'test@tootsville.org', '$2b$10$dummy.hash.for.docker', 'Test User', FALSE);

INSERT IGNORE INTO characters (user_id, name, species, position_x, position_y, position_z) VALUES
(1, 'AdminToot', 'toot', 100.0, 200.0, 0.0),
(2, 'TestCharacter', 'toot', 150.0, 250.0, 0.0);

-- Grant permissions to the tootsville user
GRANT ALL PRIVILEGES ON tootsville.* TO 'tootsville'@'%';
FLUSH PRIVILEGES;
