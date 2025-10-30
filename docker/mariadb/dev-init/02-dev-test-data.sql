-- Development Test Data for Tootsville
-- This script adds test data for development environments

USE tootsville;

-- Insert development test users
INSERT IGNORE INTO users (username, email, password_hash, display_name, is_admin, last_login) VALUES
('alice', 'alice@example.com', '$2b$10$dummy.hash.for.docker', 'Alice', FALSE, NOW()),
('bob', 'bob@example.com', '$2b$10$dummy.hash.for.docker', 'Bob', FALSE, NOW()),
('charlie', 'charlie@example.com', '$2b$10$dummy.hash.for.docker', 'Charlie', FALSE, NOW()),
('diana', 'diana@example.com', '$2b$10$dummy.hash.for.docker', 'Diana', TRUE, NOW());

-- Insert development characters
INSERT IGNORE INTO characters (user_id, name, species, position_x, position_y, position_z, is_online) VALUES
(3, 'AliceToot', 'toot', 100.0, 200.0, 0.0, TRUE),
(4, 'BobToot', 'toot', 150.0, 250.0, 0.0, TRUE),
(5, 'CharlieToot', 'toot', 200.0, 300.0, 0.0, FALSE),
(6, 'DianaToot', 'toot', 250.0, 350.0, 0.0, TRUE);

-- Add some test items to characters
INSERT IGNORE INTO character_items (character_id, item_id, quantity, is_equipped) VALUES
(1, 1, 3, FALSE),  -- AdminToot has 3 grow potions
(1, 2, 1, TRUE),   -- AdminToot has shadow caster equipped
(1, 3, 1, FALSE),  -- AdminToot has egg shooter
(2, 1, 5, FALSE),  -- TestCharacter has 5 grow potions
(3, 4, 1, TRUE),   -- AliceToot has magic wand equipped
(4, 2, 1, FALSE),  -- BobToot has shadow caster
(6, 3, 2, FALSE);  -- DianaToot has 2 egg shooters

-- Add some test chat messages
INSERT IGNORE INTO chat_messages (character_id, message, message_type, room_name) VALUES
(1, 'Welcome to Tootsville development server!', 'system', 'world'),
(3, 'Hello everyone! This is Alice.', 'chat', 'world'),
(4, 'Hi Alice! Nice to meet you.', 'chat', 'world'),
(6, 'Greetings, fellow Toots!', 'chat', 'world'),
(3, 'waves happily', 'emote', 'world'),
(4, 'dances around', 'emote', 'world');

-- Add some world events
INSERT IGNORE INTO world_events (event_type, event_data, position_x, position_y, position_z, area_name) VALUES
('player_join', '{"username": "AliceToot"}', 100.0, 200.0, 0.0, 'central-park'),
('player_join', '{"username": "BobToot"}', 150.0, 250.0, 0.0, 'central-park'),
('item_spawn', '{"item_id": 1, "quantity": 1}', 175.0, 225.0, 0.0, 'central-park'),
('weather_change', '{"weather": "sunny", "duration": 3600}', 0.0, 0.0, 0.0, 'world');

-- Add some achievements to test users
INSERT IGNORE INTO character_achievements (character_id, achievement_id) VALUES
(1, 1), (1, 2), (1, 3),  -- AdminToot has multiple achievements
(2, 1), (2, 2),          -- TestCharacter has some achievements
(3, 1),                  -- AliceToot has first login
(6, 1), (6, 2), (6, 4);  -- DianaToot has several achievements

-- Add some game statistics
INSERT IGNORE INTO game_stats (character_id, stat_name, stat_value) VALUES
(1, 'total_logins', 25),
(1, 'chat_messages_sent', 150),
(1, 'items_collected', 20),
(2, 'total_logins', 10),
(2, 'chat_messages_sent', 45),
(3, 'total_logins', 5),
(3, 'chat_messages_sent', 12),
(4, 'total_logins', 8),
(6, 'total_logins', 15),
(6, 'chat_messages_sent', 89),
(6, 'friends_made', 3);

-- Add some friendships
INSERT IGNORE INTO character_relationships (character_id, target_character_id, relationship_type) VALUES
(3, 4, 'friend'),   -- Alice is friends with Bob
(4, 3, 'friend'),   -- Bob is friends with Alice
(3, 6, 'friend'),   -- Alice is friends with Diana
(6, 3, 'friend'),   -- Diana is friends with Alice
(4, 6, 'friend'),   -- Bob is friends with Diana
(6, 4, 'friend');   -- Diana is friends with Bob

-- Create a test area/world data
INSERT IGNORE INTO world_events (event_type, event_data, position_x, position_y, position_z, area_name, character_id) VALUES
('area_created', '{"name": "Central Park", "description": "A beautiful park in the center of Tootsville"}', 0.0, 0.0, 0.0, 'central-park', NULL),
('area_created', '{"name": "Beach", "description": "Sandy beach with ocean views"}', 500.0, 500.0, 0.0, 'beach', NULL),
('area_created', '{"name": "Forest", "description": "Mysterious forest with tall trees"}', -300.0, 200.0, 0.0, 'forest', NULL);

-- Performance test data (large dataset for testing)
DELIMITER $$
CREATE PROCEDURE generate_test_data()
BEGIN
    DECLARE i INT DEFAULT 1;
    WHILE i <= 1000 DO
        INSERT INTO chat_messages (character_id, message, message_type, room_name, position_x, position_y, position_z)
        VALUES (
            FLOOR(RAND() * 4) + 3,  -- Random character_id between 3-6
            CONCAT('Test message number ', i),
            'chat',
            'world',
            RAND() * 1000 - 500,   -- Random position
            RAND() * 1000 - 500,
            0.0
        );
        SET i = i + 1;
    END WHILE;
END$$
DELIMITER ;

-- Uncomment to generate performance test data:
-- CALL generate_test_data();
-- DROP PROCEDURE generate_test_data;

-- Create indexes for better performance
CREATE INDEX idx_chat_messages_timestamp ON chat_messages (created_at);
CREATE INDEX idx_world_events_timestamp ON world_events (created_at);
CREATE INDEX idx_game_stats_value ON game_stats (stat_value);

-- Update permissions
GRANT ALL PRIVILEGES ON tootsville.* TO 'tootsville'@'%';
FLUSH PRIVILEGES;

-- Show summary
SELECT
    (SELECT COUNT(*) FROM users) as users_count,
    (SELECT COUNT(*) FROM characters) as characters_count,
    (SELECT COUNT(*) FROM items) as items_count,
    (SELECT COUNT(*) FROM chat_messages) as messages_count,
    (SELECT COUNT(*) FROM character_items) as inventory_count,
    (SELECT COUNT(*) FROM achievements) as achievements_count;
