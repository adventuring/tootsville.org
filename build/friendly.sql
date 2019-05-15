USE makesomenoise;

CREATE TABLE IF NOT EXISTS people
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  display_name VARCHAR(125) NOT NULL DEFAULT 'Player',
  given_name VARCHAR(50) NOT NULL DEFAULT 'Player',
  surname VARCHAR(50) NOT NULL DEFAULT 'Player',
  date_of_birth DATE NULL,
  age TINYINT UNSIGNED NULL,
  sensitivep ENUM('Y','N') NOT NULL DEFAULT 'N',
  child_code CHAR(6) NULL,
  gender ENUM('M','F','X') NOT NULL DEFAULT 'X',
  lang CHAR(6) NOT NULL DEFAULT 'en_US' );

CREATE TABLE IF NOT EXISTS parent_child
( parent BINARY(16) NOT NULL,
  child BINARY(16) NOT NULL,
  CONSTRAINT child_parent 
     FOREIGN KEY (parent) REFERENCES people(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  CONSTRAINT parent_child
     FOREIGN KEY (child) REFERENCES people(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  rel ENUM('PARENT','GRANDPARENT','UNCLE',
           'TEACHER','BABYSITTER','GUARDIAN',
           'CAREGIVER','FRIEND','PAR-OR-GUA')
      NOT NULL DEFAULT 'PAR-OR-GUA',
  primaryp ENUM('Y','N') NOT NULL DEFAULT 'Y' );

CREATE TABLE IF NOT EXISTS credentials
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  person BINARY(16) NOT NULL,
  CONSTRAINT credentialed_person
     FOREIGN KEY (person) REFERENCES people(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  provider VARCHAR(50) NOT NULL,
  id_token VARCHAR(200) NOT NULL,
  UNIQUE KEY provider_id_token (provider, id_token),
  auth_token VARCHAR(1000) NULL,
  refresh_token VARCHAR(1000) NULL,
  json_info VARCHAR(4096) NULL );

CREATE TABLE IF NOT EXISTS person_links
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  person BINARY(16) NOT NULL,
  CONSTRAINT person_linked
     FOREIGN KEY (person) REFERENCES people(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  rel ENUM('CONTACT','ALT-CONTACT','PHOTO','PROFILE','BLOG','OTHER')
      NOT NULL DEFAULT 'OTHER',
  verifiedp ENUM('Y','N') NOT NULL DEFAULT 'N',
  url VARCHAR(1000) NOT NULL,
  label VARCHAR(50) NULL,
  provenance VARCHAR(100) NOT NULL );

CREATE TABLE IF NOT EXISTS logins
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  person BINARY(16) NOT NULL,
  CONSTRAINT login_person
     FOREIGN KEY (person) REFERENCES people(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  credentials BINARY(16) NOT NULL, 
  CONSTRAINT login_credentials
     FOREIGN KEY (credentials) REFERENCES credentials(uuid)
     ON DELETE CASCADE
     ON UPDATE CASCADE,
  start TIMESTAMP NOT NULL,
  renewed TIMESTAMP NULL,
  last_seen TIMESTAMP NOT NULL,
  origin VARCHAR(50) NOT NULL );

CREATE TABLE IF NOT EXISTS patterns
( id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name VARCHAR(25) NOT NULL UNIQUE KEY );

CREATE TABLE IF NOT EXISTS toots
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  name VARCHAR(50) NULL,
  pattern INT NOT NULL DEFAULT 1,
  CONSTRAINT toot_pattern
     FOREIGN KEY (pattern) REFERENCES patterns(id)
     ON DELETE DEFAULT
     ON UPDATE CASCADE,
  base_color BINARY(3) NOT NULL DEFAULT b'111111111111111111111111',
  pattern_color BINARY(3) NOT NULL DEFAULT 0,
  pad_color BINARY(3) NOT NULL DEFAULT 255,
  avatar VARCHAR(30) NOT NULL DEFAULT 'UltraToot',
  player BINARY(16) NULL,
  CONSTRAINT toot_player
     FOREIGN KEY (player) REFERENCES people(uuid)
     ON DELETE SET NULL
     ON UPDATE CASCADE,
  last_active TIMESTAMP NULL,
  INDEX (name),
  INDEX (last_active));

CREATE TABLE IF NOT EXISTS wear_slots
( id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name VARCHAR(20) NOT NULL UNIQUE KEY,
  alternate INT NULL,
  avatar_point VARCHAR(8) NOT NULL,
  valence TINYINT UNSIGNED NOT NULL,
  obstruct_point VARCHAR(8) NULL,
  obstruct_min TINYINT UNSIGNED NULL,
  obstruct_max TINYINT UNSIGNED NULL );

INSERT INTO wear_slots (name, avatar_point, valence)
VALUES ('Pivitz', 'PIVITZ', 0),
       ('Hat', 'HEAD', 30),
       ('Headscarf', 'HEAD', 20),
       ('Hair', 'HEAD', 10),
       ('Undershirt', 'TORSO', 10),
       ('Shirt', 'TORSO', 20),
       ('Vest', 'TORSO', 30),
       ('Blazer', 'TORSO', 40),
       ('Jacket', 'TORSO', 50),
       ('Coat', 'TORSO', 60),
       ('Necklace', 'NECK', 10),
       ('Trunk', 'TRUNK', 10),
       ('Shorts', 'LEGS', 10),
       ('Slacks', 'LEGS', 20),
       ('Overpants', 'LEGS', 30);

CREATE TABLE IF NOT EXISTS avatar_slots
( id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  avatar VARCHAR(50) NOT NULL,
  slot CHAR(8) NOT NULL,
  valence INT NOT NULL,
  CONSTRAINT unique_avatar_slots
     UNIQUE KEY (avatar, slot, valence) );

CREATE TABLE IF NOT EXISTS item_templates
( id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name VARCHAR(50) NOT NULL UNIQUE KEY,
  default_base_color BINARY(3) NOT NULL DEFAULT b'111111111111111111111111',
  avatar VARCHAR(50) NOT NULL,
  energy_kind ENUM('COUNTABLE','UNCOUNTABLE') NULL,
  energy_max INTEGER UNSIGNED NULL,
  on_zero ENUM('VANISH','EMPTY') NOT NULL DEFAULT 'VANISH',
  wear_slot INT NULL,
  CONSTRAINT item_wearable
     FOREIGN KEY (wear_slot) REFERENCES wear_slots(id)
     ON DELETE SET NULL
     ON UPDATE CASCADE,
  weight INTEGER UNSIGNED NOT NULL DEFAULT 100 );

CREATE TABLE IF NOT EXISTS items
( uuid BINARY(16) NOT NULL PRIMARY KEY,
  base_color BINARY(3) NOT NULL DEFAULT b'111111111111111111111111',
  template INT NOT NULL,
  CONSTRAINT item_template
     FOREIGN KEY (template) REFERENCES item_templates(id)
     ON DELETE SET NULL
     ON UPDATE CASCADE,
  energy INTEGER UNSIGNED NULL );


CREATE TABLE IF NOT EXISTS inventory
( person BINARY(16) NOT NULL,
  CONSTRAINT inventory_person
     FOREIGN KEY (person) REFERENCES people(uuid)
     ON DELETE DEFAULT
     ON UPDATE CASCADE,
  toot BINARY(16) NULL,
  CONSTRAINT inventory_toot
     FOREIGN KEY (toot) REFERENCES toots(uuid)
     ON DELETE DEFAULT
     ON UPDATE CASCADE,
  item BINARY(16) NOT NULL,
  CONSTRAINT inventory_item
     FOREIGN KEY (item) REFERENCES item(uuid)
     ON DELETE DEFAULT
     ON UPDATE CASCADE,
  equipped ENUM('Y','A','N') );
