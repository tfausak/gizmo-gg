-- Undo accidental inserts from last migration.
DELETE FROM game_modes WHERE id IN (
  3,
  4,
  15,
  17,
  18,
  10,
  11,
  12,
  13
);

-- First make sure all of the IDs exist.
INSERT INTO playlists (id) VALUES
  (1),
  (2),
  (3),
  (4),
  (15),
  (17),
  (18),
  (10),
  (11),
  (12),
  (13)
ON CONFLICT DO NOTHING;

-- Then make sure all the IDs map to a name.
UPDATE playlists SET name = 'Duel' WHERE id = 1;
UPDATE playlists SET name = 'Doubles' WHERE id = 2;
UPDATE playlists SET name = 'Standard' WHERE id = 3;
UPDATE playlists SET name = 'Chaos' WHERE id = 4;
UPDATE playlists SET name = 'Snow Day' WHERE id = 15;
UPDATE playlists SET name = 'Hoops' WHERE id = 17;
UPDATE playlists SET name = 'Rumble' WHERE id = 18;
UPDATE playlists SET name = 'Competitive Solo Duel' WHERE id = 10;
UPDATE playlists SET name = 'Competitive Doubles' WHERE id = 11;
UPDATE playlists SET name = 'Competitive Solo Standard' WHERE id = 12;
UPDATE playlists SET name = 'Competitive Standard' WHERE id = 13;
