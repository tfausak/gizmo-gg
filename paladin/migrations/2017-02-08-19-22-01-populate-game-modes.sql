-- First make sure all of the IDs exist.
INSERT INTO game_modes (id) VALUES
  (64),
  (128),
  (192)
ON CONFLICT DO NOTHING;

-- Then make sure all the IDs map to a name.
UPDATE game_modes SET name = 'Hoops' WHERE id = 128;
UPDATE game_modes SET name = 'Rumble' WHERE id = 192;
UPDATE game_modes SET name = 'Snow Day' WHERE id = 64;
