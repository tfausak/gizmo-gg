-- First make sure all of the IDs exist.
INSERT INTO paints (id) VALUES
  (2),
  (4),
  (6),
  (8),
  (10),
  (12),
  (14),
  (16),
  (18),
  (20),
  (22),
  (24),
  (26)
ON CONFLICT DO NOTHING;

-- Then make sure all the IDs map to a name.
UPDATE paints SET name = 'Crimson' WHERE id = 2;
UPDATE paints SET name = 'Lime' WHERE id = 4;
UPDATE paints SET name = 'Black' WHERE id = 6;
UPDATE paints SET name = 'Sky Blue' WHERE id = 8;
UPDATE paints SET name = 'Cobalt' WHERE id = 10;
UPDATE paints SET name = 'Burnt Sienna' WHERE id = 12;
UPDATE paints SET name = 'Forest Green' WHERE id = 14;
UPDATE paints SET name = 'Purple' WHERE id = 16;
UPDATE paints SET name = 'Pink' WHERE id = 18;
UPDATE paints SET name = 'Orange' WHERE id = 20;
UPDATE paints SET name = 'Grey' WHERE id = 22;
UPDATE paints SET name = 'Titanium White' WHERE id = 24;
UPDATE paints SET name = 'Saffron' WHERE id = 26;
