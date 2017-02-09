-- First make sure all of the IDs exist.
INSERT INTO bodies (id) VALUES
  (21),
  (22),
  (23),
  (24),
  (25),
  (26),
  (27),
  (28),
  (29),
  (30),
  (31),
  (402),
  (403),
  (404),
  (523),
  (597),
  (600),
  (607),
  (625),
  (723),
  (803),
  (1018),
  (1159),
  (1171),
  (1172),
  (1286),
  (1295),
  (1300),
  (1317),
  (1416),
  (1475),
  (1478),
  (1533),
  (1568)
ON CONFLICT DO NOTHING;

-- Then make sure all the IDs map to a name.
UPDATE bodies SET name = 'Aftershock' WHERE id = 1286; -- premium
UPDATE bodies SET name = 'Armadillo' WHERE id = 625; -- xbox
UPDATE bodies SET name = 'Backfire' WHERE id = 21;
UPDATE bodies SET name = 'Batmobile' WHERE id = 803; -- premium
UPDATE bodies SET name = 'Breakout' WHERE id = 22;
UPDATE bodies SET name = 'Breakout Type-S' WHERE id = 1416; -- import
UPDATE bodies SET name = 'DeLorean Time Machine' WHERE id = 597; -- premium
UPDATE bodies SET name = 'Dominus' WHERE id = 403; -- premium
UPDATE bodies SET name = 'Dominus GT' WHERE id = 1018; -- import
UPDATE bodies SET name = 'Esper' WHERE id = 1317; -- premium
UPDATE bodies SET name = 'Gizmo' WHERE id = 26;
UPDATE bodies SET name = 'Grog' WHERE id = 607; -- premium
UPDATE bodies SET name = 'Hogsticker' WHERE id = 723; -- xbox
UPDATE bodies SET name = 'Hotshot' WHERE id = 29;
UPDATE bodies SET name = 'Marauder' WHERE id = 1172; -- premium
UPDATE bodies SET name = 'Masamune' WHERE id = 1171; -- premium
UPDATE bodies SET name = 'Merc' WHERE id = 30;
UPDATE bodies SET name = 'Octane' WHERE id = 23;
UPDATE bodies SET name = 'Octane ZSR' WHERE id = 1568; -- import
UPDATE bodies SET name = 'Paladin' WHERE id = 24;
UPDATE bodies SET name = 'Proteus' WHERE id = 1475; -- premium
UPDATE bodies SET name = 'Ripper' WHERE id = 600; -- premium
UPDATE bodies SET name = 'Road Hog' WHERE id = 25;
UPDATE bodies SET name = 'Road Hog XL' WHERE id = 1300; -- import
UPDATE bodies SET name = 'Scarab' WHERE id = 404; -- premium
UPDATE bodies SET name = 'Sweet Tooth' WHERE id = 27;
UPDATE bodies SET name = 'Takumi' WHERE id = 402; -- premium
UPDATE bodies SET name = 'Takumi RX-T' WHERE id = 1295; -- import
UPDATE bodies SET name = 'Triton' WHERE id = 1478; -- premium
UPDATE bodies SET name = 'Venom' WHERE id = 31;
UPDATE bodies SET name = 'Vulcan' WHERE id = 1533; -- premium
UPDATE bodies SET name = 'X-Devil' WHERE id = 28;
UPDATE bodies SET name = 'X-Devil Mk2' WHERE id = 1159; -- import
UPDATE bodies SET name = 'Zippy' WHERE id = 523;
