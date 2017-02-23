-- First make sure all of the IDs exist.
INSERT INTO bodies (id) VALUES
  (1603),
  (1623)
ON CONFLICT DO NOTHING;

-- Then make sure all the IDs map to a name.
UPDATE bodies SET name = 'Bone Shaker' WHERE id = 1623; -- premium
UPDATE bodies SET name = 'Twin Mill III' WHERE id = 1603; -- premium
