CREATE TABLE parsers (
  id serial PRIMARY KEY,
  -- A way to uniquely identifier parsers like "rattletrap-2.1.5".
  name text UNIQUE NOT NULL
)
