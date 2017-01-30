CREATE TABLE arenas (
  id serial PRIMARY KEY,
  -- These are internal map names like "ARC_P". They often have different
  -- capitalizations.
  name character varying(100) UNIQUE NOT NULL
)
