CREATE TABLE platforms (
  id serial PRIMARY KEY,
  -- These will be constant strings like "steam".
  name character varying(100) UNIQUE NOT NULL
)
