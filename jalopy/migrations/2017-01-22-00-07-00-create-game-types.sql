CREATE TABLE game_types (
  id serial PRIMARY KEY,
  -- These are given in the replays. They're strings like "Offline".
  name character varying(100) UNIQUE NOT NULL
)
