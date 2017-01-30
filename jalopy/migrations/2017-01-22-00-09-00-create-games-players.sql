CREATE TABLE games_players (
  id serial PRIMARY KEY,
  game_id integer NOT NULL REFERENCES games (id),
  player_id integer NOT NULL REFERENCES players (id),
  UNIQUE (game_id, player_id)
)
