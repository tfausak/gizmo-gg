CREATE TABLE games (
  id serial PRIMARY KEY,
  created_at timestamp with time zone NOT NULL DEFAULT now(),
  -- This hash is used to uniquely identify games. It is a combination of many
  -- fields on the replay. It is not possible to replace this with a multi-
  -- column uniqueness constraint because it depends on the players present in
  -- the game, which requires a couple joins.
  hash character(40) UNIQUE NOT NULL,
  game_type_id integer NOT NULL REFERENCES game_types (id),
  playlist_id integer NOT NULL REFERENCES playlists (id),
  -- This is optional because offline games don't have servers.
  server_id integer REFERENCES servers (id),
  -- This is optional because the default game mode ("Soccar") doesn't have an
  -- ID.
  game_mode_id integer REFERENCES game_modes (id),
  -- Number of players per team. For example, a 1 here means a 1v1 match.
  team_size integer NOT NULL,
  -- This is usually true. If it's false, that means the bot difficulty was set
  -- to "unfair" and the team size only represents the size of the team with
  -- the bots on it. In other words, if the team size is 2 and it's not fair,
  -- that means it's a 1v2 match.
  is_fair boolean NOT NULL,
  arena_id integer NOT NULL REFERENCES arenas (id),
  -- The teams aren't necessarily blue and orange, but conceptually it's easiet
  -- to think of them that way.
  blue_score integer NOT NULL,
  orange_score integer NOT NULL
)
