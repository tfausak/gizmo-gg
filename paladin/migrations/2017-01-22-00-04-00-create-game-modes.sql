CREATE TABLE game_modes (
  -- This is an `integer` primary key rather than a `serial` because the game
  -- mode ID is replicated.
  id integer PRIMARY KEY,
  -- Game mode names (like "Hoops") are not given in the replays. This column
  -- must be manually filled in, which is why it is nullable. Also note that
  -- the default game mode ("Soccar") has no ID and is not replicated.
  name character varying(100) UNIQUE
)
