CREATE TABLE playlists (
  -- This is an `integer` primary key rather than a `serial` because the
  -- playlist ID is replicated.
  id integer PRIMARY KEY,
  -- Playlist names (like "Competitive solo duel") are not given in the
  -- replays. This column must be manually filled in, which is why it is
  -- nullable.
  name character varying(100) UNIQUE
)
