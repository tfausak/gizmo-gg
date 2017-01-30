CREATE TABLE replays (
  id uuid PRIMARY KEY,
  created_at timestamp with time zone NOT NULL DEFAULT now(),
  major_version integer NOT NULL,
  minor_version integer NOT NULL,
  recorded_at timestamp without time zone NOT NULL,
  -- The maximum length in game is 35 characters. You might think it should be
  -- 36, but the last byte is always the null byte, which we ignore.
  custom_name character varying(35),
  -- In seconds.
  duration integer NOT NULL,
  game_id integer NOT NULL REFERENCES games (id)
)
