CREATE TABLE players (
  id serial PRIMARY KEY,
  created_at timestamp with time zone NOT NULL DEFAULT now(),
  platform_id integer NOT NULL REFERENCES platforms (id),
  -- Most platforms have integral IDs, but the PlayStation Network uses strings
  -- for IDs. So we have to use strings for everything.
  remote_id character varying(100) NOT NULL,
  local_id integer NOT NULL,
  UNIQUE (platform_id, remote_id, local_id)
)
