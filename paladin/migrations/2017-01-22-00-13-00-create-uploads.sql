CREATE TABLE uploads (
  id serial PRIMARY KEY,
  created_at timestamp with time zone NOT NULL DEFAULT now(),
  -- The original file name.
  name character varying(100) NOT NULL,
  -- The file's size in bytes.
  size integer NOT NULL,
  -- This is the SHA1 hash of the file's contents.
  hash character(40) UNIQUE NOT NULL,
  -- This is set when a worker pulls the upload off the queue for parsing.
  started_parsing_at timestamp with time zone,
  -- The parser ID is set when parsing starts. That way if parsing fails we'll
  -- know which parser we tried to use.
  parser_id integer REFERENCES parsers (id),
  -- This is set when parsing finishes, regardless of whether or not it
  -- succeeded.
  finished_parsing_at timestamp with time zone,
  -- Parse errors are only set if some part of parsing the replay failed.
  parse_error_id integer REFERENCES parse_errors (id),
  -- The replay UUID is only set if parsing the replay succeeded.
  replay_id uuid REFERENCES replays (id)
)
