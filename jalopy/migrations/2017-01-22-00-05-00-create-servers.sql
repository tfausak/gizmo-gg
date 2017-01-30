CREATE TABLE servers (
  -- This is an `integer` primary key rather than a `serial` because the server
  -- ID is replicated.
  id integer PRIMARY KEY,
  name character varying(100) UNIQUE NOT NULL
)
