CREATE TABLE parse_errors (
  id serial PRIMARY KEY,
  -- Just free-form error messages like "not enough bytes".
  content text UNIQUE NOT NULL
)
