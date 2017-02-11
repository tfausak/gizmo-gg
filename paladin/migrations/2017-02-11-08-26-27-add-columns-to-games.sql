-- Add columns to games.
ALTER TABLE games
ADD played_at timestamp without time zone,
ADD duration integer;

-- Populate columns with values from replays.
UPDATE games
SET
  played_at = replays.recorded_at,
  duration = replays.duration
FROM replays
WHERE replays.game_id = games.id;

-- Make the columns required.
ALTER TABLE games
ALTER played_at SET NOT NULL,
ALTER duration SET NOT NULL;
