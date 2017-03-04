-- https://github.com/tfausak/gizmo-gg/issues/28

create index on games (blue_goals);
create index on games (orange_goals);
create index on games (played_at);
create index on games (playlist_id);

create index on games_players (is_blue);
create index on games_players (is_present_at_end);
create index on games_players (name);
create index on games_players (player_id);

create index on players (local_id);
create index on players (platform_id);
create index on players (remote_id);

create index on uploads (created_at);
create index on uploads (started_parsing_at);
