drop table player_skills;

create table player_skills (
  id serial primary key,
  created_at timestamp with time zone not null default now(),
  player_id integer not null references players (id),
  playlist_id integer not null references playlists (id),
  matches_played integer not null,
  division integer not null,
  tier integer not null,
  mmr double precision not null,
  mu double precision not null,
  sigma double precision not null
);
