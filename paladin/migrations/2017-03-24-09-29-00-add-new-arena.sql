-- https://github.com/tfausak/gizmo-gg/issues/68

insert into arenas (name)
values ('eurostadium_night_p')
on conflict do nothing;

update arenas
set model_id = (select id from arena_templates where name = 'Standard')
where name = 'eurostadium_night_p';

update arenas
set model_id = (select id from arena_models where name = 'Mannfield')
where name = 'eurostadium_night_p';

update arenas
set skin_id = (select id from arena_skins where name = 'Night')
where name = 'eurostadium_night_p';
