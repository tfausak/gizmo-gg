-- https://github.com/tfausak/gizmo-gg/issues/89

-- arenas

insert into arenas (name) values
  ('cs_p'),
  ('EuroStadium_Night_P'),
  ('EuroStadium_P'),
  ('Park_P'),
  ('Stadium_Foggy_P'),
  ('Stadium_P'),
  ('Underwater_P'),
  ('UtopiaStadium_Dusk_P'),
  ('UtopiaStadium_P')
  on conflict do nothing;

-- templates

update arenas
set template_id = (select id from arena_templates where name = 'Standard')
where name in
  ( 'cs_p'
  , 'EuroStadium_Night_P'
  , 'EuroStadium_P'
  , 'Park_P'
  , 'Stadium_Foggy_P'
  , 'Stadium_P'
  , 'Underwater_P'
  , 'UtopiaStadium_Dusk_P'
  , 'UtopiaStadium_P'
  );

-- models

insert into arena_models (name)
values ('Champions Field')
on conflict do nothing;

update arenas
set model_id = (select id from arena_models where name = 'Champions Field')
where name = 'cs_p';

update arenas
set model_id = (select id from arena_models where name = 'Mannfield')
where name in ('EuroStadium_Night_P', 'EuroStadium_P');

update arenas
set model_id = (select id from arena_models where name = 'Beckwith Park')
where name = 'Park_P';

update arenas
set model_id = (select id from arena_models where name = 'DFH Stadium')
where name in ('Stadium_Foggy_P', 'Stadium_P');

update arenas
set model_id = (select id from arena_models where name = 'Aquadome')
where name = 'Underwater_P';

update arenas
set model_id = (select id from arena_models where name = 'Utopia Coliseum')
where name in ('UtopiaStadium_Dusk_P', 'UtopiaStadium_P');

-- skins

update arenas
set skin_id = (select id from arena_skins where name = 'Night')
where name = 'EuroStadium_Night_P';

update arenas
set skin_id = (select id from arena_skins where name = 'Stormy')
where name = 'Stadium_Foggy_P';

update arenas
set skin_id = (select id from arena_skins where name = 'Dusk')
where name = 'UtopiaStadium_Dusk_P';
