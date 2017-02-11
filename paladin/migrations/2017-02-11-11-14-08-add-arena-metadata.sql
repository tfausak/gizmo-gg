-- Arena: A specific map you can play on, like "stadium_winter_p".
-- Skin: An optional variant on a map, like "Snowy".
-- Model: The thing that skins are applied to, like "DFH Stadium".
-- Template: The overall shape of a map, like "Standard".

INSERT INTO arenas (name) VALUES
  ('ARC_P'),
  ('eurostadium_p'),
  ('EuroStadium_Rainy_P'),
  ('HoopsStadium_P'),
  ('labs_circlepillars_p'),
  ('labs_cosmic_p'),
  ('labs_cosmic_v4_p'),
  ('labs_doublegoal_p'),
  ('labs_Octagon_02_P'),
  ('labs_Octagon_P'),
  ('labs_underpass_p'),
  ('labs_utopia_p'),
  ('Neotokyo_p'),
  ('Park_Night_P'),
  ('park_p'),
  ('Park_Rainy_P'),
  ('stadium_foggy_p'),
  ('stadium_p'),
  ('Stadium_p'),
  ('stadium_winter_p'),
  ('TrainStation_Dawn_P'),
  ('Trainstation_Night_P'),
  ('TrainStation_Night_P'),
  ('trainstation_p'),
  ('TrainStation_P'),
  ('Underwater_p'),
  ('UtopiaStadium_Dusk_p'),
  ('UtopiaStadium_Dusk_P'),
  ('utopiastadium_p'),
  ('Utopiastadium_p'),
  ('UtopiaStadium_P'),
  ('UtopiaStadium_Snow_p'),
  ('Wasteland_Night_P'),
  ('Wasteland_p'),
  ('Wasteland_P')
ON CONFLICT DO NOTHING;

--

CREATE TABLE arena_skins (
  id serial PRIMARY KEY,
  name character varying(100) UNIQUE NOT NULL
);

INSERT INTO arena_skins (name) VALUES
  ('Dawn'),
  ('Dusk'),
  ('Midnight'),
  ('Night'),
  ('Snowy'),
  ('Stormy');

ALTER TABLE arenas
ADD skin_id integer REFERENCES arena_skins (id);

UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Dawn') WHERE name = 'TrainStation_Dawn_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Dusk') WHERE name = 'UtopiaStadium_Dusk_p';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Dusk') WHERE name = 'UtopiaStadium_Dusk_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Midnight') WHERE name = 'Park_Night_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Night') WHERE name = 'Trainstation_Night_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Night') WHERE name = 'TrainStation_Night_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Night') WHERE name = 'Wasteland_Night_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Snowy') WHERE name = 'stadium_winter_p';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Snowy') WHERE name = 'UtopiaStadium_Snow_p';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Stormy') WHERE name = 'EuroStadium_Rainy_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Stormy') WHERE name = 'Park_Rainy_P';
UPDATE arenas SET skin_id = (SELECT id FROM arena_skins WHERE name = 'Stormy') WHERE name = 'stadium_foggy_p';

--

CREATE TABLE arena_models (
  id serial PRIMARY KEY,
  name character varying(100) UNIQUE NOT NULL
);

INSERT INTO arena_models (name) VALUES
  ('Aquadome'),
  ('Beckwith Park'),
  ('Cosmic'),
  ('DFH Stadium'),
  ('Double Goal'),
  ('Dunk House'),
  ('Mannfield'),
  ('Neo Tokyo'),
  ('Octagon'),
  ('Pillars'),
  ('Starbase ARC'),
  ('Underpass'),
  ('Urban Central'),
  ('Utopia Coliseum'),
  ('Utopia Retro'),
  ('Wasteland');

ALTER TABLE arenas
ADD model_id integer REFERENCES arena_models (id);

UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Aquadome') WHERE name = 'Underwater_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Beckwith Park') WHERE name = 'Park_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Beckwith Park') WHERE name = 'park_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Beckwith Park') WHERE name = 'Park_Rainy_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_v4_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'DFH Stadium') WHERE name = 'stadium_foggy_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'DFH Stadium') WHERE name = 'stadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'DFH Stadium') WHERE name = 'Stadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'DFH Stadium') WHERE name = 'stadium_winter_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Double Goal') WHERE name = 'labs_doublegoal_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Dunk House') WHERE name = 'HoopsStadium_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Mannfield') WHERE name = 'eurostadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Mannfield') WHERE name = 'EuroStadium_Rainy_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Neo Tokyo') WHERE name = 'Neotokyo_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Octagon') WHERE name = 'labs_Octagon_02_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Octagon') WHERE name = 'labs_Octagon_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Pillars') WHERE name = 'labs_circlepillars_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Starbase ARC') WHERE name = 'ARC_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Underpass') WHERE name = 'labs_underpass_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Urban Central') WHERE name = 'TrainStation_Dawn_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Urban Central') WHERE name = 'Trainstation_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Urban Central') WHERE name = 'TrainStation_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Urban Central') WHERE name = 'trainstation_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Urban Central') WHERE name = 'TrainStation_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'UtopiaStadium_Dusk_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'UtopiaStadium_Dusk_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'utopiastadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'Utopiastadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'UtopiaStadium_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Coliseum') WHERE name = 'UtopiaStadium_Snow_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Utopia Retro') WHERE name = 'labs_utopia_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Wasteland') WHERE name = 'Wasteland_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Wasteland') WHERE name = 'Wasteland_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_models WHERE name = 'Wasteland') WHERE name = 'Wasteland_P';

--

CREATE TABLE arena_templates (
  id serial PRIMARY KEY,
  name character varying(100) UNIQUE NOT NULL
);

INSERT INTO arena_templates (name) VALUES
  ('Cosmic'),
  ('Double Goal'),
  ('Dunk House'),
  ('Neo Tokyo'),
  ('Octagon'),
  ('Pillars'),
  ('Standard'),
  ('Starbase ARC'),
  ('Underpass'),
  ('Utopia Retro'),
  ('Wasteland');

ALTER TABLE arenas
ADD template_id integer REFERENCES arena_templates (id);

UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_v4_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Double Goal') WHERE name = 'labs_doublegoal_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Dunk House') WHERE name = 'HoopsStadium_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Neo Tokyo') WHERE name = 'Neotokyo_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Octagon') WHERE name = 'labs_Octagon_02_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Octagon') WHERE name = 'labs_Octagon_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Pillars') WHERE name = 'labs_circlepillars_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'eurostadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'EuroStadium_Rainy_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Park_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'park_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Park_Rainy_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_foggy_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Stadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_winter_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_Dawn_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Trainstation_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'trainstation_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Underwater_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Dusk_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Dusk_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'utopiastadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Utopiastadium_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Snow_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Starbase ARC') WHERE name = 'ARC_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Underpass') WHERE name = 'labs_underpass_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Utopia Retro') WHERE name = 'labs_utopia_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_Night_P';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_p';
UPDATE arenas SET model_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_P';
