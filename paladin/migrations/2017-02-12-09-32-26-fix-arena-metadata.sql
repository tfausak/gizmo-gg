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

UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Cosmic') WHERE name = 'labs_cosmic_v4_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Double Goal') WHERE name = 'labs_doublegoal_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Dunk House') WHERE name = 'HoopsStadium_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Neo Tokyo') WHERE name = 'Neotokyo_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Octagon') WHERE name = 'labs_Octagon_02_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Octagon') WHERE name = 'labs_Octagon_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Pillars') WHERE name = 'labs_circlepillars_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'eurostadium_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'EuroStadium_Rainy_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Park_Night_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'park_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Park_Rainy_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_foggy_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Stadium_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'stadium_winter_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_Dawn_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Trainstation_Night_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_Night_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'trainstation_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'TrainStation_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Underwater_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Dusk_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Dusk_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'utopiastadium_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'Utopiastadium_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Standard') WHERE name = 'UtopiaStadium_Snow_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Starbase ARC') WHERE name = 'ARC_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Underpass') WHERE name = 'labs_underpass_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Utopia Retro') WHERE name = 'labs_utopia_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_Night_P';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_p';
UPDATE arenas SET template_id = (SELECT id FROM arena_templates WHERE name = 'Wasteland') WHERE name = 'Wasteland_P';
