-- https://github.com/tfausak/gizmo-gg/issues/41
--   id  |         name         | skin_id | model_id | template_id
-- ------+----------------------+---------+----------+-------------
--  8516 | EuroStadium_Rainy_P  |         |          |
--    44 | EuroStadium_Rainy_P  |       6 |        7 |           7
--  8515 | UtopiaStadium_Dusk_p |         |          |
--    56 | UtopiaStadium_Dusk_p |       2 |       14 |           7

update games set arena_id = 44 where arena_id = 8516;
update games set arena_id = 56 where arena_id = 8515;

delete from arenas where id in (8515, 8516);
