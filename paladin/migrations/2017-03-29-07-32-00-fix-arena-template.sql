-- https://github.com/tfausak/gizmo-gg/issues/68

update arenas
set template_id = (select id from arena_templates where name = 'Standard')
where name = 'eurostadium_night_p';
