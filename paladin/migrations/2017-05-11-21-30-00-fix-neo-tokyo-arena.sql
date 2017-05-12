update arenas
set template_id = (select id from arena_templates where name = 'Standard')
where name = 'NeoTokyo_Standard_P';

update arenas
set model_id = (select id from arena_models where name = 'Neo Tokyo')
where name = 'NeoTokyo_Standard_P';
