-- https://github.com/tfausak/gizmo-gg/issues/78

insert into arenas (name)
values ('NeoTokyo_Standard_P')
on conflict do nothing;

update arenas
set model_id = (select id from arena_templates where name = 'Standard')
where name = 'NeoTokyo_Standard_P';

update arenas
set model_id = (select id from arena_models where name = 'Neo Tokyo')
where name = 'NeoTokyo_Standard_P';
