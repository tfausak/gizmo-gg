-- https://github.com/tfausak/gizmo-gg/issues/53

insert into bodies (id) values (1675) on conflict do nothing;
update bodies set name = 'Ice Charger' where id = 1675;
