-- https://github.com/tfausak/gizmo-gg/issues/78

insert into bodies (id) values (1691) on conflict do nothing;
update bodies set name = 'Mantis' where id = 1691;
