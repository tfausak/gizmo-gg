-- https://github.com/tfausak/gizmo-gg/issues/53

insert into bodies (id) values (1624) on conflict do nothing;
update bodies set name = 'Endo' where id = 1624;
