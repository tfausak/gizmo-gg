-- https://github.com/tfausak/gizmo-gg/issues/41

update arenas set name = 'gizmo-' || name;
update arenas set name = substring(name, 7);
