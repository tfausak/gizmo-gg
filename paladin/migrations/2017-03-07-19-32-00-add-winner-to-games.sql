-- https://github.com/tfausak/gizmo-gg/issues/42

alter table games add blue_win boolean;
update games set blue_win = blue_goals > orange_goals;
alter table games alter blue_win set not null;
