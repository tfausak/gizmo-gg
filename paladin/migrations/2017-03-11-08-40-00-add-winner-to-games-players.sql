-- https://github.com/tfausak/gizmo-gg/issues/42

alter table games_players add did_win boolean;

update games_players
set
  did_win =
    case when games_players.is_blue
    then games.blue_win
    else not games.blue_win
    end
from games
where games.id = games_players.game_id;

alter table games_players alter did_win set not null;
