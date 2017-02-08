{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.GamesPlayers where

import qualified Paladin.Handler.Common as Common

getGamesPlayersHandler :: Common.Handler
getGamesPlayersHandler =
  Common.jsonHandler
    Common.gamePlayerProxy
    [Common.sql|
      SELECT
        id,
        game_id,
        player_id,
        name,
        xp,
        is_blue,
        is_present_at_end,
        score,
        goals,
        assists,
        saves,
        shots,
        body_id,
        decal_id,
        wheel_id,
        rocket_trail_id,
        antenna_id,
        topper_id,
        wheel_paint_id,
        topper_paint_id,
        primary_color_id,
        accent_color_id,
        primary_finish_id,
        accent_finish_id,
        fov,
        height,
        angle,
        distance,
        stiffness,
        swivel_speed
      FROM games_players
      ORDER BY id DESC
  |]
