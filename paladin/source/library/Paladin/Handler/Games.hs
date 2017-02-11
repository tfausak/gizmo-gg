{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Games where

import qualified Paladin.Handler.Common as Common

getGamesHandler :: Common.Handler
getGamesHandler =
  Common.jsonHandler
    Common.gameProxy
    [Common.sql|
      SELECT
        id,
        created_at,
        hash,
        game_type_id,
        playlist_id,
        server_id,
        game_mode_id,
        team_size,
        is_fair,
        arena_id,
        blue_goals,
        orange_goals,
        played_at,
        duration
      FROM games
      ORDER BY created_at DESC
   |]
