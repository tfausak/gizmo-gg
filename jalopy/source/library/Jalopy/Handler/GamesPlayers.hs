{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.GamesPlayers where

import qualified Jalopy.Handler.Common as Common

getGamesPlayersHandler :: Common.Handler
getGamesPlayersHandler =
  Common.jsonHandler
    Common.gamePlayerProxy
    [Common.sql|
      SELECT
        id,
        game_id,
        player_id
      FROM games_players
      ORDER BY id DESC
  |]
