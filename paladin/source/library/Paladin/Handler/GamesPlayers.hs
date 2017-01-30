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
        player_id
      FROM games_players
      ORDER BY id DESC
  |]
