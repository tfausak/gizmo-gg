{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.GameModes where

import qualified Paladin.Handler.Common as Common

getGameModesHandler :: Common.Handler
getGameModesHandler =
  Common.jsonHandler
    Common.gameModeProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM game_modes
      ORDER BY name ASC
    |]
