{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.GameModes where

import qualified Jalopy.Handler.Common as Common

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
