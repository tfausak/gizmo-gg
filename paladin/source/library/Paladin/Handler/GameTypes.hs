{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.GameTypes where

import qualified Paladin.Handler.Common as Common

getGameTypesHandler :: Common.Handler
getGameTypesHandler =
  Common.jsonHandler
    Common.gameTypeProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM game_types
      ORDER BY name ASC
    |]
