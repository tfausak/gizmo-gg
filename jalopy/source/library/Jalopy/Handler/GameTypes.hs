{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.GameTypes where

import qualified Jalopy.Handler.Common as Common

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
