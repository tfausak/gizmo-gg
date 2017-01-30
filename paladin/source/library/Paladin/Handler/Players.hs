{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Players where

import qualified Paladin.Handler.Common as Common

getPlayersHandler :: Common.Handler
getPlayersHandler =
  Common.jsonHandler
    Common.playerProxy
    [Common.sql|
      SELECT
        id,
        created_at,
        platform_id,
        remote_id,
        local_id
      FROM players
      ORDER BY created_at DESC
    |]
