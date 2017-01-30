{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Replays where

import qualified Paladin.Handler.Common as Common

getReplaysHandler :: Common.Handler
getReplaysHandler =
  Common.jsonHandler
    Common.replayProxy
    [Common.sql|
      SELECT
        id,
        created_at,
        major_version,
        minor_version,
        recorded_at,
        custom_name,
        duration,
        game_id
      FROM replays
      ORDER BY created_at DESC
    |]
