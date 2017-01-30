{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.Arenas where

import qualified Jalopy.Handler.Common as Common

getArenasHandler :: Common.Handler
getArenasHandler =
  Common.jsonHandler
    Common.arenaProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM arenas
      ORDER BY name ASC
    |]
