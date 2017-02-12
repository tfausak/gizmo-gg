{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Arenas where

import qualified Paladin.Handler.Common as Common

getArenasHandler :: Common.Handler
getArenasHandler =
  Common.jsonHandler
    Common.arenaProxy
    [Common.sql|
      SELECT
        arenas.id,
        arenas.name,
        arenas.template_id,
        arenas.model_id,
        arenas.skin_id
      FROM arenas
      ORDER BY arenas.name ASC
    |]
