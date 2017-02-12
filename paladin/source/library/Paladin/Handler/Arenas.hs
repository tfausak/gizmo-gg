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
        arena_templates.id,
        arena_templates.name,
        arena_models.id,
        arena_models.name,
        arena_skins.id,
        arena_skins.name
      FROM arenas
      LEFT OUTER JOIN arena_templates ON arena_templates.id = arenas.template_id
      LEFT OUTER JOIN arena_models ON arena_models.id = arenas.model_id
      LEFT OUTER JOIN arena_skins ON arena_skins.id = arenas.skin_id
      ORDER BY arenas.name ASC
    |]
