{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Platforms where

import qualified Paladin.Handler.Common as Common

getPlatformsHandler :: Common.Handler
getPlatformsHandler =
  Common.jsonHandler
    Common.platformProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM platforms
      ORDER BY name ASC
    |]
