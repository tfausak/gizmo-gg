{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.Platforms where

import qualified Jalopy.Handler.Common as Common

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
