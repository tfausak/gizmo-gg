{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Servers where

import qualified Paladin.Handler.Common as Common

getServersHandler :: Common.Handler
getServersHandler =
  Common.jsonHandler
    Common.serverProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM servers
      ORDER BY name ASC
    |]
