{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Parsers where

import qualified Paladin.Handler.Common as Common

getParsersHandler :: Common.Handler
getParsersHandler =
  Common.jsonHandler
    Common.parserProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM parsers
      ORDER BY name ASC
    |]
