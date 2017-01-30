{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.Parsers where

import qualified Jalopy.Handler.Common as Common

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
