{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.ParseErrors where

import qualified Jalopy.Handler.Common as Common

getParseErrorsHandler :: Common.Handler
getParseErrorsHandler =
  Common.jsonHandler
    Common.parseErrorProxy
    [Common.sql|
      SELECT
        id,
        content
      FROM parse_errors
      ORDER BY content ASC
    |]
