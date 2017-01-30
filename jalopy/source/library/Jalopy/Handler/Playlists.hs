{-# LANGUAGE QuasiQuotes #-}

module Jalopy.Handler.Playlists where

import qualified Jalopy.Handler.Common as Common

getPlaylistsHandler :: Common.Handler
getPlaylistsHandler =
  Common.jsonHandler
    Common.playlistProxy
    [Common.sql|
      SELECT
        id,
        name
      FROM playlists
      ORDER BY name ASC
   |]
