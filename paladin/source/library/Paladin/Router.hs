{-# LANGUAGE OverloadedStrings #-}

module Paladin.Router where

import qualified Network.Wai as Wai
import qualified Paladin.Handler as Handler

route :: Wai.Request -> Handler.Handler
route request =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", ["arenas"]) -> Handler.getArenasHandler
    ("GET", ["parsers"]) -> Handler.getParsersHandler
    ("GET", ["platforms"]) -> Handler.getPlatformsHandler
    ("GET", ["players"]) -> Handler.getPlayersHandler
    ("GET", ["playlists"]) -> Handler.getPlaylistsHandler
    ("GET", ["replays"]) -> Handler.getReplaysHandler
    ("GET", ["search"]) -> Handler.getSearchHandler
    ("GET", ["servers"]) -> Handler.getServersHandler
    ("GET", ["stats", "arenas"]) -> Handler.getStatsArenasHandler
    ("GET", ["stats", "bodies"]) -> Handler.getStatsBodiesHandler
    ("GET", ["stats", "players", playerId]) ->
      Handler.getStatsPlayersHandler playerId
    ("GET", ["stats", "players", playerId, "arenas"]) ->
      Handler.getStatsPlayersArenasHandler playerId
    ("GET", ["stats", "players", playerId, "bodies"]) ->
      Handler.getStatsPlayersBodiesHandler playerId
    ("GET", ["stats", "players", playerId, "new"]) ->
      Handler.getNewStatsPlayersHandler playerId
    ("GET", ["stats", "summary"]) -> Handler.getStatsSummaryHandler
    ("GET", ["uploads"]) -> Handler.getUploadsHandler
    ("POST", ["uploads"]) -> Handler.postUploadHandler
    ("GET", ["uploads", uploadId]) -> Handler.getUploadHandler uploadId
    _ -> Handler.notFoundHandler
