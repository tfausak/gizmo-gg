{-# LANGUAGE OverloadedStrings #-}

module Paladin.Router where

import qualified Network.Wai as Wai
import qualified Paladin.Handler as Handler

route :: Wai.Request -> Handler.Handler
route request =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", ["arenas"]) -> Handler.getArenasHandler
    ("GET", ["games", gameId]) -> Handler.getGameHandler gameId
    ("GET", ["search"]) -> Handler.getSearchHandler
    ("GET", ["stats", "arenas"]) -> Handler.getStatsArenasHandler
    ("GET", ["stats", "bodies"]) -> Handler.getStatsBodiesHandler
    ("GET", ["stats", "players", playerId]) ->
      Handler.getStatsPlayersHandler playerId
    ("GET", ["stats", "players", playerId, "arenas"]) ->
      Handler.getStatsPlayersArenasHandler playerId
    ("GET", ["stats", "players", playerId, "bodies"]) ->
      Handler.getStatsPlayersBodiesHandler playerId
    ("GET", ["stats", "players", playerId, "history"]) ->
      Handler.getStatsPlayersHistoryHandler playerId
    ("GET", ["stats", "players", playerId, "poll"]) ->
      Handler.getStatsPlayersPollHandler playerId
    ("GET", ["stats", "players", playerId, "rank"]) ->
      Handler.getStatsPlayersRankHandler playerId
    ("GET", ["stats", "summary"]) -> Handler.getStatsSummaryHandler
    ("POST", ["uploads"]) -> Handler.postUploadHandler
    ("GET", ["uploads", uploadId]) -> Handler.getUploadHandler uploadId
    _ -> Handler.notFoundHandler
