{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Search
  ( getSearchHandler
  ) where

import Control.Category ((>>>))

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common

getSearchHandler :: Common.Handler
getSearchHandler _config connection request = do
  let query = Wai.queryString request
  let name = getName query
  let pattern = makePattern name
  let platforms = getPlatforms query
  results <-
    Database.query
      connection
      [Common.sql|
        SELECT DISTINCT ON (players.id)
          players.id,
          players.platform_id,
          platforms.name,
          players.remote_id,
          players.local_id,
          games_players.name,
          games_players.xp,
          games.played_at
        FROM games_players
        INNER JOIN games ON games.id = games_players.game_id
        INNER JOIN players on players.id = games_players.player_id
        INNER JOIN platforms ON platforms.id = players.platform_id
        WHERE games_players.name ILIKE ? AND platforms.name IN ?
        LIMIT 20
      |]
      (pattern, Common.In platforms)
  let body = Aeson.toJSON (results :: [SearchResult])
  pure (Common.jsonResponse Http.status200 [] body)

data SearchResult = SearchResult
  { _searchResultId :: Int
  , _searchResultPlatformId :: Int
  , _searchResultPlatformName :: Common.Text
  , _searchResultRemoteId :: Common.Text
  , _searchResultLocalId :: Int
  , _searchResultName :: Common.Text
  , _searchResultXp :: Int
  , _searchResultLastSeen :: Common.LocalTime
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow SearchResult

instance Common.ToJSON SearchResult where
  toJSON = Common.genericToJSON "_SearchResult"

getName :: Common.Query -> String
getName query = Maybe.fromMaybe "" (Common.getParam "name" query)

makePattern :: String -> Common.Text
makePattern
  = Text.pack
  >>> likeEscape
  >>> wrap "%" "%"

likeEscape :: Common.Text -> Common.Text
likeEscape
  = Text.replace "\\" "\\\\"
  >>> Text.replace "_" "\\_"
  >>> Text.replace "%" "\\%"

wrap :: Common.Text -> Common.Text -> Common.Text -> Common.Text
wrap l r x = Text.concat [l, x, r]

getPlatforms :: Common.Query -> [Common.PlatformName]
getPlatforms query =
  case Common.getParam "platform" query of
    Just "playstation" -> [Common.PlayStation]
    Just "steam" -> [Common.Steam]
    Just "xbox" -> [Common.Xbox]
    _ -> [minBound .. maxBound]
