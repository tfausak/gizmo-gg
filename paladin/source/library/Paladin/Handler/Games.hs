{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Games
  ( getGameHandler
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Paladin.Handler.Stats as Stats
import qualified Text.Read as Read

getGameHandler :: Common.Text -> Common.Handler
getGameHandler rawGameId _config connection _request =
  let notFound = Common.jsonResponse Http.status404 [] Aeson.Null
  in case rawGameId & Text.unpack & Read.readMaybe & fmap Common.Tagged of
    Nothing -> pure notFound
    Just gameId -> do
      maybePlayerGameRow <- getPlayerGameRow connection gameId
      case maybePlayerGameRow of
        Nothing -> pure notFound
        Just playerGameRow -> do
          gamePlayerRows <- Stats.getGamesPlayers connection [Common.tagValue gameId]
          let maybePlayerOutput = Stats.makePlayerOutput
                Nothing
                [ ( Text.pack ""
                  , Time.LocalTime (Time.fromGregorian 1970 1 1) Time.midnight
                  )
                ]
                [playerGameRow]
                gamePlayerRows
          let body = Aeson.toJSON maybePlayerOutput
          pure (Common.jsonResponse Http.status200 [] body)

getPlayerGameRow
  :: Sql.Connection
  -> Common.GameId
  -> IO (Maybe Stats.PlayerGameRow)
getPlayerGameRow connection gameId = do
  playerGameRows <- Database.query connection [Common.sql|
    select
      games.id,
      playlists.id,
      playlists.name,
      arenas.id,
      arenas.name,
      arena_skins.id,
      arena_skins.name,
      arena_models.id,
      arena_models.name,
      arena_templates.id,
      arena_templates.name,
      games.played_at,
      games.duration,
      games.blue_goals,
      games.orange_goals
    from games
    inner join arenas on arenas.id = games.arena_id
    left outer join arena_skins on arena_skins.id = arenas.skin_id
    inner join arena_models on arena_models.id = arenas.model_id
    inner join arena_templates on arena_templates.id = arenas.template_id
    inner join playlists on playlists.id = games.playlist_id
    where games.id = ?
  |] [gameId]
  pure (Maybe.listToMaybe playerGameRows)
