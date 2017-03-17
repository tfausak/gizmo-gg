{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Games
  ( getGameHandler
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Text.Read as Read

getGameHandler :: Common.Text -> Common.Handler
getGameHandler rawGameId _config connection _request =
  let notFound = Common.jsonResponse Http.status404 [] Aeson.Null
  in case rawGameId & Text.unpack & Read.readMaybe & fmap Common.Tagged of
    Nothing -> pure notFound
    Just gameId -> do
      maybeGame <- getGame connection gameId
      case maybeGame of
        Nothing -> pure notFound
        Just game -> pure (Common.jsonResponse Http.status200 [] game)

getGame :: Sql.Connection -> Common.GameId -> IO (Maybe Common.Upload)
getGame connection gameId = do
  games <- Database.query connection [Common.sql|
    select
      id,
      created_at,
      hash,
      game_type_id,
      playlist_id,
      server_id,
      game_mode_id,
      team_size,
      is_fair,
      arena_id,
      blue_goals,
      orange_goals,
      played_at,
      duration,
      blue_win
    from games
    where id = ?
  |] [gameId]
  pure (Maybe.listToMaybe games)
