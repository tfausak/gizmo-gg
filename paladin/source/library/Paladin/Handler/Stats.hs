{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Text.Read as Read

getStatsPlayersHandler :: Common.Text -> Common.Handler
getStatsPlayersHandler rawPlayerId _config connection request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
      let query = Wai.queryString request
      day <- getDay query
      let playlists = getPlaylists query
      [[numBlueGames, numOrangeGames, numBlueWins, numOrangeWins, totalScore, totalGoals, totalAssists, totalSaves, totalShots, secondsPlayed]] <-
        Database.query
          connection
          [Common.sql|
            SELECT
              count(CASE WHEN games_players.is_blue THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue THEN 1 END),
              count(CASE WHEN games_players.is_blue AND games.blue_score > games.orange_score THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue and games.orange_score > games.blue_score THEN 1 END),
              sum(games_players.score),
              sum(games_players.goals),
              sum(games_players.assists),
              sum(games_players.saves),
              sum(games_players.shots),
              sum(games.duration)
            FROM games
            INNER JOIN games_players
              ON games_players.game_id = games.id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ?
          |]
          (playerId, day, Common.In playlists)
      games <-
        Database.query
          connection
          [Common.sql|
            SELECT
              games.playlist_id,
              playlists.name,
              games.played_at,
              games_players.is_blue AND games.blue_score > games.orange_score,
              CASE WHEN games_players.is_blue THEN games.blue_score ELSE games.orange_score END,
              CASE WHEN games_players.is_blue THEN games.orange_score ELSE games.blue_score END,
              games.duration,
              games_players.body_id,
              bodies.name,
              games_players.score,
              games_players.goals,
              games_players.assists,
              games_players.saves,
              games_players.shots,
              games.arena_id,
              arenas.name
            FROM games
            INNER JOIN games_players ON
              games_players.game_id = games.id
            INNER JOIN playlists ON
              playlists.id = games.playlist_id
            INNER JOIN bodies ON
              bodies.id = games_players.body_id
            INNER JOIN arenas ON
              arenas.id = games.arena_id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ?
            ORDER BY
              games.played_at DESC
            LIMIT 20
          |]
          (playerId, day, Common.In playlists)
      let numGames = numBlueGames + numOrangeGames
      let numWins = numBlueWins + numOrangeWins
      let body =
            Aeson.object
              [ (Text.pack "numGames", Aeson.toJSON numGames)
              , (Text.pack "numWins", Aeson.toJSON numWins)
              , (Text.pack "numLosses", Aeson.toJSON (numGames - numWins))
              , (Text.pack "winPct", Aeson.toJSON (makeRatio numWins numGames))
              , (Text.pack "numBlueGames", Aeson.toJSON numBlueGames)
              , (Text.pack "numBlueWins", Aeson.toJSON numBlueWins)
              , ( Text.pack "numBlueLosses"
                , Aeson.toJSON (numBlueGames - numBlueWins))
              , ( Text.pack "blueWinPct"
                , Aeson.toJSON (makeRatio numBlueWins numBlueGames))
              , (Text.pack "numOrangeWins", Aeson.toJSON numOrangeWins)
              , ( Text.pack "numOrangeLosses"
                , Aeson.toJSON (numOrangeGames - numOrangeWins))
              , ( Text.pack "orangeWinPct"
                , Aeson.toJSON (makeRatio numOrangeWins numOrangeGames))
              , (Text.pack "totalScore", Aeson.toJSON totalScore)
              , (Text.pack "totalGoals", Aeson.toJSON totalGoals)
              , (Text.pack "totalAssists", Aeson.toJSON totalAssists)
              , (Text.pack "totalSaves", Aeson.toJSON totalSaves)
              , (Text.pack "totalShots", Aeson.toJSON totalShots)
              , (Text.pack "secondsPlayed", Aeson.toJSON secondsPlayed)
              , ( Text.pack "scorePerSecond"
                , Aeson.toJSON (makeRatio totalScore secondsPlayed))
              , ( Text.pack "goalsPerSecond"
                , Aeson.toJSON (makeRatio totalGoals secondsPlayed))
              , ( Text.pack "assistsPerSecond"
                , Aeson.toJSON (makeRatio totalAssists secondsPlayed))
              , ( Text.pack "savesPerSecond"
                , Aeson.toJSON (makeRatio totalSaves secondsPlayed))
              , ( Text.pack "shotsPerSecond"
                , Aeson.toJSON (makeRatio totalShots secondsPlayed))
              , (Text.pack "games", Aeson.toJSON (games :: [GameRow]))
              ]
      pure (Common.jsonResponse Http.status200 [] body)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

data GameRow = GameRow
  { gameRowPlaylistId :: Integer
  , gameRowPlaylistName :: Maybe Common.Text
  , gameRowPlayedAt :: Common.LocalTime
  , gameRowDidWin :: Bool
  , gameRowYourScore :: Integer
  , gameRowTheirScore :: Integer
  , gameRowDuration :: Integer
  , gameRowBodyId :: Integer
  , gameRowBodyName :: Maybe Common.Text
  , gameRowScore :: Integer
  , gameRowGoals :: Integer
  , gameRowAssists :: Integer
  , gameRowSaves :: Integer
  , gameRowShots :: Integer
  , gameRowArenaId :: Integer
  , gameRowArenaName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GameRow

instance Common.ToJSON GameRow where
  toJSON = Common.genericToJSON "GameRow"

getStatsSummaryHandler :: Common.Handler
getStatsSummaryHandler _config connection request = do
  let query = Wai.queryString request
  time <- getDay query
  let playlists = getPlaylists query
  [[numGames, numBlueWins, numOrangeWins]] <-
    Database.query
      connection
      [Common.sql|
        SELECT
          count(*),
          count(CASE WHEN blue_score > orange_score THEN 1 END),
          count(CASE WHEN blue_score < orange_score THEN 1 END)
        FROM games
        WHERE
          played_at >= ? AND
          playlist_id IN ?
      |]
      (time, Common.In playlists)
  let blueWinPercentage = makeRatio numBlueWins numGames
  let orangeWinPercentage = makeRatio numOrangeWins numGames
  arenaFrequencies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.name,
          count(*)
        FROM games
        INNER JOIN arenas
          ON arenas.id = games.arena_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ?
        GROUP BY arenas.name
      |]
      (time, Common.In playlists)
  let arenaPercents =
        map
          (\(arena, frequency) -> (arena, makeRatio frequency numGames))
          arenaFrequencies
  bodyFrequencies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          coalesce(bodies.name, to_char(games_players.body_id, 'FM9999')) AS body,
          count(*)
        FROM games_players
        INNER JOIN games
          ON games.id = games_players.game_id
        INNER JOIN bodies
          ON bodies.id = games_players.body_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ?
        GROUP BY body
      |]
      (time, Common.In playlists)
  let numBodies = sum (map snd bodyFrequencies)
  let bodyPercents =
        map
          (\(k, frequency) -> (k, makeRatio frequency numBodies))
          bodyFrequencies
  let status = Http.status200
  let headers = []
  let body =
        Aeson.object
          [ ( Text.pack "bodyFreqPct"
            , Aeson.object
                (map (\(k, percent) -> (k, Aeson.toJSON percent)) bodyPercents))
          , ( Text.pack "mapFreqPct"
            , Aeson.object
                (map
                   (\(arena, percent) -> (arena, Aeson.toJSON percent))
                   arenaPercents))
          , ( Text.pack "winPct"
            , Aeson.object
                [ (Text.pack "blue", Aeson.toJSON blueWinPercentage)
                , (Text.pack "orange", Aeson.toJSON orangeWinPercentage)
                ])
          , (Text.pack "numGames", Aeson.toJSON numGames)
          ]
  let response = Common.jsonResponse status headers body
  pure response

makeRatio :: Integer -> Integer -> Float
makeRatio numerator denominator =
  if denominator == 0
    then 0
    else fromRational (numerator Ratio.% denominator)

getDay :: Common.Query -> IO Time.Day
getDay query = do
  now <- Time.getCurrentTime
  let today = Time.utctDay now
  let day =
        case getParam "time" query of
          Just "month" -> Time.addDays (-28) today
          Just "week" -> Time.addDays (-7) today
          _ -> startOfSeason3
  pure day

startOfSeason3 :: Time.Day
startOfSeason3 = Time.fromGregorian 2016 6 20

getPlaylists :: Common.Query -> [Int]
getPlaylists query =
  case getParam "playlist" query of
    Just "ranked1v1" -> [competitiveSoloDuel]
    Just "ranked2v2" -> [competitiveDoubles]
    Just "ranked3v3solo" -> [competitiveSoloStandard]
    Just "ranked3v3" -> [competitiveStandard]
    _ -> competitivePlaylists

competitivePlaylists :: [Int]
competitivePlaylists =
  [ competitiveSoloDuel
  , competitiveDoubles
  , competitiveSoloStandard
  , competitiveStandard
  ]

competitiveSoloDuel :: Int
competitiveSoloDuel = 10

competitiveDoubles :: Int
competitiveDoubles = 11

competitiveSoloStandard :: Int
competitiveSoloStandard = 12

competitiveStandard :: Int
competitiveStandard = 13

getParam :: String -> Common.Query -> Maybe String
getParam name query =
  case lookup (ByteString.pack name) query of
    Just (Just value) -> Just (ByteString.unpack value)
    _ -> Nothing
