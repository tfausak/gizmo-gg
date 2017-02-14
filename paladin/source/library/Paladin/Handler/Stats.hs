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

getStatsArenasHandler :: Common.Handler
getStatsArenasHandler _config connection request = do
  (day, playlists, templates) <- getFilters request
  arenas <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.id,
          arenas.name,
          sum(games_players.score),
          sum(games_players.goals),
          sum(games_players.assists),
          sum(games_players.saves),
          sum(games_players.shots),
          count(*)
        FROM arenas
        INNER JOIN games ON games.arena_id = arenas.id
        INNER JOIN games_players ON games_players.game_id = games.id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ?
        GROUP BY games.id, arenas.id
        ORDER BY arenas.id
      |]
      (day, Common.In playlists, Common.In templates)
  let json = Aeson.toJSON (arenas :: [ArenaStats])
  pure (Common.jsonResponse Http.status200 [] json)

data ArenaStats = ArenaStats
  { arenaStatsArenaId :: Int
  , arenaStatsArenaName :: Common.Text
  , arenaStatsTotalScore :: Int
  , arenaStatsTotalGoals :: Int
  , arenaStatsTotalAssists :: Int
  , arenaStatsTotalSaves :: Int
  , arenaStatsTotalShots :: Int
  , arenaStatsNumGames :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow ArenaStats

instance Common.ToJSON ArenaStats where
  toJSON = Common.genericToJSON "ArenaStats"

getStatsBodiesHandler :: Common.Handler
getStatsBodiesHandler _config connection request = do
  (day, playlists, templates) <- getFilters request
  bodies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          bodies.id,
          bodies.name,
          sum(games_players.score),
          sum(games_players.goals),
          sum(games_players.assists),
          sum(games_players.saves),
          sum(games_players.shots),
          count(*),
          count(CASE WHEN games_players.is_blue
            THEN (CASE WHEN games.blue_goals > games.orange_goals THEN 1 END)
            ELSE (CASE WHEN games.orange_goals > games.blue_goals THEN 1 END)
            END),
          count( CASE WHEN games_players.is_blue
            THEN (CASE WHEN games.blue_goals < games.orange_goals THEN 1 END)
            ELSE (CASE WHEN games.orange_goals < games.blue_goals THEN 1 END)
            END)
        FROM bodies
        INNER JOIN games_players ON games_players.body_id = bodies.id
        INNER JOIN games ON games.id = games_players.game_id
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games_players.is_present_at_end = true AND
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ?
        GROUP BY bodies.id
        ORDER BY bodies.id ASC
      |]
      (day, Common.In playlists, Common.In templates)
  let json = Aeson.toJSON (bodies :: [BodyStats])
  pure (Common.jsonResponse Http.status200 [] json)

data BodyStats = BodyStats
  { bodyStatsBodyId :: Int
  , bodyStatsBodyName :: Maybe Common.Text
  , bodyStatsTotalScore :: Int
  , bodyStatsTotalGoals :: Int
  , bodyStatsTotalAssists :: Int
  , bodyStatsTotalSaves :: Int
  , bodyStatsTotalShots :: Int
  , bodyStatsNumGames :: Int
  , bodyStatsNumWins :: Int
  , bodyStatsNumLosses :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow BodyStats

instance Common.ToJSON BodyStats where
  toJSON = Common.genericToJSON "BodyStats"

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
      (day, playlists, templates) <- getFilters request
      players <-
        Database.query
          connection
          [Common.sql|
            SELECT
              count(CASE WHEN games_players.is_blue THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue THEN 1 END),
              count(CASE WHEN games_players.is_blue AND games.blue_goals > games.orange_goals THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue and games.orange_goals > games.blue_goals THEN 1 END),
              coalesce(sum(games_players.score), 0),
              coalesce(sum(games_players.goals), 0),
              coalesce(sum(games_players.assists), 0),
              coalesce(sum(games_players.saves), 0),
              coalesce(sum(games_players.shots), 0),
              coalesce(sum(games.duration), 0)
            FROM games
            INNER JOIN games_players ON games_players.game_id = games.id
            INNER JOIN arenas ON arenas.id = games.arena_id
            INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ? AND
              arena_templates.name IN ?
          |]
          (playerId, day, Common.In playlists, Common.In templates)
      let player = Maybe.fromMaybe defaultPlayerRow (Maybe.listToMaybe players)
      let numBlueGames = playerRowNumBlueGames player
      let numOrangeGames = playerRowNumOrangeGames player
      let numBlueWins = playerRowNumBlueWins player
      let numOrangeWins = playerRowNumOrangeWins player
      let totalScore = playerRowTotalScore player
      let totalGoals = playerRowTotalGoals player
      let totalAssists = playerRowTotalAssists player
      let totalSaves = playerRowTotalSaves player
      let totalShots = playerRowTotalShots player
      let secondsPlayed = playerRowSecondsPlayed player
      games <-
        Database.query
          connection
          [Common.sql|
            SELECT
              games.playlist_id,
              playlists.name,
              games.played_at,
              CASE WHEN games_players.is_blue THEN games.blue_goals ELSE games.orange_goals END,
              CASE WHEN games_players.is_blue THEN games.orange_goals ELSE games.blue_goals END,
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
            INNER JOIN games_players ON games_players.game_id = games.id
            INNER JOIN playlists ON playlists.id = games.playlist_id
            INNER JOIN bodies ON bodies.id = games_players.body_id
            INNER JOIN arenas ON arenas.id = games.arena_id
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
              , (Text.pack "games", Aeson.toJSON (games :: [GameRow]))
              ]
      pure (Common.jsonResponse Http.status200 [] body)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

data PlayerRow = PlayerRow
  { playerRowNumBlueGames :: Integer
  , playerRowNumOrangeGames :: Integer
  , playerRowNumBlueWins :: Integer
  , playerRowNumOrangeWins :: Integer
  , playerRowTotalScore :: Integer
  , playerRowTotalGoals :: Integer
  , playerRowTotalAssists :: Integer
  , playerRowTotalSaves :: Integer
  , playerRowTotalShots :: Integer
  , playerRowSecondsPlayed :: Integer
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerRow

defaultPlayerRow :: PlayerRow
defaultPlayerRow =
  PlayerRow
  { playerRowNumBlueGames = 0
  , playerRowNumOrangeGames = 0
  , playerRowNumBlueWins = 0
  , playerRowNumOrangeWins = 0
  , playerRowTotalScore = 0
  , playerRowTotalGoals = 0
  , playerRowTotalAssists = 0
  , playerRowTotalSaves = 0
  , playerRowTotalShots = 0
  , playerRowSecondsPlayed = 0
  }

data GameRow = GameRow
  { gameRowPlaylistId :: Integer
  , gameRowPlaylistName :: Maybe Common.Text
  , gameRowPlayedAt :: Common.LocalTime
  , gameRowYourGoals :: Integer
  , gameRowTheirGoals :: Integer
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
  (day, playlists, templates) <- getFilters request
  [[numGames, numBlueWins, numOrangeWins]] <-
    Database.query
      connection
      [Common.sql|
        SELECT
          count(*),
          count(CASE WHEN blue_goals > orange_goals THEN 1 END),
          count(CASE WHEN blue_goals < orange_goals THEN 1 END)
        FROM games
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          played_at >= ? AND
          playlist_id IN ? AND
          arena_templates.name IN ?
      |]
      (day, Common.In playlists, Common.In templates)
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
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ?
        GROUP BY arenas.name
      |]
      (day, Common.In playlists, Common.In templates)
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
        INNER JOIN games ON games.id = games_players.game_id
        INNER JOIN bodies ON bodies.id = games_players.body_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ?
        GROUP BY body
      |]
      (day, Common.In playlists)
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

getFilters :: Wai.Request -> IO (Time.Day, [Int], [String])
getFilters request = do
  let query = Wai.queryString request
  day <- getDay query
  let playlists = getPlaylists query
  let templates = getTemplates query
  pure (day, playlists, templates)

getDay :: Common.Query -> IO Time.Day
getDay query = do
  now <- Time.getCurrentTime
  let today = Time.utctDay now
  let day =
        case getParam "time" query of
          Just "month" -> Time.addDays (-28) today
          Just "week" -> Time.addDays (-7) today
          Just "day" -> Time.addDays (-1) today
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

getTemplates :: Common.Query -> [String]
getTemplates query =
  case getParam "map" query of
    Just "arc" -> [starbaseArcTemplate]
    Just "standard" -> [standardTemplate]
    Just "tokyo" -> [neoTokyoTemplate]
    Just "wasteland" -> [wastelandTemplate]
    _ -> competitiveTemplates

competitiveTemplates :: [String]
competitiveTemplates =
  [neoTokyoTemplate, standardTemplate, starbaseArcTemplate, wastelandTemplate]

neoTokyoTemplate :: String
neoTokyoTemplate = "Neo Tokyo"

standardTemplate :: String
standardTemplate = "Standard"

starbaseArcTemplate :: String
starbaseArcTemplate = "Starbase ARC"

wastelandTemplate :: String
wastelandTemplate = "Wasteland"

getParam :: String -> Common.Query -> Maybe String
getParam name query =
  case lookup (ByteString.pack name) query of
    Just (Just value) -> Just (ByteString.unpack value)
    _ -> Nothing
