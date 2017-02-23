{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Text.Read as Read

getStatsPlayersBodiesHandler :: Common.Text -> Common.Handler
getStatsPlayersBodiesHandler rawPlayerId _config connection request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
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
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ? AND
              arena_templates.name IN ?
            GROUP BY bodies.id
            ORDER BY bodies.id ASC
          |]
          (playerId, day, Common.In playlists, Common.In templates)
      let json = Aeson.toJSON (bodies :: [BodyStats])
      pure (Common.jsonResponse Http.status200 [] json)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

getStatsPlayersArenasHandler :: Common.Text -> Common.Handler
getStatsPlayersArenasHandler rawPlayerId _config connection request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
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
            FROM games_players
            INNER JOIN games ON games.id = games_players.game_id
            INNER JOIN arenas ON arenas.id = games.arena_id
            INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ? AND
              arena_templates.name IN ?
            GROUP BY arenas.id
            ORDER BY arenas.id ASC
          |]
          (playerId, day, Common.In playlists, Common.In templates)
      let json = Aeson.toJSON (arenas :: [ArenaStats])
      pure (Common.jsonResponse Http.status200 [] json)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

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

getNewStatsPlayersHandler :: Common.Text -> Common.Handler
getNewStatsPlayersHandler rawPlayerId _config connection request = do
  maybePlayerId <- getPlayerId connection rawPlayerId
  case maybePlayerId of
    Nothing -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)
    Just playerId -> do
      platform <- getPlatform connection playerId
      namesAndTimes <- getNamesAndTimes connection playerId
      (day, playlists, templates) <- getFilters request
      games <- getGames connection day playlists templates playerId
      let gameIds = map playerGameRowGameId games
      gamesPlayers <- getGamesPlayers connection gameIds
      let body =
            Aeson.toJSON
              (makePlayerOutput platform namesAndTimes games gamesPlayers)
      pure (Common.jsonResponse Http.status200 [] body)

getGames :: Sql.Connection
         -> Time.Day
         -> [Int]
         -> [String]
         -> Int
         -> IO [PlayerGameRow]
getGames connection day playlists templates player =
  Database.query
    connection
    [Common.sql|
      SELECT
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
      FROM games
      INNER JOIN arenas ON arenas.id = games.arena_id
      LEFT OUTER JOIN arena_skins ON arena_skins.id = arenas.skin_id
      INNER JOIN arena_models ON arena_models.id = arenas.model_id
      INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
      INNER JOIN games_players ON games_players.game_id = games.id
      INNER JOIN playlists ON playlists.id = games.playlist_id
      WHERE
        games.played_at >= ? AND
        playlists.id IN ? AND
        arena_templates.name IN ? AND
        games_players.player_id = ?
      ORDER BY games.played_at DESC
      LIMIT 20
    |]
    (day, Common.In playlists, Common.In templates, player)

data PlayerGameRow = PlayerGameRow
  { playerGameRowGameId :: Int
  , playerGameRowPlaylistId :: Int
  , playerGameRowPlaylistName :: Maybe Common.Text
  , playerGameRowArenaId :: Int
  , playerGameRowArenaName :: Maybe Common.Text
  , playerGameRowArenaSkinId :: Maybe Int
  , playerGameRowArenaSkinName :: Maybe Common.Text
  , playerGameRowArenaModelId :: Int
  , playerGameRowArenaModelName :: Maybe Common.Text
  , playerGameRowArenaTemplateId :: Int
  , playerGameRowArenaTemplateName :: Maybe Common.Text
  , playerGameRowPlayedAt :: Time.LocalTime
  , playerGameRowDuration :: Int
  , playerGameRowBlueGoals :: Int
  , playerGameRowOrangeGoals :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerGameRow

getGamesPlayers :: Sql.Connection -> [Int] -> IO [GamePlayerRow]
getGamesPlayers connection gameIds =
  Database.query
    connection
    [Common.sql|
      SELECT
        games_players.id,
        games_players.game_id,
        games_players.player_id,
        players.platform_id,
        platforms.name,
        players.remote_id,
        players.local_id,
        games_players.name,
        games_players.xp,
        games_players.is_blue,
        games_players.is_present_at_end,
        games_players.score,
        games_players.goals,
        games_players.assists,
        games_players.saves,
        games_players.shots,
        games_players.body_id,
        bodies.name,
        games_players.decal_id,
        decals.name,
        games_players.wheel_id,
        wheels.name,
        games_players.rocket_trail_id,
        rocket_trails.name,
        games_players.antenna_id,
        antennas.name,
        games_players.topper_id,
        toppers.name,
        games_players.wheel_paint_id,
        games_players.topper_paint_id,
        games_players.primary_color_id,
        games_players.accent_color_id,
        games_players.primary_finish_id,
        games_players.accent_finish_id,
        games_players.fov,
        games_players.height,
        games_players.angle,
        games_players.distance,
        games_players.stiffness,
        games_players.swivel_speed
      FROM games_players
      INNER JOIN players ON players.id = games_players.player_id
      INNER JOIN platforms ON platforms.id = players.platform_id
      LEFT OUTER JOIN bodies ON bodies.id = games_players.body_id
      LEFT OUTER JOIN decals ON decals.id = games_players.decal_id
      LEFT OUTER JOIN wheels ON wheels.id = games_players.wheel_id
      LEFT OUTER JOIN rocket_trails ON rocket_trails.id = games_players.rocket_trail_id
      LEFT OUTER JOIN antennas ON antennas.id = games_players.antenna_id
      LEFT OUTER JOIN toppers ON toppers.id = games_players.topper_id
      WHERE game_id IN ?
      ORDER BY game_id ASC, player_id ASC
    |]
    [Common.In gameIds]

data GamePlayerRow = GamePlayerRow
  { gamePlayerRowId :: Int
  , gamePlayerRowGameId :: Int
  , gamePlayerRowPlayerId :: Int
  , gamePlayerRowPlatformId :: Int
  , gamePlayerRowPlatformName :: Common.Text
  , gamePlayerRowRemoteId :: Common.Text
  , gamePlayerRowLocalId :: Int
  , gamePlayerRowName :: Common.Text
  , gamePlayerRowXp :: Int
  , gamePlayerRowIsBlue :: Bool
  , gamePlayerRowIsPresentAtEnd :: Bool
  , gamePlayerRowScore :: Int
  , gamePlayerRowGoals :: Int
  , gamePlayerRowAssists :: Int
  , gamePlayerRowSaves :: Int
  , gamePlayerRowShots :: Int
  , gamePlayerRowBodyId :: Int
  , gamePlayerRowBodyName :: Maybe Common.Text
  , gamePlayerRowDecalId :: Int
  , gamePlayerRowDecalName :: Maybe Common.Text
  , gamePlayerRowWheelId :: Int
  , gamePlayerRowWheelName :: Maybe Common.Text
  , gamePlayerRowRocketTrailId :: Int
  , gamePlayerRowRocketTrailName :: Maybe Common.Text
  , gamePlayerRowAntennaId :: Int
  , gamePlayerRowAntennaName :: Maybe Common.Text
  , gamePlayerRowTopperId :: Int
  , gamePlayerRowTopperName :: Maybe Common.Text
  , gamePlayerRowWheelPaintId :: Maybe Int
  , gamePlayerRowTopperPaintId :: Maybe Int
  , gamePlayerRowPrimaryColorId :: Int
  , gamePlayerRowAccentColorId :: Int
  , gamePlayerRowPrimaryFinishId :: Int
  , gamePlayerRowAccentFinishId :: Int
  , gamePlayerRowFov :: Float
  , gamePlayerRowHeight :: Float
  , gamePlayerRowAngle :: Float
  , gamePlayerRowDistance :: Float
  , gamePlayerRowStiffness :: Float
  , gamePlayerRowSwivelSpeed :: Float
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GamePlayerRow

data PlayerOutput = PlayerOutput
  { playerOutputName :: Common.Text
  , playerOutputAliases :: [Common.Text]
  , playerOutputLastPlayedAt :: Common.LocalTime
  , playerOutputPlatform :: Common.Platform
  , playerOutputGames :: [GameOutput]
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON PlayerOutput where
  toJSON = Common.genericToJSON "PlayerOutput"

makePlayerOutput
  :: Maybe Common.Platform
  -> [(Common.Text, Common.LocalTime)]
  -> [PlayerGameRow]
  -> [GamePlayerRow]
  -> Maybe PlayerOutput
makePlayerOutput maybePlatform namesAndTimes games players = do
  platform <- maybePlatform
  (name, aliases, lastPlayedAt) <-
    case namesAndTimes of
      (n, t):nts -> pure (n, map fst nts, t)
      _ -> Nothing
  let playersByGame =
        foldr
          (\player -> Map.insertWith (++) (gamePlayerRowGameId player) [player])
          Map.empty
          players
  let gamesWithPlayers =
        map
          (\game ->
             ( game
             , Map.findWithDefault [] (playerGameRowGameId game) playersByGame))
          games
  pure
    PlayerOutput
    { playerOutputName = name
    , playerOutputAliases = aliases
    , playerOutputLastPlayedAt = lastPlayedAt
    , playerOutputPlatform = platform
    , playerOutputGames = map makeGameOutput gamesWithPlayers
    }

getPlatform :: Sql.Connection -> Int -> IO (Maybe Common.Platform)
getPlatform connection playerId = do
  result <-
    Database.query
      connection
      [Common.sql|
        SELECT platforms.id, platforms.name
        FROM platforms
        INNER JOIN players ON players.platform_id = platforms.id
        WHERE players.id = ?
      |]
      [playerId]
  case result of
    [platform] -> pure (Just platform)
    _ -> pure Nothing

data GameOutput = GameOutput
  { gameOutputId :: Int
  , gameOutputPlaylistId :: Int
  , gameOutputPlaylistName :: Maybe Common.Text
  , gameOutputArena :: ArenaOutput
  , gameOutputPlayedAt :: Time.LocalTime
  , gameOutputDuration :: Int
  , gameOutputBlueGoals :: Int
  , gameOutputOrangeGoals :: Int
  , gameOutputPlayers :: [GamePlayerOutput]
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON GameOutput where
  toJSON = Common.genericToJSON "GameOutput"

makeGameOutput :: (PlayerGameRow, [GamePlayerRow]) -> GameOutput
makeGameOutput (game, players) =
  GameOutput
  { gameOutputId = playerGameRowGameId game
  , gameOutputPlaylistId = playerGameRowPlaylistId game
  , gameOutputPlaylistName = playerGameRowPlaylistName game
  , gameOutputArena = makeArenaOutput game
  , gameOutputPlayedAt = playerGameRowPlayedAt game
  , gameOutputDuration = playerGameRowDuration game
  , gameOutputBlueGoals = playerGameRowBlueGoals game
  , gameOutputOrangeGoals = playerGameRowOrangeGoals game
  , gameOutputPlayers = map makeGamePlayerOutput players
  }

data ArenaOutput = ArenaOutput
  { arenaOutputId :: Int
  , arenaOutputName :: Maybe Common.Text
  , arenaOutputSkinId :: Maybe Int
  , arenaOutputSkinName :: Maybe Common.Text
  , arenaOutputModelId :: Int
  , arenaOutputModelName :: Maybe Common.Text
  , arenaOutputTemplateId :: Int
  , arenaOutputTemplateName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON ArenaOutput where
  toJSON = Common.genericToJSON "ArenaOutput"

makeArenaOutput :: PlayerGameRow -> ArenaOutput
makeArenaOutput game =
  ArenaOutput
  { arenaOutputId = playerGameRowArenaId game
  , arenaOutputName = playerGameRowArenaName game
  , arenaOutputSkinId = playerGameRowArenaSkinId game
  , arenaOutputSkinName = playerGameRowArenaSkinName game
  , arenaOutputModelId = playerGameRowArenaModelId game
  , arenaOutputModelName = playerGameRowArenaModelName game
  , arenaOutputTemplateId = playerGameRowArenaTemplateId game
  , arenaOutputTemplateName = playerGameRowArenaTemplateName game
  }

data GamePlayerOutput = GamePlayerOutput
  { gamePlayerOutputPlayerId :: Int
  , gamePlayerOutputPlatformId :: Int
  , gamePlayerOutputPlatformName :: Common.Text
  , gamePlayerOutputRemoteId :: Common.Text
  , gamePlayerOutputLocalId :: Int
  , gamePlayerOutputName :: Common.Text
  , gamePlayerOutputXp :: Int
  , gamePlayerOutputIsOnBlueTeam :: Bool
  , gamePlayerOutputIsPresentAtEnd :: Bool
  , gamePlayerOutputScore :: Int
  , gamePlayerOutputGoals :: Int
  , gamePlayerOutputAssists :: Int
  , gamePlayerOutputSaves :: Int
  , gamePlayerOutputShots :: Int
  , gamePlayerOutputLoadout :: LoadoutOutput
  , gamePlayerOutputCamera :: CameraOutput
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON GamePlayerOutput where
  toJSON = Common.genericToJSON "GamePlayerOutput"

makeGamePlayerOutput :: GamePlayerRow -> GamePlayerOutput
makeGamePlayerOutput player =
  GamePlayerOutput
  { gamePlayerOutputPlayerId = gamePlayerRowPlayerId player
  , gamePlayerOutputPlatformId = gamePlayerRowPlatformId player
  , gamePlayerOutputPlatformName = gamePlayerRowPlatformName player
  , gamePlayerOutputRemoteId = gamePlayerRowRemoteId player
  , gamePlayerOutputLocalId = gamePlayerRowLocalId player
  , gamePlayerOutputName = gamePlayerRowName player
  , gamePlayerOutputXp = gamePlayerRowXp player
  , gamePlayerOutputIsOnBlueTeam = gamePlayerRowIsBlue player
  , gamePlayerOutputIsPresentAtEnd = gamePlayerRowIsPresentAtEnd player
  , gamePlayerOutputScore = gamePlayerRowScore player
  , gamePlayerOutputGoals = gamePlayerRowGoals player
  , gamePlayerOutputAssists = gamePlayerRowAssists player
  , gamePlayerOutputSaves = gamePlayerRowSaves player
  , gamePlayerOutputShots = gamePlayerRowShots player
  , gamePlayerOutputLoadout = makeLoadoutOutput player
  , gamePlayerOutputCamera = makeCameraOutput player
  }

data LoadoutOutput = LoadoutOutput
  { loadoutOutputBodyId :: Int
  , loadoutOutputBodyName :: Maybe Common.Text
  , loadoutOutputDecalId :: Int
  , loadoutOutputDecalName :: Maybe Common.Text
  , loadoutOutputWheelId :: Int
  , loadoutOutputWheelName :: Maybe Common.Text
  , loadoutOutputRocketTrailId :: Int
  , loadoutOutputRocketTrailName :: Maybe Common.Text
  , loadoutOutputAntennaId :: Int
  , loadoutOutputAntennaName :: Maybe Common.Text
  , loadoutOutputTopperId :: Int
  , loadoutOutputTopperName :: Maybe Common.Text
  , loadoutOutputWheelPaintId :: Maybe Int
  , loadoutOutputTopperPaintId :: Maybe Int
  , loadoutOutputPrimaryColorId :: Int
  , loadoutOutputAccentColorId :: Int
  , loadoutOutputPrimaryFinishId :: Int
  , loadoutOutputAccentFinishId :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON LoadoutOutput where
  toJSON = Common.genericToJSON "LoadoutOutput"

makeLoadoutOutput :: GamePlayerRow -> LoadoutOutput
makeLoadoutOutput player =
  LoadoutOutput
  { loadoutOutputBodyId = gamePlayerRowBodyId player
  , loadoutOutputBodyName = gamePlayerRowBodyName player
  , loadoutOutputDecalId = gamePlayerRowDecalId player
  , loadoutOutputDecalName = gamePlayerRowDecalName player
  , loadoutOutputWheelId = gamePlayerRowWheelId player
  , loadoutOutputWheelName = gamePlayerRowWheelName player
  , loadoutOutputRocketTrailId = gamePlayerRowRocketTrailId player
  , loadoutOutputRocketTrailName = gamePlayerRowRocketTrailName player
  , loadoutOutputAntennaId = gamePlayerRowAntennaId player
  , loadoutOutputAntennaName = gamePlayerRowAntennaName player
  , loadoutOutputTopperId = gamePlayerRowTopperId player
  , loadoutOutputTopperName = gamePlayerRowTopperName player
  , loadoutOutputWheelPaintId = gamePlayerRowWheelPaintId player
  , loadoutOutputTopperPaintId = gamePlayerRowTopperPaintId player
  , loadoutOutputPrimaryColorId = gamePlayerRowPrimaryColorId player
  , loadoutOutputAccentColorId = gamePlayerRowAccentColorId player
  , loadoutOutputPrimaryFinishId = gamePlayerRowPrimaryFinishId player
  , loadoutOutputAccentFinishId = gamePlayerRowAccentFinishId player
  }

data CameraOutput = CameraOutput
  { cameraOutputFov :: Float
  , cameraOutputHeight :: Float
  , cameraOutputAngle :: Float
  , cameraOutputDistance :: Float
  , cameraOutputStiffness :: Float
  , cameraOutputSwivelSpeed :: Float
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON CameraOutput where
  toJSON = Common.genericToJSON "CameraOutput"

makeCameraOutput :: GamePlayerRow -> CameraOutput
makeCameraOutput player =
  CameraOutput
  { cameraOutputFov = gamePlayerRowFov player
  , cameraOutputHeight = gamePlayerRowHeight player
  , cameraOutputAngle = gamePlayerRowAngle player
  , cameraOutputDistance = gamePlayerRowDistance player
  , cameraOutputStiffness = gamePlayerRowStiffness player
  , cameraOutputSwivelSpeed = gamePlayerRowSwivelSpeed player
  }

getNamesAndTimes :: Sql.Connection
                 -> Int
                 -> IO [(Common.Text, Common.LocalTime)]
getNamesAndTimes connection playerId =
  Database.query
    connection
    [Common.sql|
      SELECT games_players.name, max(games.played_at) AS last_played_at
      FROM games_players
      INNER JOIN games ON games.id = games_players.game_id
      WHERE games_players.player_id = ?
      GROUP BY games_players.name
      ORDER BY last_played_at DESC
    |]
    [playerId]

getPlayerId :: Sql.Connection -> Common.Text -> IO (Maybe Int)
getPlayerId connection rawPlayerId = do
  let maybePlayerId = Read.readMaybe (Text.unpack rawPlayerId)
  rows <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [maybePlayerId :: Maybe Int]
  case rows of
    [[playerId]] -> pure (Just playerId)
    _ -> pure Nothing

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
        case Common.getParam "time" query of
          Just "month" -> Time.addDays (-28) today
          Just "week" -> Time.addDays (-7) today
          Just "day" -> Time.addDays (-1) today
          _ -> startOfSeason3
  pure day

startOfSeason3 :: Time.Day
startOfSeason3 = Time.fromGregorian 2016 6 20

getPlaylists :: Common.Query -> [Int]
getPlaylists query =
  case Common.getParam "playlist" query of
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
  case Common.getParam "map" query of
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
