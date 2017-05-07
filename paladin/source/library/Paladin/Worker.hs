{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Worker
  ( startWorker
  ) where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad.Fail as Fail
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.SqlQQ as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Paladin.Analysis as Analysis
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Entity as Entity
import qualified Paladin.Rank as Rank
import qualified Paladin.Storage as Storage
import qualified Paladin.Utility as Utility
import qualified Rattletrap

startWorker :: Config.Config -> Sql.Connection -> IO ()
startWorker config connection = do
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO parsers (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [parser]
  manager <- Client.newManager TLS.tlsManagerSettings
  let apiToken = Config.configApiToken config
  _ <- Concurrent.forkIO (updatePlayerSkills connection manager apiToken)
  _ <- Concurrent.forkIO (parseUploads config connection)
  pure ()

updatePlayerSkills :: Sql.Connection -> Client.Manager -> String -> IO ()
updatePlayerSkills connection manager apiToken = do
  maybePlayerWithPlatform <- getOldestPlayerWithPlatform connection
  case maybePlayerWithPlatform of
    Nothing -> do
      -- Could not find any players, so no skills to look up. Wait a second and
      -- try again.
      Utility.sleep 1
      updatePlayerSkills connection manager apiToken
    Just (player, platform) -> do
      let platformName = case Entity.platformName platform of
            Entity.PlayStation -> "ps4"
            Entity.Splitscreen -> "unknown-platform" -- FIXME
            Entity.Steam -> "steam"
            Entity.Xbox -> "xboxone"
      maybePlayerName <- getPlayerName connection player
      let playerId = case Entity.platformName platform of
            Entity.Steam -> player & Entity.playerRemoteId & Text.unpack
            _ -> maybe "unknown-player" Text.unpack maybePlayerName -- FIXME
      result <- Rank.getPlayerSkills manager apiToken platformName playerId
      let emptySkill = Rank.Skill
            { Rank.skillDivision = 0
            , Rank.skillMatchesPlayed = 0
            , Rank.skillPlaylist = 1 -- unranked 1v1
            , Rank.skillSkill = 0
            , Rank.skillTier = 0
            , Rank.skillTierMax = 0
            }
      case result of
        Left message -> do
          putStrLn ("updatePlayerSkills: request failed! " ++ message)
          createPlayerSkill connection player emptySkill
        Right (Rank.Single (Rank.Skills skills)) -> do
          if null skills
            -- The player doesn't have any skills. We need to insert a row into
            -- our skill table otherwise we'll try to get them again on the
            -- next iteration. Janky, but keeps us from having to have a queue.
            then createPlayerSkill connection player emptySkill
            -- The player did have skills, so save them.
            else mapM_ (createPlayerSkill connection player) skills
      Utility.sleep 1
      updatePlayerSkills connection manager apiToken

createPlayerSkill :: Sql.Connection -> Entity.Player -> Rank.Skill -> IO ()
createPlayerSkill connection player skill = Database.execute connection
  [Sql.sql|
    insert into player_skills (
      player_id,
      playlist_id,
      matches_played,
      division,
      tier,
      mmr,
      mu,
      sigma
    ) values ( ?, ?, ?, ?, ?, ?, ?, ? )
  |]
  [ player & Entity.playerId & Sql.toField
  , skill & Rank.skillPlaylist & Entity.Tagged & Sql.toField
  , skill & Rank.skillMatchesPlayed & Sql.toField
  , skill & Rank.skillDivision & Sql.toField
  , skill & Rank.skillTier & Sql.toField
  , skill & Rank.skillSkill & Sql.toField
  , (0 :: Int) & Sql.toField -- mu
  , (0 :: Int) & Sql.toField -- sigma
  ]

getPlayerName :: Sql.Connection -> Entity.Player -> IO (Maybe Text.Text)
getPlayerName connection player = do
  rows <- Database.query connection
    [Sql.sql| select name from games_players where player_id = ? |]
    [Entity.playerId player]
  case rows of
    [[name]] -> pure (Just name)
    _ -> pure Nothing

getOldestPlayerWithPlatform
  :: Sql.Connection -> IO (Maybe (Entity.Player, Entity.Platform))
getOldestPlayerWithPlatform connection = do
  maybePlayerId <- getOldestPlayerId connection
  case maybePlayerId of
    Nothing -> pure Nothing
    Just playerId -> getPlayerWithPlatform connection playerId

getPlayerWithPlatform
  :: Sql.Connection
  -> Entity.PlayerId
  -> IO (Maybe (Entity.Player, Entity.Platform))
getPlayerWithPlatform connection playerId = do
  maybePlayer <- getPlayer connection playerId
  case maybePlayer of
    Nothing -> pure Nothing
    Just player -> do
      let platformId = Entity.playerPlatformId player
      maybePlatform <- getPlatform connection platformId
      case maybePlatform of
        Nothing -> pure Nothing
        Just platform -> pure (Just (player, platform))

getPlayer :: Sql.Connection -> Entity.PlayerId -> IO (Maybe Entity.Player)
getPlayer connection playerId = do
  maybePlayer <- Database.query connection [Sql.sql|
    select id, created_at, platform_id, remote_id, local_id
    from players
    where id = ?
  |] [playerId]
  case maybePlayer of
    [player] -> pure (Just player)
    _ -> pure Nothing

getPlatform :: Sql.Connection -> Entity.PlatformId -> IO (Maybe Entity.Platform)
getPlatform connection platformId = do
  maybePlatform <- Database.query connection [Sql.sql|
    select id, name
    from platforms
    where id = ?
  |] [platformId]
  case maybePlatform of
    [platform] -> pure (Just platform)
    _ -> pure Nothing

getOldestPlayerId :: Sql.Connection -> IO (Maybe Entity.PlayerId)
getOldestPlayerId connection = do
  maybePlayerId <- getOldestPlayerIdWithoutSkill connection
  case maybePlayerId of
    Just playerId -> pure (Just playerId)
    Nothing -> getOldestPlayerIdWithSkill connection

getOldestPlayerIdWithoutSkill :: Sql.Connection -> IO (Maybe Entity.PlayerId)
getOldestPlayerIdWithoutSkill connection = do
  playerIds <- Database.query_ connection [Sql.sql|
    select players.id
    from players
    left join player_skills on player_skills.player_id = players.id
    where player_skills.player_id is null
    order by players.created_at asc
    limit 1
  |]
  case playerIds of
    [[playerId]] -> pure (Just playerId)
    _ -> pure Nothing

getOldestPlayerIdWithSkill :: Sql.Connection -> IO (Maybe Entity.PlayerId)
getOldestPlayerIdWithSkill connection = do
  playerIds <- Database.query_ connection [Sql.sql|
    select player_id
    from player_skills
    group by player_id
    order by max(created_at) asc
    limit 1
  |]
  case playerIds of
    [[playerId]] -> pure (Just playerId)
    _ -> pure Nothing

parseUploads :: Config.Config -> Sql.Connection -> IO ()
parseUploads config connection = do
  uploads <-
    Sql.query
      connection
      [Sql.sql|
        UPDATE uploads
        SET
          started_parsing_at = now(),
          parser_id = (SELECT id FROM parsers WHERE name = ? ORDER BY id ASC LIMIT 1)
        FROM (
          SELECT id AS upload_id
          FROM uploads
          WHERE started_parsing_at IS NULL
          ORDER BY created_at ASC
          LIMIT 1
        ) AS it
        WHERE id = upload_id
        RETURNING id, hash
      |]
      [parser]
  case uploads of
    [] -> Utility.sleep 1
    _ -> mapM_ (parseUpload config connection) uploads
  parseUploads config connection

parseUpload
  :: Config.Config
  -> Sql.Connection
  -> (Int, Utility.Tagged Hash.SHA1 String)
  -> IO ()
parseUpload config connection (uploadId, hash) =
  Exception.catch
    (do contents <- Storage.getUploadFile config hash
        replay <- parseReplay contents
        analysis <- Analysis.makeReplayAnalysis replay
        insertReplay connection uploadId analysis)
    (insertError connection uploadId)

parseReplay
  :: Fail.MonadFail m
  => LazyByteString.ByteString -> m Rattletrap.Replay
parseReplay contents =
  case Rattletrap.decodeReplay contents of
    Left message -> fail message
    Right replay -> pure replay

insertReplay :: Sql.Connection -> Int -> Analysis.ReplayAnalysis -> IO ()
insertReplay connection uploadId replay = do
  let arena = Analysis.replayAnalysisArena replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO arenas (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [arena]
  let maybeServerId = Analysis.replayAnalysisServerId replay
  let serverName = Analysis.replayAnalysisServerName replay
  case maybeServerId of
    Just serverId ->
      Database.execute
        connection
        [Sql.sql|
          INSERT INTO servers (id, name)
          VALUES (?, ?)
          ON CONFLICT DO NOTHING
        |]
        (serverId, serverName)
    Nothing -> Database.execute connection
      [Sql.sql| insert into servers (name) values (?) |]
      [serverName]
  let playlist = Analysis.replayAnalysisPlaylist replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO playlists (id)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [playlist]
  let maybeGameMode = Analysis.replayAnalysisGameMode replay
  case maybeGameMode of
    Just gameMode ->
      Database.execute
        connection
        [Sql.sql|
          INSERT INTO game_modes (id)
          VALUES (?)
          ON CONFLICT DO NOTHING
        |]
        [gameMode]
    _ -> pure ()
  let gameType = Analysis.replayAnalysisGameType replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO game_types (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [gameType]
  let players = Analysis.replayAnalysisPlayers replay
  mapM_ (insertPlayer connection) players
  let teamSize = Analysis.replayAnalysisTeamSize replay
  let isFair = Analysis.replayAnalysisIsFair replay
  let blueGoals = Analysis.replayAnalysisBlueGoals replay
  let orangeGoals = Analysis.replayAnalysisOrangeGoals replay
  let hash =
        makeGameHash
          gameType
          playlist
          maybeServerId
          maybeGameMode
          teamSize
          isFair
          arena
          blueGoals
          orangeGoals
          players
  let recordedAt = Analysis.replayAnalysisRecordedAt replay
  let duration = round (Analysis.replayAnalysisDuration replay)
  let blueWin = blueGoals > orangeGoals
  let insertGameRow =
        InsertGameRow
        { insertGameRowHash = show hash
        , insertGameRowGameType = gameType
        , insertGameRowPlaylist = playlist
        , insertGameRowMaybeServerId = maybeServerId
        , insertGameRowMaybeGameMode = maybeGameMode
        , insertGameRowTeamSize = teamSize
        , insertGameRowIsFair = isFair
        , insertGameRowArena = arena
        , insertGameRowBlueGoals = blueGoals
        , insertGameRowOrangeGoals = orangeGoals
        , insertGameRowRecordedAt = recordedAt
        , insertGameRowDuration = duration
        , insertGameRowBlueWin = blueWin
        }
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO games (
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
      )
      VALUES (
        ?,
        (SELECT id FROM game_types WHERE name = ? ORDER BY id ASC LIMIT 1),
        ?,
        ?,
        ?,
        ?,
        ?,
        (SELECT id FROM arenas WHERE name = ? ORDER BY id ASC LIMIT 1),
        ?,
        ?,
        ?,
        ?,
        ?
      )
      ON CONFLICT DO NOTHING
    |]
    insertGameRow
  mapM_ (insertGamePlayer connection hash blueWin) players
  let uuid = Analysis.replayAnalysisUuid replay
  let majorVersion = Analysis.replayAnalysisMajorVersion replay
  let minorVersion = Analysis.replayAnalysisMinorVersion replay
  let customName = Analysis.replayAnalysisCustomName replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO replays (
        id,
        major_version,
        minor_version,
        recorded_at,
        custom_name,
        duration,
        game_id
      )
      VALUES (?, ?, ?, ?, ?, ?, (SELECT id FROM games WHERE hash = ? ORDER BY id ASC LIMIT 1))
    |]
    ( uuid
    , majorVersion
    , minorVersion
    , recordedAt
    , customName
    , duration
    , show hash)
  Database.execute
    connection
    [Sql.sql|
      UPDATE uploads
      SET
        finished_parsing_at = now(),
        replay_id = ?
      WHERE id = ?
    |]
    (uuid, uploadId)

data InsertGameRow = InsertGameRow
  { insertGameRowHash :: String
  , insertGameRowGameType :: Text.Text
  , insertGameRowPlaylist :: Int
  , insertGameRowMaybeServerId :: Maybe Int
  , insertGameRowMaybeGameMode :: Maybe Int
  , insertGameRowTeamSize :: Int
  , insertGameRowIsFair :: Bool
  , insertGameRowArena :: Text.Text
  , insertGameRowBlueGoals :: Int
  , insertGameRowOrangeGoals :: Int
  , insertGameRowRecordedAt :: Time.LocalTime
  , insertGameRowDuration :: Int
  , insertGameRowBlueWin :: Bool
  } deriving (Eq, Entity.Generic, Show)

instance Sql.ToRow InsertGameRow

makeGameHash
  :: Text.Text
  -> Int
  -> Maybe Int
  -> Maybe Int
  -> Int
  -> Bool
  -> Text.Text
  -> Int
  -> Int
  -> Set.Set Analysis.PlayerAnalysis
  -> Hash.Digest Hash.SHA1
makeGameHash gameType playlist server mode size fair arena blue orange players =
  let getPlayerInfo player = if Analysis.playerAnalysisIsPresentAtEnd player
        then Just
          ( Analysis.playerAnalysisRemoteId player
          , Analysis.playerAnalysisLocalId player
          , Analysis.playerAnalysisXp player
          , Analysis.playerAnalysisIsBlue player
          )
        else Nothing
      key =
        unwords
          [ gameType & Text.unpack
          , playlist & show
          , server & show
          , mode & show
          , size & show
          , fair & show
          , arena & Text.unpack
          , blue & show
          , orange & show
          , players & Set.toAscList & Maybe.mapMaybe getPlayerInfo & show
          ]
  in Hash.hash (ByteString.pack key)

insertPlayer :: Sql.Connection -> Analysis.PlayerAnalysis -> IO ()
insertPlayer connection player = do
  let (platform, remoteId, localId) = getPlayerId player
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO platforms (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [platform]
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO players (
        platform_id,
        remote_id,
        local_id
      )
      VALUES (
        (SELECT id FROM platforms WHERE name = ? ORDER BY id ASC LIMIT 1),
        ?,
        ?
      )
      ON CONFLICT DO NOTHING
    |]
    (platform, remoteId, localId)

insertGamePlayer :: Sql.Connection
                 -> Hash.Digest Hash.SHA1
                 -> Bool
                 -> Analysis.PlayerAnalysis
                 -> IO ()
insertGamePlayer connection hash blueWin player = do
  let (platform, remoteId, localId) = getPlayerId player
  mapM_
    (insertRequiredAuxiliary connection player)
    [ ("bodies", Analysis.playerAnalysisBody)
    , ("decals", Analysis.playerAnalysisDecal)
    , ("wheels", Analysis.playerAnalysisWheels)
    , ("rocket_trails", Analysis.playerAnalysisRocketTrail)
    , ("antennas", Analysis.playerAnalysisAntenna)
    , ("toppers", Analysis.playerAnalysisTopper)
    , ("colors", Analysis.playerAnalysisPrimaryColor)
    , ("colors", Analysis.playerAnalysisAccentColor)
    , ("finishes", Analysis.playerAnalysisPrimaryFinish)
    , ("finishes", Analysis.playerAnalysisAccentFinish)
    ]
  mapM_
    (insertOptionalAuxiliary connection player)
    [ ("paints", Analysis.playerAnalysisWheelsPaint)
    , ("paints", Analysis.playerAnalysisTopperPaint)
    ]
  let isBlue = Analysis.playerAnalysisIsBlue player
  let didWin = if isBlue then blueWin else not blueWin
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO games_players (
        game_id,
        player_id,
        name,
        xp,
        is_blue,
        is_present_at_end,
        score,
        goals,
        assists,
        saves,
        shots,
        body_id,
        decal_id,
        wheel_id,
        rocket_trail_id,
        antenna_id,
        topper_id,
        wheel_paint_id,
        topper_paint_id,
        primary_color_id,
        accent_color_id,
        primary_finish_id,
        accent_finish_id,
        fov,
        height,
        angle,
        distance,
        stiffness,
        swivel_speed,
        did_win
      )
      VALUES (
        (SELECT id FROM games WHERE hash = ? ORDER BY id ASC LIMIT 1),
        (SELECT id FROM players WHERE
          platform_id = (SELECT id FROM platforms WHERE name = ? ORDER BY id ASC LIMIT 1) AND
          remote_id = ? AND
          local_id = ? ORDER BY id ASC LIMIT 1),
        ?, -- name
        ?, -- xp
        ?, -- is_blue
        ?, -- is_present_at_end
        ?, -- score
        ?, -- goals
        ?, -- assists
        ?, -- saves
        ?, -- shots
        ?, -- body_id
        ?, -- decal_id
        ?, -- wheel_id
        ?, -- rocket_trail_id
        ?, -- antenna_id
        ?, -- topper_id
        ?, -- wheel_paint_id
        ?, -- topper_paint_id
        ?, -- primary_color_id
        ?, -- accent_color_id
        ?, -- primary_finish_id
        ?, -- accent_finish_id
        ?, -- fov
        ?, -- height
        ?, -- angle
        ?, -- distance
        ?, -- stiffness
        ?, -- swivel_speed
        ? -- did_win
      )
      ON CONFLICT DO NOTHING
    |]
    [ hash & show & Sql.toField
    , platform & Sql.toField
    , remoteId & Sql.toField
    , localId & Sql.toField
    , player & Analysis.playerAnalysisName & Sql.toField
    , player & Analysis.playerAnalysisXp & Sql.toField
    , isBlue & Sql.toField
    , player & Analysis.playerAnalysisIsPresentAtEnd & Sql.toField
    , player & Analysis.playerAnalysisScore & Sql.toField
    , player & Analysis.playerAnalysisGoals & Sql.toField
    , player & Analysis.playerAnalysisAssists & Sql.toField
    , player & Analysis.playerAnalysisSaves & Sql.toField
    , player & Analysis.playerAnalysisShots & Sql.toField
    , player & Analysis.playerAnalysisBody & Sql.toField
    , player & Analysis.playerAnalysisDecal & Sql.toField
    , player & Analysis.playerAnalysisWheels & Sql.toField
    , player & Analysis.playerAnalysisRocketTrail & Sql.toField
    , player & Analysis.playerAnalysisAntenna & Sql.toField
    , player & Analysis.playerAnalysisTopper & Sql.toField
    , player & Analysis.playerAnalysisWheelsPaint & Sql.toField
    , player & Analysis.playerAnalysisTopperPaint & Sql.toField
    , player & Analysis.playerAnalysisPrimaryColor & Sql.toField
    , player & Analysis.playerAnalysisAccentColor & Sql.toField
    , player & Analysis.playerAnalysisPrimaryFinish & Sql.toField
    , player & Analysis.playerAnalysisAccentFinish & Sql.toField
    , player & Analysis.playerAnalysisFov & Sql.toField
    , player & Analysis.playerAnalysisHeight & Sql.toField
    , player & Analysis.playerAnalysisAngle & Sql.toField
    , player & Analysis.playerAnalysisDistance & Sql.toField
    , player & Analysis.playerAnalysisStiffness & Sql.toField
    , player & Analysis.playerAnalysisSwivelSpeed & Sql.toField
    , didWin & Sql.toField
    ]

insertRequiredAuxiliary
  :: Sql.Connection
  -> Analysis.PlayerAnalysis
  -> (Sql.Query, Analysis.PlayerAnalysis -> Int)
  -> IO ()
insertRequiredAuxiliary connection player (table, getValue) =
  Database.execute
    connection
    (mconcat ["INSERT INTO ", table, " (id) VALUES (?) ON CONFLICT DO NOTHING"])
    [getValue player]

insertOptionalAuxiliary
  :: Sql.Connection
  -> Analysis.PlayerAnalysis
  -> (Sql.Query, Analysis.PlayerAnalysis -> Maybe Int)
  -> IO ()
insertOptionalAuxiliary connection player (table, getValue) =
  case getValue player of
    Nothing -> pure ()
    Just value ->
      insertRequiredAuxiliary connection player (table, const value)

getPlayerId :: Analysis.PlayerAnalysis -> (Entity.PlatformName, Text.Text, Int)
getPlayerId player =
  ( player & Analysis.playerAnalysisRemoteId & toPlatform
  , player & Analysis.playerAnalysisRemoteId & toRemoteId
  , Analysis.playerAnalysisLocalId player)

toPlatform :: Rattletrap.RemoteId -> Entity.PlatformName
toPlatform remoteId =
  case remoteId of
    Rattletrap.PlayStationId _ _ -> Entity.PlayStation
    Rattletrap.SplitscreenId _ -> Entity.Splitscreen
    Rattletrap.SteamId _ -> Entity.Steam
    Rattletrap.XboxId _ -> Entity.Xbox

toRemoteId :: Rattletrap.RemoteId -> Text.Text
toRemoteId remoteId =
  case remoteId of
    Rattletrap.PlayStationId x _ -> x
    Rattletrap.SplitscreenId x -> x & show & Text.pack
    Rattletrap.SteamId x -> x & Rattletrap.word64Value & show & Text.pack
    Rattletrap.XboxId x -> x & Rattletrap.word64Value & show & Text.pack

insertError :: Sql.Connection -> Int -> Exception.SomeException -> IO ()
insertError connection uploadId exception = do
  let content = show exception
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO parse_errors (content)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [content]
  Database.execute
    connection
    [Sql.sql|
      UPDATE uploads
      SET
        finished_parsing_at = now(),
        parse_error_id = (SELECT id FROM parse_errors WHERE content = ? ORDER BY id ASC LIMIT 1)
      WHERE id = ?
    |]
    (content, uploadId)

parser :: String
parser =
  let package = "rattletrap"
      version = Version.showVersion Rattletrap.version
  in concat [package, "-", version]
