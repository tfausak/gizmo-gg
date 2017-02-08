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
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.SqlQQ as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Paladin.Analysis as Analysis
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Storage as Storage
import qualified Paladin.Utility as Utility
import qualified Rattletrap

startWorker :: Config.Config -> Sql.Connection -> IO Concurrent.ThreadId
startWorker config connection = do
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO parsers (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [parser]
  Concurrent.forkIO (parseUploads config connection)

parseUploads :: Config.Config -> Sql.Connection -> IO ()
parseUploads config connection = do
  uploads <-
    Sql.query
      connection
      [Sql.sql|
        UPDATE uploads
        SET
          started_parsing_at = now(),
          parser_id = (SELECT id FROM parsers WHERE name = ?)
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
    [] -> sleep 1
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
    _ -> pure ()
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
  let blueScore = Analysis.replayAnalysisBlueScore replay
  let orangeScore = Analysis.replayAnalysisOrangeScore replay
  let hash =
        makeGameHash
          gameType
          playlist
          maybeServerId
          maybeGameMode
          teamSize
          isFair
          arena
          blueScore
          orangeScore
          players
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
        blue_score,
        orange_score
      )
      VALUES (
        ?,
        (SELECT id FROM game_types WHERE name = ?),
        ?,
        ?,
        ?,
        ?,
        ?,
        (SELECT id FROM arenas WHERE name = ?),
        ?,
        ?
      )
      ON CONFLICT DO NOTHING
    |]
    ( show hash
    , gameType
    , playlist
    , maybeServerId
    , maybeGameMode
    , teamSize
    , isFair
    , arena
    , blueScore
    , orangeScore)
  mapM_ (insertGamePlayer connection hash) players
  let uuid = Analysis.replayAnalysisUuid replay
  let majorVersion = Analysis.replayAnalysisMajorVersion replay
  let minorVersion = Analysis.replayAnalysisMinorVersion replay
  let recordedAt = Analysis.replayAnalysisRecordedAt replay
  let customName = Analysis.replayAnalysisCustomName replay
  let duration = Analysis.replayAnalysisDuration replay
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
      VALUES (?, ?, ?, ?, ?, ?, (SELECT id FROM games WHERE hash = ?))
    |]
    ( uuid
    , majorVersion
    , minorVersion
    , recordedAt
    , customName
    , round duration :: Int
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
  let key =
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
          , players & Set.toAscList & show
          ]
  in Hash.hash (ByteString.pack key)

data PlatformName
  = PlayStation
  | Splitscreen
  | Steam
  | Xbox
  deriving (Eq, Show)

instance Sql.ToField PlatformName where
  toField name = name & show & ByteString.pack & Sql.Escape

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
        (SELECT id FROM platforms WHERE name = ?),
        ?,
        ?
      )
      ON CONFLICT DO NOTHING
    |]
    (platform, remoteId, localId)

insertGamePlayer :: Sql.Connection
                 -> Hash.Digest Hash.SHA1
                 -> Analysis.PlayerAnalysis
                 -> IO ()
insertGamePlayer connection hash player = do
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
        swivel_speed
      )
      VALUES (
        (SELECT id FROM games WHERE hash = ?),
        (SELECT id FROM players WHERE
          platform_id = (SELECT id FROM platforms WHERE name = ?) AND
          remote_id = ? AND
          local_id = ?),
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
        ? -- swivel_speed
      )
      ON CONFLICT DO NOTHING
    |]
    [ hash & show & Sql.toField
    , platform & Sql.toField
    , remoteId & Sql.toField
    , localId & Sql.toField
    , player & Analysis.playerAnalysisName & Sql.toField
    , player & Analysis.playerAnalysisXp & Sql.toField
    , player & Analysis.playerAnalysisIsBlue & Sql.toField
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

getPlayerId :: Analysis.PlayerAnalysis -> (Text.Text, Text.Text, Int)
getPlayerId player =
  ( player & Analysis.playerAnalysisRemoteId & toPlatform
  , player & Analysis.playerAnalysisRemoteId & toRemoteId
  , Analysis.playerAnalysisLocalId player)

toPlatform :: Rattletrap.RemoteId -> Text.Text
toPlatform remoteId =
  case remoteId of
    Rattletrap.PlayStationId _ _ -> Text.pack "PlayStation"
    Rattletrap.SplitscreenId _ -> Text.pack "Splitscreen"
    Rattletrap.SteamId _ -> Text.pack "Steam"
    Rattletrap.XboxId _ -> Text.pack "Xbox"

toRemoteId :: Rattletrap.RemoteId -> Text.Text
toRemoteId remoteId =
  case remoteId of
    Rattletrap.PlayStationId x _ -> x
    Rattletrap.SplitscreenId x -> x & show & Text.pack
    Rattletrap.SteamId x -> x & Rattletrap.word64Value & show & Text.pack
    Rattletrap.XboxId x -> x & Rattletrap.word64Value & show & Text.pack

sleep :: Int -> IO ()
sleep seconds = Concurrent.threadDelay (seconds * 1000000)

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
        parse_error_id = (SELECT id FROM parse_errors WHERE content = ?)
      WHERE id = ?
    |]
    (content, uploadId)

parser :: String
parser =
  let package = "rattletrap"
      version = Version.showVersion Rattletrap.version
  in concat [package, "-", version]
