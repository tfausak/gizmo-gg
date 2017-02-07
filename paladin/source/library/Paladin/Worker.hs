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
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Data.Word as Word
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
        replayAnalysis <- Analysis.makeReplayAnalysis replay
        insertReplay connection uploadId replay replayAnalysis)
    (insertError connection uploadId)

parseReplay
  :: Fail.MonadFail m
  => LazyByteString.ByteString -> m Rattletrap.Replay
parseReplay contents =
  case Rattletrap.decodeReplay contents of
    Left message -> fail message
    Right replay -> pure replay

insertReplay :: Sql.Connection
             -> Int
             -> Rattletrap.Replay
             -> Analysis.ReplayAnalysis
             -> IO ()
insertReplay connection uploadId replay replayAnalysis = do
  let arena = Analysis.replayAnalysisArena replayAnalysis
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO arenas (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [arena]
  let maybeServerId = Analysis.replayAnalysisServerId replayAnalysis
  let serverName = Analysis.replayAnalysisServerName replayAnalysis
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
  let playlist = Analysis.replayAnalysisPlaylist replayAnalysis
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO playlists (id)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [playlist]
  let maybeGameMode = Analysis.replayAnalysisGameMode replayAnalysis
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
  let gameType = Analysis.replayAnalysisGameType replayAnalysis
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO game_types (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [gameType]
  players <- getPlayers replay
  mapM_ (insertPlayer connection) players
  let teamSize = Analysis.replayAnalysisTeamSize replayAnalysis
  let isFair = Analysis.replayAnalysisIsFair replayAnalysis
  let blueScore = Analysis.replayAnalysisBlueScore replayAnalysis
  let orangeScore = Analysis.replayAnalysisOrangeScore replayAnalysis
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
  let uuid = Analysis.replayAnalysisUuid replayAnalysis
  let majorVersion = Analysis.replayAnalysisMajorVersion replayAnalysis
  let minorVersion = Analysis.replayAnalysisMinorVersion replayAnalysis
  let recordedAt = Analysis.replayAnalysisRecordedAt replayAnalysis
  let customName = Analysis.replayAnalysisCustomName replayAnalysis
  duration <- getDuration replay
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
  -> NonEmpty.NonEmpty Player3
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
          , players & NonEmpty.toList & List.sort & show
          ]
  in Hash.hash (ByteString.pack key)

type Player3 = (Rattletrap.UniqueIdAttribute, Text.Text, Int)

data PlatformName
  = PlayStation
  | Splitscreen
  | Steam
  | Xbox
  deriving (Eq, Show)

instance Sql.ToField PlatformName where
  toField name = name & show & ByteString.pack & Sql.Escape

insertPlayer :: Sql.Connection -> Player3 -> IO ()
insertPlayer connection (playerId, _playerName, _playerXp) = do
  let (platform, remoteId, localId) = splitPlayerId playerId
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

insertGamePlayer :: Sql.Connection -> Hash.Digest Hash.SHA1 -> Player3 -> IO ()
insertGamePlayer connection hash (playerId, _playerName, _playerXp) = do
  let (platform, remoteId, localId) = splitPlayerId playerId
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO games_players (
        game_id,
        player_id
      )
      VALUES (
        (SELECT id FROM games WHERE hash = ?),
        (SELECT id FROM players WHERE
          platform_id = (SELECT id FROM platforms WHERE name = ?) AND
          remote_id = ? AND
          local_id = ?)
      )
      ON CONFLICT DO NOTHING
    |]
    (show hash, platform, remoteId, localId)

splitPlayerId :: Rattletrap.UniqueIdAttribute
              -> (PlatformName, Text.Text, Word.Word8)
splitPlayerId playerId =
  (getPlatform playerId, getRemoteId playerId, getLocalId playerId)

getPlatform :: Rattletrap.UniqueIdAttribute -> PlatformName
getPlatform playerId =
  case Rattletrap.uniqueIdAttributeRemoteId playerId of
    Rattletrap.PlayStationId _ _ -> PlayStation
    Rattletrap.SplitscreenId _ -> Splitscreen
    Rattletrap.SteamId _ -> Steam
    Rattletrap.XboxId _ -> Xbox

getRemoteId :: Rattletrap.UniqueIdAttribute -> Text.Text
getRemoteId playerId =
  case Rattletrap.uniqueIdAttributeRemoteId playerId of
    Rattletrap.PlayStationId x _ -> x
    Rattletrap.SplitscreenId x -> x & show & Text.pack
    Rattletrap.SteamId x -> x & Rattletrap.word64Value & show & Text.pack
    Rattletrap.XboxId x -> x & Rattletrap.word64Value & show & Text.pack

getLocalId :: Rattletrap.UniqueIdAttribute -> Word.Word8
getLocalId playerId =
  playerId & Rattletrap.uniqueIdAttributeLocalId & Rattletrap.word8Value

getPlayers
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m (NonEmpty.NonEmpty Player3)
getPlayers replay =
  replay & Rattletrap.replayContent & Rattletrap.sectionBody &
  Rattletrap.contentFrames &
  concatMap Rattletrap.frameReplications &
  map Rattletrap.replicationValue &
  Maybe.mapMaybe getUpdatedReplicationValue &
  map Rattletrap.updatedReplicationAttributes &
  Maybe.mapMaybe
    (\attributes -> do
       idAttribute <-
         findAttribute attributes "Engine.PlayerReplicationInfo:UniqueId"
       playerId <-
         case idAttribute of
           Rattletrap.UniqueIdAttributeValue x -> pure x
           _ -> fail "player id is wrong type"
       nameAttribute <-
         findAttribute attributes "Engine.PlayerReplicationInfo:PlayerName"
       playerName <-
         case nameAttribute of
           Rattletrap.StringAttributeValue x ->
             x & Rattletrap.stringAttributeValue & fromText & pure
           _ -> fail "player name is wrong type"
       xpAttribute <- findAttribute attributes "TAGame.PRI_TA:TotalXP"
       playerXp <-
         case xpAttribute of
           Rattletrap.IntAttributeValue x ->
             x & Rattletrap.intAttributeValue & fromInt32 & pure
           _ -> fail "player xp is wrong type"
       Just (playerId, playerName, playerXp)) &
  Set.fromList &
  Set.toAscList &
  NonEmpty.nonEmpty &
  maybe (fail "no players") pure

findAttribute
  :: Fail.MonadFail m
  => [Rattletrap.Attribute] -> Text.Text -> m Rattletrap.AttributeValue
findAttribute attributes name =
  attributes & filter (attributeNameIs name) & Maybe.listToMaybe &
  fmap Rattletrap.attributeValue &
  maybe (fail ("could not find attribute " ++ show name)) pure

getDuration
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getDuration replay = do
  let header = getHeader replay
  numFrames <- getIntProperty "NumFrames" header
  framesPerSecond <- getFloatProperty "RecordFPS" header
  let exactDuration = fromIntegral (numFrames :: Int) / framesPerSecond
  pure (round exactDuration)

getUpdatedReplicationValue :: Rattletrap.ReplicationValue
                           -> Maybe Rattletrap.UpdatedReplication
getUpdatedReplicationValue replication =
  case replication of
    Rattletrap.UpdatedReplicationValue value -> Just value
    _ -> Nothing

attributeNameIs :: Text.Text -> Rattletrap.Attribute -> Bool
attributeNameIs name attribute =
  Rattletrap.attributeName attribute == Rattletrap.Text name

getHeader :: Rattletrap.Replay -> Rattletrap.Header
getHeader replay = replay & Rattletrap.replayHeader & Rattletrap.sectionBody

getProperty
  :: Fail.MonadFail m
  => Text.Text -> Rattletrap.Header -> m Rattletrap.Property
getProperty name header =
  header & Rattletrap.headerProperties & Rattletrap.dictionaryValue &
  Map.lookup name &
  maybe (fail ("could not find " ++ show name ++ " property")) pure

getIntProperty
  :: (Integral a, Fail.MonadFail m)
  => Text.Text -> Rattletrap.Header -> m a
getIntProperty name header = do
  property <- header & getProperty name
  case Rattletrap.propertyValue property of
    Rattletrap.IntProperty x -> pure (fromInt32 x)
    _ -> fail (show name ++ " property is not a Int")

getFloatProperty
  :: (Fail.MonadFail m)
  => Text.Text -> Rattletrap.Header -> m Float
getFloatProperty name header = do
  property <- header & getProperty name
  case Rattletrap.propertyValue property of
    Rattletrap.FloatProperty x -> pure (fromFloat32 x)
    _ -> fail (show name ++ " property is not a Float")

fromText :: Rattletrap.Text -> Text.Text
fromText text = text & Rattletrap.textValue

fromInt32
  :: Integral a
  => Rattletrap.Int32 -> a
fromInt32 int32 = int32 & Rattletrap.int32Value & fromIntegral

fromFloat32 :: Rattletrap.Float32 -> Float
fromFloat32 float32 = float32 & Rattletrap.float32Value

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
