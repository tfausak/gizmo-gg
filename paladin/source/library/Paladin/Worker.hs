{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Worker where

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
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Data.Word as Word
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.SqlQQ as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
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
    Sql.query_
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
  case uploads of
    [] -> sleep 1
    _ -> mapM_ (parseUpload config connection) uploads
  parseUploads config connection

parseUpload
  :: Config.Config
  -> Sql.Connection
  -> (Int, Utility.Tagged Hash.SHA1 String)
  -> IO ()
parseUpload config connection (uploadId, hash) = do
  Exception.catch
    (do contents <- Storage.getUploadFile config hash
        replay <- parseReplay contents
        insertReplay connection uploadId replay)
    (insertError connection uploadId)

parseReplay
  :: Fail.MonadFail m
  => LazyByteString.ByteString -> m Rattletrap.Replay
parseReplay contents =
  case Rattletrap.decodeReplay contents of
    Left message -> fail message
    Right replay -> pure replay

insertReplay :: Sql.Connection -> Int -> Rattletrap.Replay -> IO ()
insertReplay connection uploadId replay = do
  arena <- getArenaName replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO arenas (name)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [arena]
  (maybeServerId, serverName) <- getServer replay
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
  playlist <- getPlaylist replay
  Database.execute
    connection
    [Sql.sql|
      INSERT INTO playlists (id)
      VALUES (?)
      ON CONFLICT DO NOTHING
    |]
    [playlist]
  maybeGameMode <- getGameMode replay
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
  gameType <- getGameType replay
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
  teamSize <- getTeamSize replay
  let isFair = Maybe.fromMaybe True (getIsFair replay)
  let blueScore = Maybe.fromMaybe 0 (getBlueScore replay)
  let orangeScore = Maybe.fromMaybe 0 (getOrangeScore replay)
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
  uuid <- getUuid replay
  majorVersion <- getMajorVersion replay
  minorVersion <- getMinorVersion replay
  recordedAt <- getRecordedAt replay
  customName <- getCustomName replay
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

getTeamSize
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getTeamSize replay = replay & getHeader & getIntProperty "TeamSize"

getIsFair
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Bool
getIsFair replay = replay & getHeader & getBoolProperty "bUnfairBots"

getBlueScore
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getBlueScore replay = replay & getHeader & getIntProperty "Team0Score"

getOrangeScore
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getOrangeScore replay = replay & getHeader & getIntProperty "Team1Score"

getBoolProperty
  :: Fail.MonadFail m
  => Text.Text -> Rattletrap.Header -> m Bool
getBoolProperty name header = do
  property <- header & getProperty name
  case Rattletrap.propertyValue property of
    Rattletrap.BoolProperty x -> x & Rattletrap.word8Value & (/= 0) & pure
    _ -> fail (show name ++ " property is not a Bool")

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

getGameType
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Text.Text
getGameType replay = do
  let header = getHeader replay
  getNameProperty "MatchType" header

getGameMode
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m (Maybe Int)
getGameMode replay = do
  let attributes = getAttributes "TAGame.GameEvent_TA:GameMode" replay
  case Set.toList attributes of
    [] -> pure Nothing
    [attribute] ->
      case Rattletrap.attributeValue attribute of
        Rattletrap.GameModeAttributeValue x ->
          x & Rattletrap.gameModeAttributeWord & fromWord8 & Just & pure
        _ -> fail "game mode is not a word"
    _ -> fail "more than one game mode"

getDuration
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getDuration replay = do
  let header = getHeader replay
  numFrames <- getIntProperty "NumFrames" header
  framesPerSecond <- getFloatProperty "RecordFPS" header
  let exactDuration = fromIntegral (numFrames :: Int) / framesPerSecond
  pure (round exactDuration)

getPlaylist
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getPlaylist replay = do
  let attributes = getAttributes "ProjectX.GRI_X:ReplicatedGamePlaylist" replay
  case Set.toList attributes of
    [] -> fail "no playlist"
    [attribute] ->
      case Rattletrap.attributeValue attribute of
        Rattletrap.IntAttributeValue x ->
          x & Rattletrap.intAttributeValue & fromInt32 & pure
        _ -> fail "playlist is not an int"
    _ -> fail "more than one playlist"

getServer
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m (Maybe Int, Text.Text)
getServer replay = do
  let idAttributes = getAttributes "ProjectX.GRI_X:GameServerID" replay
  serverId <-
    case Set.toList idAttributes of
      [] -> pure Nothing
      [attribute] ->
        case Rattletrap.attributeValue attribute of
          Rattletrap.QWordAttributeValue x ->
            x & Rattletrap.qWordAttributeValue & fromWord64 & Just & pure
          _ -> fail "server ID is not a qword"
      _ -> fail "more than one server ID"
  let nameAttributes =
        getAttributes "Engine.GameReplicationInfo:ServerName" replay
  serverName <-
    case Set.toList nameAttributes of
      [] -> fail "no server name"
      [attribute] ->
        case Rattletrap.attributeValue attribute of
          Rattletrap.StringAttributeValue x ->
            x & Rattletrap.stringAttributeValue & fromText & pure
          _ -> fail "server name is not a string"
      _ -> fail "more than one server name"
  pure (serverId, serverName)

getAttributes :: Text.Text -> Rattletrap.Replay -> Set.Set Rattletrap.Attribute
getAttributes name replay =
  replay & Rattletrap.replayContent & Rattletrap.sectionBody &
  Rattletrap.contentFrames &
  concatMap Rattletrap.frameReplications &
  map Rattletrap.replicationValue &
  Maybe.mapMaybe getUpdatedReplicationValue &
  concatMap Rattletrap.updatedReplicationAttributes &
  filter (attributeNameIs name) &
  Set.fromList

getUpdatedReplicationValue :: Rattletrap.ReplicationValue
                           -> Maybe Rattletrap.UpdatedReplication
getUpdatedReplicationValue replication =
  case replication of
    Rattletrap.UpdatedReplicationValue value -> Just value
    _ -> Nothing

attributeNameIs :: Text.Text -> Rattletrap.Attribute -> Bool
attributeNameIs name attribute =
  Rattletrap.attributeName attribute == Rattletrap.Text name

getArenaName
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Text.Text
getArenaName replay = do
  let header = getHeader replay
  case getNameProperty "MapName" header of
    Just arena -> pure arena
    _ -> getStrProperty "MapName" header

getUuid
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Text.Text
getUuid replay = getStrProperty "Id" (getHeader replay)

getMajorVersion
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getMajorVersion replay =
  replay & getHeader & Rattletrap.headerEngineVersion & fromWord32 & pure

getMinorVersion
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Int
getMinorVersion replay =
  replay & getHeader & Rattletrap.headerLicenseeVersion & fromWord32 & pure

getRecordedAt
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m Time.LocalTime
getRecordedAt replay = do
  let header = getHeader replay
  rawDate <- getStrProperty "Date" header
  parseTime (Text.unpack rawDate)

getCustomName
  :: Fail.MonadFail m
  => Rattletrap.Replay -> m (Maybe Text.Text)
getCustomName replay = do
  let header = getHeader replay
  case getProperty "ReplayName" header of
    Just nameProperty -> do
      nameText <-
        case Rattletrap.propertyValue nameProperty of
          Rattletrap.StrProperty x -> pure x
          _ -> fail "ReplayName property is not a string"
      let name = fromText nameText
      pure (Just name)
    _ -> pure Nothing

getHeader :: Rattletrap.Replay -> Rattletrap.Header
getHeader replay = replay & Rattletrap.replayHeader & Rattletrap.sectionBody

getProperty
  :: Fail.MonadFail m
  => Text.Text -> Rattletrap.Header -> m Rattletrap.Property
getProperty name header =
  header & Rattletrap.headerProperties & Rattletrap.dictionaryValue &
  Map.lookup name &
  maybe (fail ("could not find " ++ show name ++ " property")) pure

getStrProperty
  :: Fail.MonadFail m
  => Text.Text -> Rattletrap.Header -> m Text.Text
getStrProperty name header = do
  property <- header & getProperty name
  case Rattletrap.propertyValue property of
    Rattletrap.StrProperty text -> pure (fromText text)
    _ -> fail (show name ++ " property is not a Str")

getNameProperty
  :: Fail.MonadFail m
  => Text.Text -> Rattletrap.Header -> m Text.Text
getNameProperty name header = do
  property <- header & getProperty name
  case Rattletrap.propertyValue property of
    Rattletrap.NameProperty text -> pure (fromText text)
    _ -> fail (show name ++ " property is not a Name")

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

parseTime
  :: Fail.MonadFail m
  => String -> m Time.LocalTime
parseTime string =
  let newFormat = "%Y-%m-%d %H-%M-%S"
      oldFormat = "%Y-%m-%d:%H-%M"
  in case Time.parseTimeM False Time.defaultTimeLocale newFormat string of
       Just x -> pure x
       _ ->
         case Time.parseTimeM False Time.defaultTimeLocale oldFormat string of
           Right x -> pure x
           Left x -> fail x

fromText :: Rattletrap.Text -> Text.Text
fromText text = text & Rattletrap.textValue

fromWord64
  :: Integral a
  => Rattletrap.Word64 -> a
fromWord64 word64 = word64 & Rattletrap.word64Value & fromIntegral

fromWord32
  :: Integral a
  => Rattletrap.Word32 -> a
fromWord32 word32 = word32 & Rattletrap.word32Value & fromIntegral

fromInt32
  :: Integral a
  => Rattletrap.Int32 -> a
fromInt32 int32 = int32 & Rattletrap.int32Value & fromIntegral

fromWord8
  :: Integral a
  => Word.Word8 -> a
fromWord8 word8 = word8 & fromIntegral

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
