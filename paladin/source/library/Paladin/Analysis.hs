module Paladin.Analysis
  ( ReplayAnalysis(..)
  , PlayerAnalysis(..)
  , makeReplayAnalysis
  ) where

import Data.Function ((&))

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Rattletrap

data ReplayAnalysis = ReplayAnalysis
  { replayAnalysisMajorVersion :: Int
  , replayAnalysisMinorVersion :: Int
  , replayAnalysisUuid :: Text.Text
  , replayAnalysisRecordedAt :: Time.LocalTime
  , replayAnalysisCustomName :: Maybe Text.Text
  , replayAnalysisGameType :: Text.Text
  , replayAnalysisGameMode :: Maybe Int
  , replayAnalysisPlaylist :: Int
  , replayAnalysisServerId :: Maybe Int
  , replayAnalysisServerName :: Text.Text
  , replayAnalysisArena :: Text.Text
  , replayAnalysisTeamSize :: Int
  , replayAnalysisIsFair :: Bool
  , replayAnalysisPlayers :: Set.Set PlayerAnalysis
  , replayAnalysisDuration :: Time.DiffTime
  , replayAnalysisBlueGoals :: Int
  , replayAnalysisOrangeGoals :: Int
  } deriving (Eq, Show)

data PlayerAnalysis = PlayerAnalysis
  { playerAnalysisIsPresentAtEnd :: Bool
  , playerAnalysisIsBlue :: Bool
  , playerAnalysisName :: Text.Text
  , playerAnalysisRemoteId :: Rattletrap.RemoteId
  , playerAnalysisLocalId :: Int
  , playerAnalysisXp :: Int
  , playerAnalysisScore :: Int
  , playerAnalysisGoals :: Int
  , playerAnalysisAssists :: Int
  , playerAnalysisSaves :: Int
  , playerAnalysisShots :: Int
  , playerAnalysisBody :: Int
  , playerAnalysisDecal :: Int
  , playerAnalysisWheels :: Int
  , playerAnalysisWheelsPaint :: Maybe Int
  , playerAnalysisRocketTrail :: Int
  , playerAnalysisAntenna :: Int
  , playerAnalysisTopper :: Int
  , playerAnalysisTopperPaint :: Maybe Int
  , playerAnalysisPrimaryColor :: Int
  , playerAnalysisAccentColor :: Int
  , playerAnalysisAccentFinish :: Int
  , playerAnalysisPrimaryFinish :: Int
  , playerAnalysisFov :: Float
  , playerAnalysisHeight :: Float
  , playerAnalysisAngle :: Float
  , playerAnalysisDistance :: Float
  , playerAnalysisStiffness :: Float
  , playerAnalysisSwivelSpeed :: Float
  } deriving (Eq, Ord, Show)

makeReplayAnalysis
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m ReplayAnalysis
makeReplayAnalysis replay = do
  majorVersion <- getMajorVersion replay
  minorVersion <- getMinorVersion replay
  uuid <- getUuid replay
  recordedAt <- getRecordedAt replay
  customName <- getCustomName replay
  gameType <- getGameType replay
  gameMode <- getGameMode replay
  playlist <- getPlaylist replay
  serverId <- getServerId replay
  serverName <- getServerName replay
  arena <- getArena replay
  teamSize <- getTeamSize replay
  isFair <- getIsFair replay
  players <- getPlayers replay
  duration <- getDuration replay
  blueGoals <- getBlueGoals replay
  orangeGoals <- getOrangeGoals replay
  pure
    ReplayAnalysis
    { replayAnalysisMajorVersion = majorVersion
    , replayAnalysisMinorVersion = minorVersion
    , replayAnalysisUuid = uuid
    , replayAnalysisRecordedAt = recordedAt
    , replayAnalysisCustomName = customName
    , replayAnalysisGameType = gameType
    , replayAnalysisGameMode = gameMode
    , replayAnalysisPlaylist = playlist
    , replayAnalysisServerId = serverId
    , replayAnalysisServerName = serverName
    , replayAnalysisArena = arena
    , replayAnalysisTeamSize = teamSize
    , replayAnalysisIsFair = isFair
    , replayAnalysisPlayers = players
    , replayAnalysisDuration = duration
    , replayAnalysisBlueGoals = blueGoals
    , replayAnalysisOrangeGoals = orangeGoals
    }

-- High-level helpers
getMajorVersion
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getMajorVersion replay =
  replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
  Rattletrap.headerEngineVersion &
  Rattletrap.word32Value &
  fromIntegral &
  pure

getMinorVersion
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getMinorVersion replay =
  replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
  Rattletrap.headerLicenseeVersion &
  Rattletrap.word32Value &
  fromIntegral &
  pure

getUuid
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Text.Text
getUuid replay = do
  property <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "Id"
  value <- property & Rattletrap.propertyValue & fromStrProperty
  value & Rattletrap.textValue & pure

getRecordedAt
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Time.LocalTime
getRecordedAt replay = do
  property <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "Date"
  value <- property & Rattletrap.propertyValue & fromStrProperty
  value & Rattletrap.textValue & Text.unpack & parseTime

getCustomName
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m (Maybe Text.Text)
getCustomName replay = do
  let maybeProperty =
        replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
        Rattletrap.headerProperties &
        Rattletrap.dictionaryValue &
        Map.lookup (Text.pack "ReplayName")
  case maybeProperty of
    Nothing -> pure Nothing
    Just property -> do
      value <- property & Rattletrap.propertyValue & fromStrProperty
      value & Rattletrap.textValue & Just & pure

getGameType
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Text.Text
getGameType replay = do
  property <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "MatchType"
  value <- property & Rattletrap.propertyValue & fromNameProperty
  value & Rattletrap.textValue & pure

getGameMode
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m (Maybe Int)
getGameMode replay = do
  let maybeAttribute =
        replay & Rattletrap.replayContent & Rattletrap.sectionBody &
        Rattletrap.contentFrames &
        concatMap Rattletrap.frameReplications &
        map Rattletrap.replicationValue &
        Maybe.mapMaybe fromUpdatedReplication &
        concatMap Rattletrap.updatedReplicationAttributes &
        filter (attributeNameIs "TAGame.GameEvent_TA:GameMode") &
        lastThrow
  case maybeAttribute of
    Nothing -> pure Nothing
    Just attribute -> do
      value <- attribute & Rattletrap.attributeValue & fromGameModeAttribute
      value & Rattletrap.gameModeAttributeWord & fromIntegral & Just & pure

getPlaylist
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getPlaylist replay = do
  attribute <-
    replay & Rattletrap.replayContent & Rattletrap.sectionBody &
    Rattletrap.contentFrames &
    concatMap Rattletrap.frameReplications &
    map Rattletrap.replicationValue &
    Maybe.mapMaybe fromUpdatedReplication &
    concatMap Rattletrap.updatedReplicationAttributes &
    filter (attributeNameIs "ProjectX.GRI_X:ReplicatedGamePlaylist") &
    lastThrow
  value <- attribute & Rattletrap.attributeValue & fromIntAttribute
  value & Rattletrap.intAttributeValue & Rattletrap.int32Value & fromIntegral &
    pure

getServerId
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m (Maybe Int)
getServerId replay = do
  let maybeAttribute =
        replay & Rattletrap.replayContent & Rattletrap.sectionBody &
        Rattletrap.contentFrames &
        concatMap Rattletrap.frameReplications &
        map Rattletrap.replicationValue &
        Maybe.mapMaybe fromUpdatedReplication &
        concatMap Rattletrap.updatedReplicationAttributes &
        filter (attributeNameIs "ProjectX.GRI_X:GameServerID") &
        lastThrow
  case maybeAttribute of
    Nothing -> pure Nothing
    Just attribute -> do
      value <- attribute & Rattletrap.attributeValue & fromQWordAttribute
      value & Rattletrap.qWordAttributeValue & Rattletrap.word64Value &
        fromIntegral &
        Just &
        pure

getServerName
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Text.Text
getServerName replay = do
  attribute <-
    replay & Rattletrap.replayContent & Rattletrap.sectionBody &
    Rattletrap.contentFrames &
    concatMap Rattletrap.frameReplications &
    map Rattletrap.replicationValue &
    Maybe.mapMaybe fromUpdatedReplication &
    concatMap Rattletrap.updatedReplicationAttributes &
    filter (attributeNameIs "Engine.GameReplicationInfo:ServerName") &
    lastThrow
  value <- attribute & Rattletrap.attributeValue & fromStringAttribute
  value & Rattletrap.stringAttributeValue & Rattletrap.textValue & pure

getArena
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Text.Text
getArena replay = do
  property <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "MapName"
  let propertyValue = Rattletrap.propertyValue property
  value <-
    case fromNameProperty propertyValue of
      Just x -> pure x
      Nothing -> fromStrProperty propertyValue
  value & Rattletrap.textValue & pure

getTeamSize
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getTeamSize replay = do
  property <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "TeamSize"
  value <- property & Rattletrap.propertyValue & fromIntProperty
  value & Rattletrap.int32Value & fromIntegral & pure

getIsFair
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Bool
getIsFair replay = do
  let maybeProperty =
        replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
        Rattletrap.headerProperties &
        Rattletrap.dictionaryValue &
        lookupThrow "bUnfairBots"
  case maybeProperty of
    Nothing -> pure defaultIsFair
    Just property -> do
      value <- property & Rattletrap.propertyValue & fromBoolProperty
      value & Rattletrap.word8Value & (/= 0) & pure

getPlayers
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m (Set.Set PlayerAnalysis)
getPlayers replay = do
  let actors =
        replay & Rattletrap.replayContent & Rattletrap.sectionBody &
        Rattletrap.contentFrames &
        concatMap Rattletrap.frameReplications &
        groupValues &
        Map.map partitionValues &
        Map.mapMaybe toActor
  let cameras = Map.mapMaybe toCamera actors
  let players = Map.mapMaybe toPlayer actors
  let cars = Map.mapMaybe toCar actors
  pure (combinePlayers cameras players cars)

getDuration
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Time.DiffTime
getDuration replay = do
  framesProperty <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "NumFrames"
  framesValue <- framesProperty & Rattletrap.propertyValue & fromIntProperty
  let frames = framesValue & Rattletrap.int32Value & fromIntegral
  rateProperty <-
    replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
    Rattletrap.headerProperties &
    Rattletrap.dictionaryValue &
    lookupThrow "RecordFPS"
  rateValue <- rateProperty & Rattletrap.propertyValue & fromFloatProperty
  let rate = rateValue & Rattletrap.float32Value
  frames / rate & realToFrac & pure

getBlueGoals
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getBlueGoals replay = do
  let maybeProperty =
        replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
        Rattletrap.headerProperties &
        Rattletrap.dictionaryValue &
        lookupThrow "Team0Score"
  case maybeProperty of
    Nothing -> pure defaultGoals
    Just property -> do
      value <- property & Rattletrap.propertyValue & fromIntProperty
      value & Rattletrap.int32Value & fromIntegral & pure

getOrangeGoals
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m Int
getOrangeGoals replay = do
  let maybeProperty =
        replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
        Rattletrap.headerProperties &
        Rattletrap.dictionaryValue &
        lookupThrow "Team1Score"
  case maybeProperty of
    Nothing -> pure defaultGoals
    Just property -> do
      value <- property & Rattletrap.propertyValue & fromIntProperty
      value & Rattletrap.int32Value & fromIntegral & pure

-- Defaults
defaultIsFair :: Bool
defaultIsFair = True

defaultGoals :: Int
defaultGoals = 0

defaultFov :: Float
defaultFov = 90.0

defaultHeight :: Float
defaultHeight = 100.0

defaultAngle :: Float
defaultAngle = -5.0

defaultDistance :: Float
defaultDistance = 240.0

defaultStiffness :: Float
defaultStiffness = 0.0

defaultSwivelSpeed :: Float
defaultSwivelSpeed = 2.5

-- Generic helpers
lastThrow
  :: Catch.MonadThrow m
  => [a] -> m a
lastThrow xs =
  case xs of
    [] -> Catch.throwM (userError "lastThrow: empty list")
    _ -> pure (last xs)

lookupThrow
  :: Catch.MonadThrow m
  => String -> Map.Map Text.Text v -> m v
lookupThrow k m =
  case Map.lookup (Text.pack k) m of
    Just v -> pure v
    Nothing ->
      let message = "could not find key: " ++ k
      in Catch.throwM (userError message)

-- Attribute converters
fromGameModeAttribute
  :: Catch.MonadThrow m
  => Rattletrap.AttributeValue -> m Rattletrap.GameModeAttribute
fromGameModeAttribute a =
  case a of
    Rattletrap.GameModeAttributeValue x -> pure x
    _ -> Catch.throwM (userError "not a GameModeAttribute")

fromIntAttribute
  :: Catch.MonadThrow m
  => Rattletrap.AttributeValue -> m Rattletrap.IntAttribute
fromIntAttribute a =
  case a of
    Rattletrap.IntAttributeValue x -> pure x
    _ -> Catch.throwM (userError "not a IntAttribute")

fromQWordAttribute
  :: Catch.MonadThrow m
  => Rattletrap.AttributeValue -> m Rattletrap.QWordAttribute
fromQWordAttribute a =
  case a of
    Rattletrap.QWordAttributeValue x -> pure x
    _ -> Catch.throwM (userError "not a QWordAttribute")

fromStringAttribute
  :: Catch.MonadThrow m
  => Rattletrap.AttributeValue -> m Rattletrap.StringAttribute
fromStringAttribute a =
  case a of
    Rattletrap.StringAttributeValue x -> pure x
    _ -> Catch.throwM (userError "not a StringAttribute")

-- Property converters
fromBoolProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Word8
fromBoolProperty p =
  case p of
    Rattletrap.BoolProperty x -> pure x
    _ -> Catch.throwM (userError "not a BoolProperty")

fromFloatProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Float32
fromFloatProperty p =
  case p of
    Rattletrap.FloatProperty x -> pure x
    _ -> Catch.throwM (userError "not a FloatProperty")

fromIntProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Int32
fromIntProperty p =
  case p of
    Rattletrap.IntProperty x -> pure x
    _ -> Catch.throwM (userError "not a IntProperty")

fromNameProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Text
fromNameProperty p =
  case p of
    Rattletrap.NameProperty x -> pure x
    _ -> Catch.throwM (userError "not a NameProperty")

fromStrProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Text
fromStrProperty p =
  case p of
    Rattletrap.StrProperty x -> pure x
    _ -> Catch.throwM (userError "not a StrProperty")

-- Replication converters
fromUpdatedReplication
  :: Catch.MonadThrow m
  => Rattletrap.ReplicationValue -> m Rattletrap.UpdatedReplication
fromUpdatedReplication r =
  case r of
    Rattletrap.UpdatedReplicationValue x -> pure x
    _ -> Catch.throwM (userError "not an UpdatedReplication")

-- Other helpers
attributeNameIs :: String -> Rattletrap.Attribute -> Bool
attributeNameIs name attribute =
  Rattletrap.attributeName attribute == Rattletrap.Text (Text.pack name)

parseTime
  :: Catch.MonadThrow m
  => String -> m Time.LocalTime
parseTime string =
  let whitespace = False
      locale = Time.defaultTimeLocale
      newFormat = "%Y-%m-%d %H-%M-%S"
      oldFormat = "%Y-%m-%d:%H-%M"
  in case Time.parseTimeM whitespace locale newFormat string of
       Just x -> pure x
       Nothing ->
         case Time.parseTimeM whitespace locale oldFormat string of
           Right x -> pure x
           Left x -> Catch.throwM (userError x)

-- Player stuff
type Values = Map.Map Int [Rattletrap.ReplicationValue]

groupValues :: [Rattletrap.Replication] -> Values
groupValues = foldr updateValueGroup Map.empty

updateValueGroup :: Rattletrap.Replication -> Values -> Values
updateValueGroup replication =
  Map.insertWith
    (++)
    (replication & Rattletrap.replicationActorId &
     Rattletrap.compressedWordValue &
     fromIntegral)
    [Rattletrap.replicationValue replication]

data Partition = Partition
  { partitionSpawned :: [Rattletrap.SpawnedReplication]
  , partitionUpdated :: [Rattletrap.UpdatedReplication]
  , partitionDestroyed :: [Rattletrap.DestroyedReplication]
  } deriving (Show)

partitionValues :: [Rattletrap.ReplicationValue] -> Partition
partitionValues = foldr updateValuePartition emptyPartition

emptyPartition :: Partition
emptyPartition =
  Partition
  {partitionSpawned = [], partitionUpdated = [], partitionDestroyed = []}

updateValuePartition :: Rattletrap.ReplicationValue -> Partition -> Partition
updateValuePartition replicationValue partition =
  case replicationValue of
    Rattletrap.SpawnedReplicationValue replication ->
      partition {partitionSpawned = replication : partitionSpawned partition}
    Rattletrap.UpdatedReplicationValue replication ->
      partition {partitionUpdated = replication : partitionUpdated partition}
    Rattletrap.DestroyedReplicationValue replication ->
      partition
      {partitionDestroyed = replication : partitionDestroyed partition}

data Actor = Actor
  { actorClassName :: Text.Text
  , actorObjectName :: Text.Text
  , actorIsDestroyed :: Bool
  , actorAttributes :: Attributes
  } deriving (Show)

toActor :: Partition -> Maybe Actor
toActor partition = do
  initial <- Maybe.listToMaybe (partitionSpawned partition)
  pure
    Actor
    { actorClassName =
        initial & Rattletrap.spawnedReplicationClassName & Rattletrap.textValue
    , actorObjectName =
        initial & Rattletrap.spawnedReplicationObjectName &
        Rattletrap.textValue
    , actorIsDestroyed = partition & partitionDestroyed & null & not
    , actorAttributes = partition & partitionUpdated & groupAttributes
    }

type Attributes = Map.Map Text.Text (Set.Set Rattletrap.AttributeValue)

groupAttributes :: [Rattletrap.UpdatedReplication] -> Attributes
groupAttributes = foldr updateAttributeGroup Map.empty

updateAttributeGroup :: Rattletrap.UpdatedReplication
                     -> Attributes
                     -> Attributes
updateAttributeGroup replication attributes =
  foldr
    insertAttribute
    attributes
    (Rattletrap.updatedReplicationAttributes replication)

insertAttribute :: Rattletrap.Attribute -> Attributes -> Attributes
insertAttribute attribute =
  Map.insertWith
    Set.union
    (attribute & Rattletrap.attributeName & Rattletrap.textValue)
    (attribute & Rattletrap.attributeValue & Set.singleton)

data Camera = Camera
  { cameraFov :: Float
  , cameraHeight :: Float
  , cameraAngle :: Float
  , cameraDistance :: Float
  , cameraStiffness :: Float
  , cameraSwivelSpeed :: Float
  } deriving (Show)

toCamera :: Actor -> Maybe Camera
toCamera actor =
  case Map.lookup
         (Text.pack "TAGame.CameraSettingsActor_TA:ProfileSettings")
         (actorAttributes actor) of
    Just xs ->
      case Set.toDescList xs of
        Rattletrap.CamSettingsAttributeValue attribute:_ ->
          Just (camSettingsToCamera attribute)
        _ -> Nothing
    _ -> Nothing

camSettingsToCamera :: Rattletrap.CamSettingsAttribute -> Camera
camSettingsToCamera attribute =
  Camera
  { cameraFov =
      attribute & Rattletrap.camSettingsAttributeFov & Rattletrap.float32Value
  , cameraHeight =
      attribute & Rattletrap.camSettingsAttributeHeight &
      Rattletrap.float32Value
  , cameraAngle =
      attribute & Rattletrap.camSettingsAttributeAngle &
      Rattletrap.float32Value
  , cameraDistance =
      attribute & Rattletrap.camSettingsAttributeDistance &
      Rattletrap.float32Value
  , cameraStiffness =
      attribute & Rattletrap.camSettingsAttributeStiffness &
      Rattletrap.float32Value
  , cameraSwivelSpeed =
      attribute & Rattletrap.camSettingsAttributeSwivelSpeed &
      Rattletrap.float32Value
  }

data Player = Player
  { playerIsDestroyed :: Bool
  , playerRemoteId :: Rattletrap.RemoteId
  , playerLocalId :: Int
  , playerCamera :: Either (Maybe Camera) Int
  , playerName :: Text.Text
  , playerXp :: Int
  , playerScore :: Int
  , playerGoals :: Int
  , playerAssists :: Int
  , playerSaves :: Int
  , playerShots :: Int
  , playerBlueLoadout :: Loadout
  , playerOrangeLoadout :: Loadout
  } deriving (Show)

toPlayer :: Actor -> Maybe Player
toPlayer actor = do
  Rattletrap.UniqueIdAttributeValue idAttribute <-
    getSingletonAttribute actor "Engine.PlayerReplicationInfo:UniqueId"
  camera <-
    case Map.lookup
           (Text.pack "TAGame.PRI_TA:PersistentCamera")
           (actorAttributes actor) of
      Just xs ->
        case Set.toDescList xs of
          Rattletrap.FlaggedIntAttributeValue x:_ ->
            x & Rattletrap.flaggedIntAttributeInt & Rattletrap.int32Value &
            fromIntegral &
            Right &
            Just
          _ -> Nothing
      Nothing ->
        case Map.lookup
               (Text.pack "TAGame.PRI_TA:CameraSettings")
               (actorAttributes actor) of
          Just xs ->
            case Set.toDescList xs of
              Rattletrap.CamSettingsAttributeValue x:_ ->
                x & camSettingsToCamera & Just & Left & Just
              _ -> Nothing
          Nothing -> Nothing & Left & Just
  Rattletrap.StringAttributeValue nameAttribute <-
    getSingletonAttribute actor "Engine.PlayerReplicationInfo:PlayerName"
  Rattletrap.IntAttributeValue xpAttribute <-
    getSingletonAttribute actor "TAGame.PRI_TA:TotalXP"
  let Rattletrap.IntAttributeValue scoreAttribute =
        getMaximumAttributeWithDefault actor zero "TAGame.PRI_TA:MatchScore"
  let Rattletrap.IntAttributeValue goalsAttribute =
        getMaximumAttributeWithDefault actor zero "TAGame.PRI_TA:MatchGoals"
  let Rattletrap.IntAttributeValue assistsAttribute =
        getMaximumAttributeWithDefault actor zero "TAGame.PRI_TA:MatchAssists"
  let Rattletrap.IntAttributeValue savesAttribute =
        getMaximumAttributeWithDefault actor zero "TAGame.PRI_TA:MatchSaves"
  let Rattletrap.IntAttributeValue shotsAttribute =
        getMaximumAttributeWithDefault actor zero "TAGame.PRI_TA:MatchShots"
  loadoutsAttribute <-
    case Map.lookup
           (Text.pack "TAGame.PRI_TA:ClientLoadouts")
           (actorAttributes actor) of
      Just xs ->
        case Set.toList xs of
          [Rattletrap.LoadoutsAttributeValue x] -> Just x
          _ -> Nothing
      _ ->
        case Map.lookup
               (Text.pack "TAGame.PRI_TA:ClientLoadout")
               (actorAttributes actor) of
          Just xs ->
            case Set.toList xs of
              [Rattletrap.LoadoutAttributeValue x] ->
                Just
                  Rattletrap.LoadoutsAttribute
                  { Rattletrap.loadoutsAttributeBlue = x
                  , Rattletrap.loadoutsAttributeOrange = x
                  }
              _ -> Nothing
          _ -> Nothing
  let onlineLoadoutsAttribute =
        case Map.lookup
               (Text.pack "TAGame.PRI_TA:ClientLoadoutsOnline")
               (actorAttributes actor) of
          Just xs ->
            case Set.toDescList xs of
              Rattletrap.LoadoutsOnlineAttributeValue x:_ -> Just x
              _ -> Nothing
          Nothing ->
            case Map.lookup
                   (Text.pack "TAGame.PRI_TA:ClientLoadoutOnline")
                   (actorAttributes actor) of
              Just xs ->
                case Set.toDescList xs of
                  Rattletrap.LoadoutOnlineAttributeValue x:_ ->
                    Just
                      Rattletrap.LoadoutsOnlineAttribute
                      { Rattletrap.loadoutsOnlineAttributeBlue = x
                      , Rattletrap.loadoutsOnlineAttributeOrange = x
                      , Rattletrap.loadoutsOnlineAttributeUnknown1 = False
                      , Rattletrap.loadoutsOnlineAttributeUnknown2 = False
                      }
                  _ -> Nothing
              _ -> Nothing
  Just
    Player
    { playerIsDestroyed = actorIsDestroyed actor
    , playerRemoteId = idAttribute & Rattletrap.uniqueIdAttributeRemoteId
    , playerLocalId =
        idAttribute & Rattletrap.uniqueIdAttributeLocalId &
        Rattletrap.word8Value &
        fromIntegral
    , playerCamera = camera
    , playerName =
        nameAttribute & Rattletrap.stringAttributeValue & Rattletrap.textValue
    , playerXp =
        xpAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerScore =
        scoreAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerGoals =
        goalsAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerAssists =
        assistsAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerSaves =
        savesAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerShots =
        shotsAttribute & Rattletrap.intAttributeValue & Rattletrap.int32Value &
        fromIntegral
    , playerBlueLoadout =
        toLoadout
          (fmap Rattletrap.loadoutsOnlineAttributeBlue onlineLoadoutsAttribute)
          (Rattletrap.loadoutsAttributeBlue loadoutsAttribute)
    , playerOrangeLoadout =
        toLoadout
          (fmap
             Rattletrap.loadoutsOnlineAttributeOrange
             onlineLoadoutsAttribute)
          (Rattletrap.loadoutsAttributeOrange loadoutsAttribute)
    }

data Loadout = Loadout
  { loadoutBody :: Int
  , loadoutDecal :: Int
  , loadoutWheels :: Int
  , loadoutWheelsPaint :: Maybe Int
  , loadoutRocketTrail :: Int
  , loadoutAntenna :: Int
  , loadoutTopper :: Int
  , loadoutTopperPaint :: Maybe Int
  } deriving (Show)

toLoadout
  :: Maybe Rattletrap.LoadoutOnlineAttribute
  -> Rattletrap.LoadoutAttribute
  -> Loadout
toLoadout maybeOnline attribute =
  Loadout
  { loadoutBody =
      attribute & Rattletrap.loadoutAttributeBody & Rattletrap.word32Value &
      fromIntegral
  , loadoutDecal =
      attribute & Rattletrap.loadoutAttributeDecal & Rattletrap.word32Value &
      fromIntegral
  , loadoutWheels =
      attribute & Rattletrap.loadoutAttributeWheels & Rattletrap.word32Value &
      fromIntegral
  , loadoutWheelsPaint =
      maybeOnline & fmap Rattletrap.loadoutAttributeValue & fmap (drop 2) &
      fmap Maybe.listToMaybe &
      Monad.join &
      fmap Maybe.listToMaybe &
      Monad.join &
      fmap snd &
      fmap Rattletrap.compressedWordValue &
      fmap fromIntegral
  , loadoutRocketTrail =
      attribute & Rattletrap.loadoutAttributeRocketTrail &
      Rattletrap.word32Value &
      fromIntegral
  , loadoutAntenna =
      attribute & Rattletrap.loadoutAttributeAntenna & Rattletrap.word32Value &
      fromIntegral
  , loadoutTopper =
      attribute & Rattletrap.loadoutAttributeTopper & Rattletrap.word32Value &
      fromIntegral
  , loadoutTopperPaint =
      maybeOnline & fmap Rattletrap.loadoutAttributeValue & fmap (drop 5) &
      fmap Maybe.listToMaybe &
      Monad.join &
      fmap Maybe.listToMaybe &
      Monad.join &
      fmap snd &
      fmap Rattletrap.compressedWordValue &
      fmap fromIntegral
  }

data Car = Car
  { carPlayerId :: Int
  , carTeam :: Int
  , carPrimaryColor :: Int
  , carAccentColor :: Int
  , carPrimaryFinish :: Int
  , carAccentFinish :: Int
  } deriving (Show)

toCar :: Actor -> Maybe Car
toCar actor = do
  Rattletrap.FlaggedIntAttributeValue playerAttribute <-
    getSingletonAttribute actor "Engine.Pawn:PlayerReplicationInfo"
  Rattletrap.TeamPaintAttributeValue paintAttribute <-
    getSingletonAttribute actor "TAGame.Car_TA:TeamPaint"
  Just
    Car
    { carPlayerId =
        playerAttribute & Rattletrap.flaggedIntAttributeInt &
        Rattletrap.int32Value &
        fromIntegral
    , carTeam =
        paintAttribute & Rattletrap.teamPaintAttributeTeam &
        Rattletrap.word8Value &
        fromIntegral
    , carPrimaryColor =
        paintAttribute & Rattletrap.teamPaintAttributePrimaryColor &
        Rattletrap.word8Value &
        fromIntegral
    , carAccentColor =
        paintAttribute & Rattletrap.teamPaintAttributeAccentColor &
        Rattletrap.word8Value &
        fromIntegral
    , carPrimaryFinish =
        paintAttribute & Rattletrap.teamPaintAttributePrimaryFinish &
        Rattletrap.word32Value &
        fromIntegral
    , carAccentFinish =
        paintAttribute & Rattletrap.teamPaintAttributeAccentFinish &
        Rattletrap.word32Value &
        fromIntegral
    }

getSingletonAttribute :: Actor -> String -> Maybe Rattletrap.AttributeValue
getSingletonAttribute actor name = do
  values <- Map.lookup (Text.pack name) (actorAttributes actor)
  case Set.toList values of
    [value] -> Just value
    _ -> Nothing

getMaximumAttribute :: Actor -> String -> Maybe Rattletrap.AttributeValue
getMaximumAttribute actor name = do
  values <- Map.lookup (Text.pack name) (actorAttributes actor)
  case Set.toDescList values of
    value:_ -> Just value
    _ -> Nothing

getMaximumAttributeWithDefault :: Actor
                               -> Rattletrap.AttributeValue
                               -> String
                               -> Rattletrap.AttributeValue
getMaximumAttributeWithDefault actor def name =
  Maybe.fromMaybe def (getMaximumAttribute actor name)

zero :: Rattletrap.AttributeValue
zero =
  Rattletrap.IntAttributeValue
    Rattletrap.IntAttribute
    { Rattletrap.intAttributeValue =
        Rattletrap.Int32 {Rattletrap.int32Value = 0}
    }

combinePlayers
  :: Map.Map Int Camera
  -> Map.Map Int Player
  -> Map.Map Int Car
  -> Set.Set PlayerAnalysis
combinePlayers cameras players =
  Map.foldr
    (\car fullPlayers ->
       case Map.lookup (carPlayerId car) players of
         Just player ->
           let maybeCamera =
                 case playerCamera player of
                   Left camera -> camera
                   Right cameraId -> Map.lookup cameraId cameras
           in Set.insert (makePlayer maybeCamera player car) fullPlayers
         Nothing -> fullPlayers)
    Set.empty

makePlayer :: Maybe Camera -> Player -> Car -> PlayerAnalysis
makePlayer maybeCamera player car =
  let team = carTeam car
      loadout =
        case team of
          1 -> playerOrangeLoadout player
          _ -> playerBlueLoadout player
  in PlayerAnalysis
     { playerAnalysisIsPresentAtEnd = not (playerIsDestroyed player)
     , playerAnalysisFov = maybe defaultFov cameraFov maybeCamera
     , playerAnalysisHeight = maybe defaultHeight cameraHeight maybeCamera
     , playerAnalysisAngle = maybe defaultAngle cameraAngle maybeCamera
     , playerAnalysisDistance =
         maybe defaultDistance cameraDistance maybeCamera
     , playerAnalysisStiffness =
         maybe defaultStiffness cameraStiffness maybeCamera
     , playerAnalysisSwivelSpeed =
         maybe defaultSwivelSpeed cameraSwivelSpeed maybeCamera
     , playerAnalysisRemoteId = playerRemoteId player
     , playerAnalysisLocalId = playerLocalId player
     , playerAnalysisName = playerName player
     , playerAnalysisXp = playerXp player
     , playerAnalysisScore = playerScore player
     , playerAnalysisGoals = playerGoals player
     , playerAnalysisAssists = playerAssists player
     , playerAnalysisSaves = playerSaves player
     , playerAnalysisShots = playerShots player
     , playerAnalysisIsBlue = carTeam car /= 1
     , playerAnalysisPrimaryColor = carPrimaryColor car
     , playerAnalysisAccentColor = carAccentColor car
     , playerAnalysisPrimaryFinish = carPrimaryFinish car
     , playerAnalysisAccentFinish = carAccentFinish car
     , playerAnalysisBody = loadoutBody loadout
     , playerAnalysisDecal = loadoutDecal loadout
     , playerAnalysisWheels = loadoutWheels loadout
     , playerAnalysisRocketTrail = loadoutRocketTrail loadout
     , playerAnalysisAntenna = loadoutAntenna loadout
     , playerAnalysisTopper = loadoutTopper loadout
     , playerAnalysisWheelsPaint = loadoutWheelsPaint loadout
     , playerAnalysisTopperPaint = loadoutTopperPaint loadout
     }
