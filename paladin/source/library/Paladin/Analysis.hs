module Paladin.Analysis where

import Data.Function ((&))

import qualified Control.Monad.Catch as Catch
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Rattletrap

data ReplayAnalysis = ReplayAnalysis
  { replayAnalysisMajorVersion :: Int
  , replayAnalysisMinorVersion :: Int
  , replayAnalysisUuid :: Text.Text
  , replayAnalysisRecordedAt :: Time.LocalTime
  , replayAnalysisCustomName :: Maybe Text.Text
  , replayAnalysisGameTypeName :: Text.Text
  , replayAnalysisGameModeId :: Maybe Int
  , replayAnalysisPlaylistId :: Int
  , replayAnalysisServerId :: Maybe Int
  , replayAnalysisServerName :: Text.Text
  , replayAnalysisArenaName :: Text.Text
  , replayAnalysisTeamSize :: Int
  , replayAnalysisIsFair :: Bool
  , replayAnalysisPlayers :: [PlayerAnalysis]
  , replayAnalysisDuration :: Time.DiffTime
  , replayAnalysisBlueScore :: Int
  , replayAnalysisOrangeScore :: Int
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
  , playerAnalysisFov :: Maybe Float
  , playerAnalysisHeight :: Maybe Float
  , playerAnalysisAngle :: Maybe Float
  , playerAnalysisDistance :: Maybe Float
  , playerAnalysisStiffness :: Maybe Float
  , playerAnalysisSwivelSpeed :: Maybe Float
  } deriving (Eq, Show)

makeReplayAnalysis
  :: Catch.MonadThrow m
  => Rattletrap.Replay -> m ReplayAnalysis
makeReplayAnalysis replay = do
  majorVersion <- getMajorVersion replay
  minorVersion <- getMinorVersion replay
  uuid <- getUuid replay
  recordedAt <- getRecordedAt replay
  customName <- getCustomName replay
  pure
    ReplayAnalysis
    { replayAnalysisMajorVersion = majorVersion
    , replayAnalysisMinorVersion = minorVersion
    , replayAnalysisUuid = uuid
    , replayAnalysisRecordedAt = recordedAt
    , replayAnalysisCustomName = customName
    , replayAnalysisGameTypeName = undefined
    , replayAnalysisGameModeId = undefined
    , replayAnalysisPlaylistId = undefined
    , replayAnalysisServerId = undefined
    , replayAnalysisServerName = undefined
    , replayAnalysisArenaName = undefined
    , replayAnalysisTeamSize = undefined
    , replayAnalysisIsFair = undefined
    , replayAnalysisPlayers = undefined
    , replayAnalysisDuration = undefined
    , replayAnalysisBlueScore = undefined
    , replayAnalysisOrangeScore = undefined
    }

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
  let maybeName =
        replay & Rattletrap.replayHeader & Rattletrap.sectionBody &
        Rattletrap.headerProperties &
        Rattletrap.dictionaryValue &
        Map.lookup (Text.pack "ReplayName")
  case maybeName of
    Nothing -> pure Nothing
    Just name ->
      name & Rattletrap.propertyValue & fromStrProperty &
      fmap Rattletrap.textValue &
      fmap Just

lookupThrow
  :: Catch.MonadThrow m
  => String -> Map.Map Text.Text v -> m v
lookupThrow k m =
  case Map.lookup (Text.pack k) m of
    Just v -> pure v
    Nothing ->
      let message = "could not find key: " ++ k
      in Catch.throwM (userError message)

fromStrProperty
  :: Catch.MonadThrow m
  => Rattletrap.PropertyValue a -> m Rattletrap.Text
fromStrProperty p =
  case p of
    Rattletrap.StrProperty x -> pure x
    _ -> Catch.throwM (userError "not a StrProperty")

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
