module Paladin.Analysis where

import qualified Control.Monad.Catch as Catch
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
makeReplayAnalysis _replay = undefined
