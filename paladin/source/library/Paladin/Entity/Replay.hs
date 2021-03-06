{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Replay where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.Game as Game

type ReplayId = Common.Tagged Replay Common.Guid

data Replay = Replay
  { replayId :: ReplayId
  , replayCreatedAt :: Common.UTCTime
  , replayMajorVersion :: Int
  , replayMinorVersion :: Int
  , replayRecordedAt :: Common.LocalTime
  , replayCustomName :: Maybe Common.Text
  , replayDuration :: Int
  , replayGameId :: Common.Tagged Game.Game Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Replay

instance Common.ToJSON Replay where
  toEncoding = Common.genericToEncoding "Replay"

replayProxy :: Common.Proxy Replay
replayProxy = Common.Proxy
