{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.Player where

import qualified Jalopy.Entity.Common as Common
import qualified Jalopy.Entity.Platform as Platform

data Player = Player
  { playerId :: Common.Tagged Player Int
  , playerCreatedAt :: Common.UTCTime
  , playerPlatformId :: Common.Tagged Platform.Platform Int
  , playerRemoteId :: Common.Text
  , playerLocalId :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Player

instance Common.ToJSON Player where
  toEncoding = Common.genericToEncoding "Player"

playerProxy :: Common.Proxy Player
playerProxy = Common.Proxy
