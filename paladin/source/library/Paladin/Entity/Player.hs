{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Player where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.Platform as Platform

type PlayerId = Common.Tagged Player Int

data Player = Player
  { playerId :: PlayerId
  , playerCreatedAt :: Common.UTCTime
  , playerPlatformId :: Platform.PlatformId
  , playerRemoteId :: Common.Text
  , playerLocalId :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Player

instance Common.ToJSON Player where
  toEncoding = Common.genericToEncoding "Player"

playerProxy :: Common.Proxy Player
playerProxy = Common.Proxy
