{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.GameMode where

import qualified Paladin.Entity.Common as Common

data GameMode = GameMode
  { gameModeId :: Common.Tagged GameMode Int
  , gameModeName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GameMode

instance Common.ToJSON GameMode where
  toEncoding = Common.genericToEncoding "GameMode"

gameModeProxy :: Common.Proxy GameMode
gameModeProxy = Common.Proxy
