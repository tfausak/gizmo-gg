{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.GameType where

import qualified Paladin.Entity.Common as Common

data GameType = GameType
  { gameTypeId :: Common.Tagged GameType Int
  , gameTypeName :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GameType

instance Common.ToJSON GameType where
  toEncoding = Common.genericToEncoding "GameType"

gameTypeProxy :: Common.Proxy GameType
gameTypeProxy = Common.Proxy
