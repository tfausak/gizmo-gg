{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.GameType where

import qualified Jalopy.Entity.Common as Common

data GameType = GameType
  { gameTypeId :: Common.Tagged GameType Int
  , gameTypeName :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GameType

instance Common.ToJSON GameType where
  toEncoding = Common.genericToEncoding "GameType"

gameTypeProxy :: Common.Proxy GameType
gameTypeProxy = Common.Proxy
