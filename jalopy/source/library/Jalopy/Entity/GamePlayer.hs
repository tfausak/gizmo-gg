{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.GamePlayer where

import qualified Jalopy.Entity.Common as Common
import qualified Jalopy.Entity.Game as Game
import qualified Jalopy.Entity.Player as Player

data GamePlayer = GamePlayer
  { gamePlayerId :: Common.Tagged GamePlayer Int
  , gamePlayerGameId :: Common.Tagged Game.Game Int
  , gamePlayerPlayerId :: Common.Tagged Player.Player Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GamePlayer

instance Common.ToJSON GamePlayer where
  toEncoding = Common.genericToEncoding "GamePlayer"

gamePlayerProxy :: Common.Proxy GamePlayer
gamePlayerProxy = Common.Proxy
