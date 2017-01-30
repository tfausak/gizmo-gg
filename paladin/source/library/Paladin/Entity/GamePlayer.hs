{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.GamePlayer where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.Game as Game
import qualified Paladin.Entity.Player as Player

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
