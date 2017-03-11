{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.GamePlayer where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.Game as Game
import qualified Paladin.Entity.Player as Player

data GamePlayer = GamePlayer
  { gamePlayerId :: Common.Tagged GamePlayer Int
  , gamePlayerGameId :: Common.Tagged Game.Game Int
  , gamePlayerPlayerId :: Common.Tagged Player.Player Int
  , gamePlayerName :: Common.Text
  , gamePlayerXp :: Int
  , gamePlayerIsBlue :: Bool
  , gamePlayerIsPresentAtEnd :: Bool
  , gamePlayerScore :: Int
  , gamePlayerGoals :: Int
  , gamePlayerAssists :: Int
  , gamePlayerSaves :: Int
  , gamePlayerShots :: Int
  , gamePlayerBodyId :: Int
  , gamePlayerDecalId :: Int
  , gamePlayerWheelId :: Int
  , gamePlayerRocketTrailId :: Int
  , gamePlayerAntennaId :: Int
  , gamePlayerTopperId :: Int
  , gamePlayerWheelPaintId :: Maybe Int
  , gamePlayerTopperPaintId :: Maybe Int
  , gamePlayerPrimaryColorId :: Int
  , gamePlayerAccentColorId :: Int
  , gamePlayerPrimaryFinishId :: Int
  , gamePlayerAccentFinishId :: Int
  , gamePlayerFov :: Float
  , gamePlayerHeight :: Float
  , gamePlayerAngle :: Float
  , gamePlayerDistance :: Float
  , gamePlayerStiffness :: Float
  , gamePlayerSwivelSpeed :: Float
  , gamePlayerDidWin :: Bool
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GamePlayer

instance Common.ToJSON GamePlayer where
  toEncoding = Common.genericToEncoding "GamePlayer"

gamePlayerProxy :: Common.Proxy GamePlayer
gamePlayerProxy = Common.Proxy
