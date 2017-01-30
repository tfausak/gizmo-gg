{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Game where

import qualified Paladin.Entity.Arena as Arena
import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.GameMode as GameMode
import qualified Paladin.Entity.GameType as GameType
import qualified Paladin.Entity.Playlist as Playlist
import qualified Paladin.Entity.Server as Server

data Game = Game
  { gameId :: Common.Tagged Game Int
  , gameCreatedAt :: Common.UTCTime
  , gameHash :: Common.Tagged Common.SHA1 String
  , gameGameTypeId :: Common.Tagged GameType.GameType Int
  , gamePlaylistId :: Common.Tagged Playlist.Playlist Int
  , gameServerId :: Maybe (Common.Tagged Server.Server Int)
  , gameGameModeId :: Maybe (Common.Tagged GameMode.GameMode Int)
  , gameTeamSize :: Int
  , gameIsFair :: Bool
  , gameArenaId :: Common.Tagged Arena.Arena Int
  , gameBlueScore :: Int
  , gameOrangeScore :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Game

instance Common.ToJSON Game where
  toEncoding = Common.genericToEncoding "Game"

gameProxy :: Common.Proxy Game
gameProxy = Common.Proxy
