{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.Game where

import qualified Jalopy.Entity.Arena as Arena
import qualified Jalopy.Entity.Common as Common
import qualified Jalopy.Entity.GameMode as GameMode
import qualified Jalopy.Entity.GameType as GameType
import qualified Jalopy.Entity.Playlist as Playlist
import qualified Jalopy.Entity.Server as Server

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
