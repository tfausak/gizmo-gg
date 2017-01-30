{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.Playlist where

import qualified Jalopy.Entity.Common as Common

data Playlist = Playlist
  { playlistId :: Common.Tagged Playlist Int
  , playlistName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Playlist

instance Common.ToJSON Playlist where
  toEncoding = Common.genericToEncoding "Common"

playlistProxy :: Common.Proxy Playlist
playlistProxy = Common.Proxy
