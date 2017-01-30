{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Server where

import qualified Paladin.Entity.Common as Common

data Server = Server
  { serverId :: Common.Tagged Server Int
  , serverName :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Server

instance Common.ToJSON Server where
  toEncoding = Common.genericToEncoding "Server"

serverProxy :: Common.Proxy Server
serverProxy = Common.Proxy
