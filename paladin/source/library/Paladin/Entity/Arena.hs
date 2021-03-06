{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Arena where

import qualified Paladin.Entity.Common as Common

data Arena = Arena
  { arenaId :: Common.Tagged Arena Int
  , arenaName :: Common.Text
  , arenaTemplateId :: Maybe Int
  , arenaTemplateName :: Maybe Common.Text
  , arenaModelId :: Maybe Int
  , arenaModelName :: Maybe Common.Text
  , arenaSkinId :: Maybe Int
  , arenaSkinName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Arena

instance Common.ToJSON Arena where
  toEncoding = Common.genericToEncoding "Arena"

arenaProxy :: Common.Proxy Arena
arenaProxy = Common.Proxy
