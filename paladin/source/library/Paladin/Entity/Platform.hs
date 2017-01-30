{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Platform where

import qualified Paladin.Entity.Common as Common

data Platform = Platform
  { platformId :: Common.Tagged Platform Int
  , platformName :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Platform

instance Common.ToJSON Platform where
  toEncoding = Common.genericToEncoding "Platform"

platformProxy :: Common.Proxy Platform
platformProxy = Common.Proxy
