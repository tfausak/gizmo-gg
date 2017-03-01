{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Body where

import qualified Paladin.Entity.Common as Common

data Body = Body
  { bodyId :: Common.Tagged Body Int
  , bodyName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Body

instance Common.ToJSON Body where
  toEncoding = Common.genericToEncoding "Body"

bodyProxy :: Common.Proxy Body
bodyProxy = Common.Proxy
