{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.ParseError where

import qualified Paladin.Entity.Common as Common

data ParseError = ParseError
  { parseErrorId :: Common.Tagged ParseError Int
  , parseErrorContent :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow ParseError

instance Common.ToJSON ParseError where
  toEncoding = Common.genericToEncoding "ParseError"

parseErrorProxy :: Common.Proxy ParseError
parseErrorProxy = Common.Proxy
