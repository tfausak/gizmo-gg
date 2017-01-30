{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.ParseError where

import qualified Jalopy.Entity.Common as Common

data ParseError = ParseError
  { parseErrorId :: Common.Tagged ParseError Int
  , parseErrorContent :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow ParseError

instance Common.ToJSON ParseError where
  toEncoding = Common.genericToEncoding "ParseError"

parseErrorProxy :: Common.Proxy ParseError
parseErrorProxy = Common.Proxy
