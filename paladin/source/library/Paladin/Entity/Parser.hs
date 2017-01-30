{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Parser where

import qualified Paladin.Entity.Common as Common

data Parser = Parser
  { parserId :: Common.Tagged Parser Int
  , parserName :: Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Parser

instance Common.ToJSON Parser where
  toEncoding = Common.genericToEncoding "Parser"

parserProxy :: Common.Proxy Parser
parserProxy = Common.Proxy
