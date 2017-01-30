{-# LANGUAGE DeriveGeneric #-}

module Jalopy.Entity.Upload where

import qualified Jalopy.Entity.Common as Common
import qualified Jalopy.Entity.ParseError as ParseError
import qualified Jalopy.Entity.Parser as Parser
import qualified Jalopy.Entity.Replay as Replay

data Upload = Upload
  { uploadId :: Common.Tagged Upload Int
  , uploadCreatedAt :: Common.UTCTime
  , uploadName :: Common.Text
  , uploadSize :: Int
  , uploadHash :: Common.Tagged Common.SHA1 String
  , uploadStartedParsingAt :: Maybe Common.UTCTime
  , uploadParserId :: Maybe (Common.Tagged Parser.Parser Int)
  , uploadFinishedParsingAt :: Maybe Common.UTCTime
  , uploadParseErrorId :: Maybe (Common.Tagged ParseError.ParseError Int)
  , uploadReplayId :: Maybe (Common.Tagged Replay.Replay Common.Guid)
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Upload

instance Common.ToJSON Upload where
  toEncoding = Common.genericToEncoding "Upload"

uploadProxy :: Common.Proxy Upload
uploadProxy = Common.Proxy
