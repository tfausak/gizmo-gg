{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Upload where

import qualified Paladin.Entity.Common as Common
import qualified Paladin.Entity.ParseError as ParseError
import qualified Paladin.Entity.Parser as Parser
import qualified Paladin.Entity.Replay as Replay

type UploadId = Common.Tagged Upload Int

data Upload = Upload
  { uploadId :: UploadId
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
