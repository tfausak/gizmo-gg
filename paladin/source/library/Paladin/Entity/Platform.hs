{-# LANGUAGE DeriveGeneric #-}

module Paladin.Entity.Platform where

import qualified Data.ByteString.Char8 as ByteString
import qualified Database.PostgreSQL.Simple.FromField as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Sql
import qualified Paladin.Entity.Common as Common
import qualified Text.Read as Read

data PlatformName
  = PlayStation
  | Splitscreen
  | Steam
  | Xbox
  deriving (Bounded, Enum, Eq, Read, Show)

instance Sql.FromField PlatformName where
  fromField field maybeBytes =
    if Sql.typeOid field /= Sql.typoid Sql.varchar
      then Sql.returnError Sql.Incompatible field ""
      else case maybeBytes of
             Nothing -> Sql.returnError Sql.UnexpectedNull field ""
             Just bytes ->
               case Read.readMaybe (ByteString.unpack bytes) of
                 Nothing -> Sql.returnError Sql.ConversionFailed field ""
                 Just name -> pure name

instance Sql.ToField PlatformName where
  toField name = Sql.Escape (ByteString.pack (show name))

instance Common.ToJSON PlatformName where
  toJSON name = Common.toJSON (show name)

data Platform = Platform
  { platformId :: Common.Tagged Platform Int
  , platformName :: PlatformName
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow Platform

instance Common.ToJSON Platform where
  toEncoding = Common.genericToEncoding "Platform"
  toJSON = Common.genericToJSON "Platform"

platformProxy :: Common.Proxy Platform
platformProxy = Common.Proxy
