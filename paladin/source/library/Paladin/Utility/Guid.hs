module Paladin.Utility.Guid where

import qualified Data.Aeson as Aeson
import qualified Data.UUID as Uuid
import qualified Database.PostgreSQL.Simple.FromField as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Sql

newtype Guid = Guid
  { guidValue :: Uuid.UUID
  } deriving (Eq, Show)

instance Sql.FromField Guid where
  fromField field maybeBytes =
    if Sql.typeOid field /= Sql.typoid Sql.uuid
      then Sql.returnError Sql.Incompatible field ""
      else case maybeBytes of
             Nothing -> Sql.returnError Sql.UnexpectedNull field ""
             Just bytes ->
               case Uuid.fromASCIIBytes bytes of
                 Nothing -> Sql.returnError Sql.ConversionFailed field ""
                 Just uuid ->
                   let guid = Guid uuid
                   in pure guid

instance Sql.ToField Guid where
  toField guid =
    let value = guidValue guid
    in Sql.toField value

instance Aeson.ToJSON Guid where
  toJSON guid =
    let uuid = guidValue guid
        text = Uuid.toText uuid
    in Aeson.String text
