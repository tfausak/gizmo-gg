module Paladin.Utility.Tagged where

import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple.FromField as Sql
import qualified Database.PostgreSQL.Simple.ToField as Sql

newtype Tagged tag value = Tagged
  { tagValue :: value
  } deriving (Eq, Ord, Show)

instance Sql.FromField value =>
         Sql.FromField (Tagged tag value) where
  fromField field bytes = do
    value <- Sql.fromField field bytes
    let tagged = Tagged value
    pure tagged

instance Sql.ToField value => Sql.ToField (Tagged tag value) where
  toField tagged =
    let value = tagValue tagged
    in Sql.toField value

instance Aeson.ToJSON value =>
         Aeson.ToJSON (Tagged tag value) where
  toJSON tagged =
    let value = tagValue tagged
    in Aeson.toJSON value
