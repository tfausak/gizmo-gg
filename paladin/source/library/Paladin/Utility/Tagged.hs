module Paladin.Utility.Tagged where

import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple.FromField as Sql

newtype Tagged tag value = Tagged
  { tagValue :: value
  } deriving (Eq, Show)

instance Sql.FromField value =>
         Sql.FromField (Tagged tag value) where
  fromField field bytes = do
    value <- Sql.fromField field bytes
    let tagged = Tagged value
    pure tagged

instance Aeson.ToJSON value =>
         Aeson.ToJSON (Tagged tag value) where
  toJSON tagged =
    let value = tagValue tagged
    in Aeson.toJSON value
