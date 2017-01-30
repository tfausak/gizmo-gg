{-# LANGUAGE FlexibleContexts #-}

module Jalopy.Entity.Common
  ( module Export
  , module Jalopy.Entity.Common
  ) where

import Crypto.Hash as Export (SHA1)
import Data.Aeson as Export (ToJSON(toEncoding))
import Data.Proxy as Export (Proxy(Proxy))
import Data.Text as Export (Text)
import Data.Time as Export (LocalTime, UTCTime)
import Database.PostgreSQL.Simple as Export (FromRow)
import GHC.Generics as Export (Generic)
import Jalopy.Utility as Export (Guid, Tagged)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as Casing
import qualified GHC.Generics as Generics

genericToEncoding
  :: (Generic a, Aeson.GToEncoding (Generics.Rep a))
  => String -> a -> Aeson.Encoding
genericToEncoding name =
  let size = length name
      options = Casing.aesonDrop size Casing.camelCase
  in Aeson.genericToEncoding options
