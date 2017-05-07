{-# LANGUAGE DeriveGeneric #-}

module Paladin.Rank
  ( Single(..)
  , Skills(..)
  , Skill(..)
  , getPlayerSkills
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

getPlayerSkills
  :: Client.Manager
  -> String
  -> String
  -> String
  -> IO (Either String (Single Skills))
getPlayerSkills manager apiToken platform player = do
  let url = concat ["https://api.rocketleaguegame.com/api/v1/" , platform , "/playerskills/" , player , "/"]
  initialRequest <- Client.parseRequest url
  let request = initialRequest { Client.requestHeaders = [(HTTP.hAuthorization, bs ("Token " ++ apiToken))] }
  response <- Client.httpLbs request manager
  let body = Client.responseBody response
  pure (Aeson.eitherDecode body)

newtype Single a = Single
  { singleValue :: a
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (Single a) where
  parseJSON = Aeson.withArray "Single" (\a -> case Vector.toList a of
    [v] -> do
      x <- Aeson.parseJSON v
      pure (Single x)
    _ -> fail ("invalid Single: " ++ show a))

newtype Skills = Skills
  { skillsPlayerSkills :: [Skill]
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Skills where
  parseJSON = Aeson.genericParseJSON (jsonOptions "skills")

data Skill = Skill
  { skillDivision :: Integer
  , skillMatchesPlayed :: Integer
  , skillPlaylist :: Integer
  , skillSkill :: Integer
  , skillTier :: Integer
  , skillTierMax :: Integer
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Skill where
  parseJSON = Aeson.genericParseJSON (jsonOptions "skill")

jsonOptions :: String -> Aeson.Options
jsonOptions prefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . dropPrefix prefix
  }

dropPrefix :: String -> String -> String
dropPrefix prefix string =
  if prefix `List.isPrefixOf` string
    then drop (length prefix) string
    else error ("dropPrefix: " ++ show prefix ++ " is not a prefix of " ++ show string)

bs :: String -> ByteString.ByteString
bs = Encoding.encodeUtf8 . t

t :: String -> Text.Text
t = Text.pack
