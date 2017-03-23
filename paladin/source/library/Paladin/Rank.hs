{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Rank
  ( Skill(..)
  , getPlayerSkills
  , keepSessionAlive
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as Casing
import qualified Data.Aeson.QQ as QQ
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Client as Client
import qualified Paladin.Utility as Utility

getPlayerSkills :: Client.Manager -> String -> String -> String -> IO [Skill]
getPlayerSkills manager sessionId platform player = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/Services"
  let
    playerId = concat [platform, "|", player, "|", "0"]
    request = initialRequest
      { Client.requestHeaders =
        [ ("BuildID", "-1677331153")
        , ("Cache-Control", "no-cache")
        , ("Content-Type", "application/x-www-form-urlencoded")
        , ("Environment", "Prod")
        -- This value is likely incorrect because it was part of a request
        -- for a different player.
        , ("PsySig", "l/R4TlCv7/XPyrRY9p9RzpMiayYP5FgLLXWGjUyLFAA=")
        , ("SessionID", bs sessionId)
        , ("User-Agent", "RL Win/170316.47017.154572 gzip")
        ]
      , Client.requestBody = Client.RequestBodyLBS (Aeson.encode [QQ.aesonQQ|
        [
          {
            "Service": "Skills/GetSkillLeaderboardValueForUser",
            "Version": 1,
            "ID": 1,
            "Params": {
              "Playlist": 10,
              "PlayerID": #{playerId}
            }
          },
          {
            "Service": "Skills/GetSkillLeaderboardValueForUser",
            "Version": 1,
            "ID": 2,
            "Params": {
              "Playlist": 11,
              "PlayerID": #{playerId}
            }
          },
          {
            "Service": "Skills/GetSkillLeaderboardValueForUser",
            "Version": 1,
            "ID": 3,
            "Params": {
              "Playlist": 12,
              "PlayerID": #{playerId}
            }
          },
          {
            "Service": "Skills/GetSkillLeaderboardValueForUser",
            "Version": 1,
            "ID": 4,
            "Params": {
              "Playlist": 13,
              "PlayerID": #{playerId}
            }
          }
        ]
      |])
      }
  response <- Client.httpLbs request manager
  let body = Client.responseBody response
  case Aeson.decode body of
    Nothing -> pure []
    Just apiResponse -> pure (toSkills apiResponse)

keepSessionAlive :: Client.Manager -> String -> IO ()
keepSessionAlive manager sessionId = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/Services"
  let request = initialRequest
        { Client.requestHeaders =
          [ ("BuildID", "-1677331153")
          , ("Cache-Control", "no-cache")
          , ("Content-Type", "application/x-www-form-urlencoded")
          , ("Environment", "Prod")
          , ("PsySig", "bLHDwyZg0cj4ekeMDnacS8use3eY49HsJnbIhek3cZg=")
          , ("SessionID", bs sessionId)
          , ("User-Agent", "RL Win/170316.47017.154572 gzip")
          ]
        , Client.requestBody = Client.RequestBodyLBS (Aeson.encode [QQ.aesonQQ|
          [
            {
              "Service": "Population/UpdatePlayerPlaylist",
              "Version": 1,
              "ID": 30,
              "Params": {
                "Playlist": 0,
                "NumLocalPlayers": 1
              }
            }
          ]
        |])
        }
  Monad.forever (do
    Exception.catch
      (do
        response <- Client.httpLbs request manager
        print response)
      (\e -> print (e :: Exception.SomeException))
    Utility.sleep 60)

toSkills :: ApiResponse -> [Skill]
toSkills apiResponse = Maybe.mapMaybe toSkill (apiResponseResponses apiResponse)

toSkill :: ServiceResponse -> Maybe Skill
toSkill serviceResponse = toSkill' (_serviceResponseResult serviceResponse)

toSkill' :: Aeson.Object -> Maybe Skill
toSkill' object = do
  rawPlaylist <- HashMap.lookup "LeaderboardID" object
  rawMmr <- HashMap.lookup "MMR" object
  rawTier <- HashMap.lookup "Value" object

  playlist <- case rawPlaylist of
    Aeson.String "Skill10" -> Just 10
    Aeson.String "Skill11" -> Just 11
    Aeson.String "Skill12" -> Just 12
    Aeson.String "Skill13" -> Just 13
    _ -> Nothing
  mmr <- case rawMmr of
    Aeson.Number number -> rightToMaybe (Scientific.toBoundedRealFloat number)
    _ -> Nothing
  tier <- case rawTier of
    Aeson.Number number -> Scientific.toBoundedInteger number
    _ -> Nothing

  pure Skill
    { skillDivision = 0
    , skillMmr = mmr
    , skillMatchesPlayed = 0
    , skillMu = 0
    , skillPlaylist = playlist
    , skillSigma = 0
    , skillTier = tier
    }

data Skill = Skill
  { skillDivision :: Int
  , skillMmr :: Double
  , skillMatchesPlayed :: Int
  , skillMu :: Double
  , skillPlaylist :: Int
  , skillSigma :: Double
  , skillTier :: Int
  } deriving Show

data ApiResponse = ApiResponse
  { apiResponseResponses :: [ServiceResponse]
  } deriving (Generics.Generic, Show)

instance Aeson.FromJSON ApiResponse where
  parseJSON = genericParseJSON "apiResponse"

data ServiceResponse = ServiceResponse
  { _serviceResponseID :: Int
  , _serviceResponseResult :: Aeson.Object
  } deriving (Generics.Generic, Show)

instance Aeson.FromJSON ServiceResponse where
  parseJSON = genericParseJSON "_serviceResponse"

genericParseJSON
  :: (Aeson.GFromJSON Aeson.Zero (Generics.Rep a), Generics.Generic a)
  => String -> Aeson.Value -> Aeson.Parser a
genericParseJSON prefix =
  let toDrop = length prefix
      options = Casing.aesonDrop toDrop Casing.pascalCase
  in Aeson.genericParseJSON options

t :: String -> Text.Text
t = Text.pack

bs :: String -> ByteString.ByteString
bs = Encoding.encodeUtf8 . t

rightToMaybe :: Either a b -> Maybe b
rightToMaybe e = case e of
  Left _ -> Nothing
  Right r -> Just r
