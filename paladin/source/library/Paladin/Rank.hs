{-# LANGUAGE QuasiQuotes #-}

module Paladin.Rank
  ( Skill(..)
  , getPlayerSkills
  , keepSessionAlive
  ) where

import Data.Function ((&))

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ as QQ
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Client
import qualified Paladin.Utility as Utility
import qualified Text.Read as Read

getPlayerSkills :: Client.Manager -> String -> String -> String -> IO [Skill]
getPlayerSkills manager sessionId platform player = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/callproc105/"
  let request = initialRequest
        { Client.requestHeaders = [(ci "SessionID", bs sessionId)]
        , Client.requestBody = Client.RequestBodyBS (bs (concat ["&Proc[]=GetPlayerSkill", platform, "&P0P[]=", player]))
        }
  Exception.catch
    (do
      response <- Client.httpLbs request manager
      print response
      response
        & Client.responseBody
        & LazyByteString.toStrict
        & Encoding.decodeUtf8
        & Text.splitOn (t "\r\n")
        & map (Text.splitOn (t "&"))
        & map (map (Text.splitOn (t "=")))
        & map (Maybe.mapMaybe listToTuple)
        & map Map.fromList
        & Maybe.mapMaybe toSkill
        & pure)
    (\e -> do
      print (e :: Exception.SomeException)
      pure [])

keepSessionAlive :: Client.Manager -> String -> IO ()
keepSessionAlive manager sessionId = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/Services"
  let request = initialRequest
        { Client.requestHeaders =
          [ (ci "BuildID", bs "-1677331153")
          , (ci "Cache-Control", bs "no-cache")
          , (ci "Content-Type", bs "application/x-www-form-urlencoded")
          , (ci "Environment", bs "Prod")
          , (ci "PsySig", bs "bLHDwyZg0cj4ekeMDnacS8use3eY49HsJnbIhek3cZg=")
          , (ci "SessionID", bs sessionId)
          , (ci "User-Agent", bs "RL Win/170316.47017.154572 gzip")
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

data Skill = Skill
  { skillDivision :: Int
  , skillMmr :: Double
  , skillMatchesPlayed :: Int
  , skillMu :: Double
  , skillPlaylist :: Int
  , skillSigma :: Double
  , skillTier :: Int
  } deriving Show

toSkill :: Map.Map Text.Text Text.Text -> Maybe Skill
toSkill m = Skill
  <$> lookupAndRead m "Division"
  <*> lookupAndRead m "MMR"
  <*> lookupAndRead m "MatchesPlayed"
  <*> lookupAndRead m "Mu"
  <*> lookupAndRead m "Playlist"
  <*> lookupAndRead m "Sigma"
  <*> lookupAndRead m "Tier"

lookupAndRead :: Read a => Map.Map Text.Text Text.Text -> String -> Maybe a
lookupAndRead m k = do
  v <- Map.lookup (t k) m
  Read.readMaybe (Text.unpack v)

listToTuple :: [a] -> Maybe (a, a)
listToTuple xs = case xs of
  [x, y] -> Just (x, y)
  _ -> Nothing

t :: String -> Text.Text
t = Text.pack

bs :: String -> ByteString.ByteString
bs = Encoding.encodeUtf8 . t

ci :: String -> CaseInsensitive.CI ByteString.ByteString
ci = CaseInsensitive.mk . bs
