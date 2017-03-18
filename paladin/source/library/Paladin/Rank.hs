module Paladin.Rank
  ( Skill(..)
  , getPlayerSkills
  , keepSessionAlive
  ) where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Client
import qualified Text.Read as Read

getPlayerSkills :: Client.Manager -> String -> String -> String -> IO [Skill]
getPlayerSkills manager sessionId platform player = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/callproc105/"
  let request = initialRequest
        { Client.requestHeaders = [(ci "SessionID", bs sessionId)]
        , Client.requestBody = Client.RequestBodyBS (bs (concat ["&Proc[]=GetPlayerSkill", platform, "&P0P[]=", player]))
        }
  response <- Client.httpLbs request manager
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
    & pure

keepSessionAlive :: Client.Manager -> String -> IO ()
keepSessionAlive manager sessionId = do
  initialRequest <- Client.parseUrlThrow "POST https://psyonix-rl.appspot.com/Population/UpdatePlayerCurrentGame/"
  let request = initialRequest
        { Client.requestHeaders = [(ci "SessionID", bs sessionId)]
        , Client.requestBody = Client.RequestBodyBS (bs "&PlaylistID=0&NumLocalPlayers=1")
        }
  Monad.forever (do
    Monad.void (Client.httpLbs request manager)
    Concurrent.threadDelay 60000000)

data Skill = Skill
  { skillDivision :: Int
  , skillMmr :: Double
  , skillMatchesPlayed :: Int
  , skillMu :: Double
  , skillPlaylist :: Int
  , skillSigma :: Double
  , skillTier :: Int
  }

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
