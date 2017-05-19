{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.RankWorker where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types
import Paladin.Database
import Paladin.Entity.Platform
import Paladin.Entity.Player
import Paladin.Utility

import qualified Data.Text as Text

type Token = String

updatePlayerSkills :: Connection -> Manager -> Token -> IO ()
updatePlayerSkills connection manager token = do
  forM_ platforms $ \platform -> do
    handle handleException (do
      players <- getPlayers connection platform
      skills <- getSkills manager token platform players
      -- TODO: Insert skills into database.
      print skills)
    sleep 1

platforms :: [PlatformName]
platforms =
  [ PlayStation
  , Steam
  , Xbox
  ]

handleException :: SomeException -> IO ()
handleException exception = do
  putStrLn $ "updatePlayerSkills: " ++ show exception

type PlayerName = Text

getPlayers :: Connection -> PlatformName -> IO [(PlayerId, PlayerName, UTCTime)]
getPlayers connection platform = query
  connection
  [sql|
    select
      players.id,
      max(case
        when platforms.name = 'Xbox' then games_players.name
        else players.remote_id
        end) as name,
      max(player_skills.created_at) as updated_at
    from players
    inner join platforms on platforms.id = players.platform_id
    inner join games_players on games_players.player_id = players.id
    left outer join player_skills on player_skills.player_id = players.id
    where platforms.name = ?
    group by players.id
    order by
      updated_at asc nulls first,
      players.id asc
    limit 100
  |]
  [platform]

getSkills
  :: Manager
  -> Token
  -> PlatformName
  -> [(PlayerId, PlayerName, UTCTime)]
  -> IO [PlayerSkills]
getSkills manager token platform players = do
  initialRequest <- parseUrlThrow $ concat
    [ "https://api.rocketleaguegame.com/api/v1/"
    , platformSlug platform
    , "/playerskills/"
    ]
  let request = initialRequest
        { method = methodPost
        , requestHeaders =
          [ (hAuthorization, encodeUtf8 . Text.pack $ "Token " ++ token)
          ]
        , requestBody = RequestBodyLBS . encode $ playerIds players
        }
  response <- httpLbs request manager
  case eitherDecode $ responseBody response of
    Left message -> fail $ message ++ " " ++ show response
    Right playerSkills -> pure playerSkills

platformSlug :: PlatformName -> String
platformSlug platform = case platform of
  PlayStation -> "playstation"
  Steam -> "steam"
  Xbox -> "xboxone"
  _ -> error $ "unsupported platform: " ++ show platform

playerIds :: [(PlayerId, PlayerName, UTCTime)] -> PlayerIds
playerIds players = PlayerIds $ map (\(_, x, _) -> x) players

newtype PlayerIds = PlayerIds
  { playerIdsPlayerIds :: [Text]
  } deriving (Eq, Generic, Show)

instance ToJSON PlayerIds where
  toJSON = genericToJSON $ jsonOptions "playerIds"

data PlayerSkills = PlayerSkills
  { playerSkillsUserId :: Maybe Integer
  , playerSkillsUserName :: Text
  , playerSkillsPlayerSkills :: [Skill]
  } deriving (Eq, Generic, Show)

instance FromJSON PlayerSkills where
  parseJSON = genericParseJSON $ jsonOptions "playerSkills"

data Skill = Skill
  { skillDivision :: Integer
  , skillMatchesPlayed :: Integer
  , skillPlaylist :: Integer
  , skillSkill :: Integer
  , skillTier :: Integer
  , skillTierMax :: Integer
  } deriving (Eq, Generic, Show)

instance FromJSON Skill where
  parseJSON = genericParseJSON $ jsonOptions "skill"

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix
  }

dropPrefix :: String -> String -> String
dropPrefix prefix string =
  if prefix `isPrefixOf` string
    then drop (length prefix) string
    else error $ unwords [show prefix, "is not a prefix of", show string]
