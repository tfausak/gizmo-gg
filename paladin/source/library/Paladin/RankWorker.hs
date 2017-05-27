{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.RankWorker
  ( updatePlayerSkills
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Database.PostgreSQL.Simple hiding (executeMany, query)
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types
import Paladin.Database
import Paladin.Entity.Platform
import Paladin.Entity.Player
import Paladin.Utility

import qualified Data.Map as Map
import qualified Data.Text as Text

type Token = String

updatePlayerSkills :: Connection -> Manager -> Token -> IO ()
updatePlayerSkills connection manager token = do
  forM_ platforms $ \platform -> do
    handle handleException $ do
      players <- getPlayers connection platform
      let playerIds = getPlayerIds players
      skills <- getSkills manager token platform playerIds
      let playerIdsByName = getPlayerIdsByName players
      insertSkills connection playerIdsByName skills
    sleep 1

insertSkills :: Connection -> Map PlayerName PlayerId -> [PlayerSkills] -> IO ()
insertSkills connection playerIdsByName skills =
  case convertSkillsToRows playerIdsByName skills of
    [] -> pure ()
    rows -> void $ executeMany
      connection
      [sql|
        insert into player_skills (
          player_id,
          playlist_id,
          matches_played,
          division,
          tier,
          mmr,
          mu,
          sigma
        ) values ( ?, ?, ?, ?, ?, ?, ?, ? )
      |]
      rows

convertSkillsToRows :: Map PlayerName PlayerId -> [PlayerSkills] -> [(PlayerId, Integer, Integer, Integer, Integer, Integer, Integer, Integer)]
convertSkillsToRows playerIdsByName skills
  -- Turn the skill values into the format that the database wants.
  = concatMap (\(pid, xs) -> case xs of
    -- If they didn't have any skill values, insert all zeros to avoid fetching
    -- them again next time.
    [] -> [(pid, 1, 0, 0, 0, 0, 0, 0)]
    _ -> flip map xs (\x ->
      ( pid
      , skillPlaylist x
      , skillMatchesPlayed x
      , skillDivision x
      , skillTier x
      , skillSkill x
      , 0
      , 0
      )))
  -- Extract the actual skill values out of their wrapper.
  . map (\(pid, xs) -> (pid, concatMap playerSkillsPlayerSkills xs))
  -- Find all the skills for each player.
  . map (\(name, pid) ->
    (pid, flip filter skills (\skill -> case playerSkillsUserId skill of
      Nothing -> playerSkillsUserName skill == name
      Just x -> Text.pack (show x) == name)))
  -- Start with every player just in case some didn't come back in the API
  -- response.
  $ Map.assocs playerIdsByName

getPlayerIdsByName :: [(PlayerId, PlayerName, Maybe UTCTime)] -> Map PlayerName PlayerId
getPlayerIdsByName = Map.fromList . map (\(v, k, _) -> (k, v))

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

getPlayers :: Connection -> PlatformName -> IO [(PlayerId, PlayerName, Maybe UTCTime)]
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
  -> PlayerIds
  -> IO [PlayerSkills]
getSkills manager token platform playerIds = do
  initialRequest <- parseUrlThrow $ concat
    [ "https://api.rocketleague.com/api/v1/"
    , platformSlug platform
    , "/playerskills/"
    ]
  let request = initialRequest
        { method = methodPost
        , requestHeaders =
          [ (hAuthorization, encodeUtf8 . Text.pack $ "Token " ++ token)
          ]
        , requestBody = RequestBodyLBS $ encode playerIds
        }
  response <- httpLbs request manager
  case eitherDecode $ responseBody response of
    Left message -> fail $ message ++ " " ++ show response
    Right playerSkills -> pure playerSkills

platformSlug :: PlatformName -> String
platformSlug platform = case platform of
  PlayStation -> "ps4"
  Steam -> "steam"
  Xbox -> "xboxone"
  _ -> error $ "unsupported platform: " ++ show platform

getPlayerIds :: [(PlayerId, PlayerName, Maybe UTCTime)] -> PlayerIds
getPlayerIds players = PlayerIds $ map (\(_, x, _) -> x) players

newtype PlayerIds = PlayerIds
  { _playerIdsPlayerIds :: [Text]
  } deriving (Eq, Generic, Show)

instance ToJSON PlayerIds where
  toJSON = genericToJSON $ jsonOptions "_playerIds"

data PlayerSkills = PlayerSkills
  { playerSkillsUserId :: Maybe Integer
  , playerSkillsUserName :: PlayerName
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
  -- , skillTierMax :: Integer
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
