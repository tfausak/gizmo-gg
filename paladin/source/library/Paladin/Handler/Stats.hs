{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Paladin.Handler.Stats
  ( getStatsArenasHandler
  , getStatsBodiesHandler
  , getStatsPlayersArenasHandler
  , getStatsPlayersBodiesHandler
  , getStatsPlayersHandler
  , getStatsPlayersPollHandler
  , getStatsSummaryHandler
  , PlayerGameRow(..)
  , getGamesPlayers
  , makePlayerOutputWithSkill
  , getStatsPlayersHistoryHandler
  , getStatsPlayersRankHandler
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Text.Read as Read

getStatsPlayersRankHandler :: Common.Text -> Common.Handler
getStatsPlayersRankHandler rawPlayerId _config connection request = do
  maybePlayerId <- getPlayerId connection rawPlayerId
  case maybePlayerId of
    Nothing -> pure notFound
    Just playerId -> do
      day <- getDay (Wai.queryString request)
      rows <- Database.query connection [Common.sql|
        select created_at, playlist_id, mmr, tier, division
        from player_skills
        where player_id = ?
        and playlist_id in ?
        and created_at >= ?
        order by created_at desc
      |] (playerId, Common.In competitivePlaylists, day)
      pure (Common.jsonResponse Http.status200 [] (Aeson.toJSON (toRankOutputs rows)))

toRankOutputs :: [RankRow] -> Map.Map Common.Text [RankOutput]
toRankOutputs rows =
  let
    toElement x = (toPlaylistKey (rankOutputPlaylist x), [x])
    removeDuplicates xs = case xs of
      x : y : zs -> if rankOutputMmr x == rankOutputMmr y
        then removeDuplicates (x : zs)
        else x : removeDuplicates (y : zs)
      _ -> xs
  in
    Map.map removeDuplicates
      (Map.fromListWith (flip (++))
        (map (toElement . toRankOutput) rows))

toRankOutput :: RankRow -> RankOutput
toRankOutput (at, playlist, mmr, tier, division) = RankOutput
  { rankOutputAt = at
  , rankOutputPlaylist = playlist
  , rankOutputMmr = mmr
  , rankOutputTier = tier
  , rankOutputDivision = division
  }

toPlaylistKey :: Common.PlaylistId -> Common.Text
toPlaylistKey (Common.Tagged playlistId) =
  Text.pack (
    if playlistId == competitiveSoloDuel then "ranked1v1" else
    if playlistId == competitiveDoubles then "ranked2v2" else
    if playlistId == competitiveSoloStandard then "ranked3v3solo" else
    if playlistId == competitiveStandard then "ranked3v3" else
    "unknown"
  )

type RankRow = (Common.UTCTime, Common.PlaylistId, Double, Int, Int)

data RankOutput = RankOutput
  { rankOutputAt :: Common.UTCTime
  , rankOutputPlaylist :: Common.PlaylistId
  , rankOutputMmr :: Double
  , rankOutputTier :: Int
  , rankOutputDivision :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON RankOutput where
  toJSON = Common.genericToJSON "rankOutput"

getStatsPlayersHistoryHandler :: Common.Text -> Common.Handler
getStatsPlayersHistoryHandler rawPlayerId _config connection request = do
  maybePlayerId <- getPlayerId connection rawPlayerId
  case maybePlayerId of
    Nothing -> pure notFound
    Just playerId -> do
      (day, playlists, templates) <- getFilters request
      results <- Database.query connection
        [Common.sql|
          select
            games.played_at,
            case when games_players.is_blue
              then games.blue_goals else games.orange_goals end,
            case when games_players.is_blue
              then games.orange_goals else games.blue_goals end
          from games_players
          inner join games on games.id = games_players.game_id
          inner join arenas on arenas.id = games.arena_id
          inner join arena_templates on arena_templates.id = arenas.template_id
          where
            games_players.player_id = ? and
            games_players.is_present_at_end = true and
            games.played_at >= ? and
            games.playlist_id in ? and
            arena_templates.name in ?
          order by games.played_at desc
        |]
        (playerId, day, Common.In playlists, Common.In templates)
      let output = flip map results (\(playedAt, myGoals, theirGoals) ->
            HistoryOutput
              { historyOutputAt = playedAt
              , historyOutputMyGoals = myGoals
              , historyOutputTheirGoals = theirGoals
              })
      let body = Aeson.toJSON output
      pure (Common.jsonResponse Http.status200 [] body)

data HistoryOutput = HistoryOutput
  { historyOutputAt :: Common.LocalTime
  , historyOutputMyGoals :: Int
  , historyOutputTheirGoals :: Int
  } deriving (Common.Generic)

instance Common.ToJSON HistoryOutput where
  toJSON = Common.genericToJSON "historyOutput"

notFound :: Wai.Response
notFound = Common.jsonResponse Http.status404 [] Aeson.Null

getStatsPlayersPollHandler :: Common.Text -> Common.Handler
getStatsPlayersPollHandler rawPlayerId _config connection _request = do
  let playerId = rawPlayerId & Text.unpack & Read.readMaybe & Maybe.fromMaybe 0
  maybeLastPlayedAt <- Database.query connection
    [Common.sql|
      select max(games.played_at)
      from games
      inner join games_players
      on games_players.game_id = games.id
      where games_players.player_id = ?
    |]
    [playerId :: Int]
  case maybeLastPlayedAt of
    [[Just lastPlayedAt]] -> do
      let json = Aeson.toJSON (lastPlayedAt :: Time.LocalTime)
      pure (Common.jsonResponse Http.status200 [] json)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

getStatsPlayersBodiesHandler :: Common.Text -> Common.Handler
getStatsPlayersBodiesHandler rawPlayerId _config connection request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
      (day, playlists, templates) <- getFilters request
      bodies <-
        Database.query
          connection
          [Common.sql|
            SELECT id, name
            FROM bodies
            WHERE name NOT IN ?
            ORDER BY name ASC
          |]
          [Common.In bodyNamesToIgnore]
      stats <-
        Database.query
          connection
          [Common.sql|
            SELECT
              bodies.id,
              bodies.name,
              sum(games_players.score),
              sum(games_players.goals),
              sum(games_players.assists),
              sum(games_players.saves),
              sum(games_players.shots),
              sum(games.duration),
              count(games.id),
              count(case when games_players.did_win then 1 end),
              count(case when not games_players.did_win then 1 end)
            FROM bodies
            INNER JOIN games_players ON games_players.body_id = bodies.id
            INNER JOIN games ON games.id = games_players.game_id
            INNER JOIN arenas ON arenas.id = games.arena_id
            INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ? AND
              arena_templates.name IN ?
            GROUP BY bodies.id
            ORDER BY bodies.id ASC
          |]
          (playerId, day, Common.In playlists, Common.In templates)
      let statsById = toMapBy _playerBodyStatsBodyId stats
      let get x =
            Map.findWithDefault
              (defaultPlayerBodyStats x)
              (Common.bodyId x)
              statsById
      let output = map get bodies
      let json = Aeson.toJSON output
      pure (Common.jsonResponse Http.status200 [] json)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

toMapBy
  :: Ord k
  => (v -> k) -> [v] -> Map.Map k v
toMapBy f xs = Map.fromList (map (\x -> (f x, x)) xs)

data PlayerBodyStats = PlayerBodyStats
  { _playerBodyStatsBodyId :: Common.Tagged Common.Body Int
  , _playerBodyStatsBodyName :: Maybe Common.Text
  , _playerBodyStatsTotalScore :: Int
  , _playerBodyStatsTotalGoals :: Int
  , _playerBodyStatsTotalAssists :: Int
  , _playerBodyStatsTotalSaves :: Int
  , _playerBodyStatsTotalShots :: Int
  , _playerBodyStatsTotalDuration :: Int
  , _playerBodyStatsNumGames :: Int
  , _playerBodyStatsNumWins :: Int
  , _playerBodyStatsNumLosses :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerBodyStats

instance Common.ToJSON PlayerBodyStats where
  toJSON = Common.genericToJSON "_PlayerBodyStats"

defaultPlayerBodyStats :: Common.Body -> PlayerBodyStats
defaultPlayerBodyStats body =
  PlayerBodyStats
  { _playerBodyStatsBodyId = Common.bodyId body
  , _playerBodyStatsBodyName = Common.bodyName body
  , _playerBodyStatsTotalScore = 0
  , _playerBodyStatsTotalGoals = 0
  , _playerBodyStatsTotalAssists = 0
  , _playerBodyStatsTotalSaves = 0
  , _playerBodyStatsTotalShots = 0
  , _playerBodyStatsTotalDuration = 0
  , _playerBodyStatsNumGames = 0
  , _playerBodyStatsNumWins = 0
  , _playerBodyStatsNumLosses = 0
  }

getStatsPlayersArenasHandler :: Common.Text -> Common.Handler
getStatsPlayersArenasHandler rawPlayerId _config connection request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
      (day, playlists, templates) <- getFilters request
      arenas <-
        Database.query
          connection
          [Common.sql|
            SELECT
              arenas.id,
              arenas.name,
              arena_templates.id,
              arena_templates.name,
              null, -- arena_models.id
              null, -- arena_models.name
              null, -- arena_skins.id
              null -- arena_skins.name
            FROM arenas
            INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
            WHERE
              arena_templates.name IN ? AND
              arenas.name NOT IN ?
            ORDER BY arenas.name ASC
          |]
          (Common.In competitiveTemplates, Common.In arenaNamesToIgnore)
      stats <-
        Database.query
          connection
          [Common.sql|
            SELECT
              arenas.id,
              arenas.name,
              sum(games_players.score),
              sum(games_players.goals),
              sum(games_players.assists),
              sum(games_players.saves),
              sum(games_players.shots),
              sum(games.duration),
              count(games.id),
              count(case when games_players.did_win then 1 end),
              count(case when not games_players.did_win then 1 end)
            FROM games_players
            INNER JOIN games ON games.id = games_players.game_id
            INNER JOIN arenas ON arenas.id = games.arena_id
            INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true AND
              games.played_at >= ? AND
              games.playlist_id IN ? AND
              arena_templates.name IN ?
            GROUP BY arenas.id
            ORDER BY arenas.id ASC
          |]
          (playerId, day, Common.In playlists, Common.In templates)
      let statsById = toMapBy _playerArenaStatsArenaId stats
      let get x =
            Map.findWithDefault
              (defaultPlayerArenaStats x)
              (Common.arenaId x)
              statsById
      let output = map get arenas
      let json = Aeson.toJSON output
      pure (Common.jsonResponse Http.status200 [] json)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

data PlayerArenaStats = PlayerArenaStats
  { _playerArenaStatsArenaId :: Common.Tagged Common.Arena Int
  , _playerArenaStatsArenaName :: Common.Text
  , _playerArenaStatsTotalScore :: Int
  , _playerArenaStatsTotalGoals :: Int
  , _playerArenaStatsTotalAssists :: Int
  , _playerArenaStatsTotalSaves :: Int
  , _playerArenaStatsTotalShots :: Int
  , _playerArenaStatsTotalDuration :: Int
  , _playerArenaStatsNumGames :: Int
  , _playerArenaStatsNumWins :: Int
  , _playerArenaStatsNumLosses :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerArenaStats

instance Common.ToJSON PlayerArenaStats where
  toJSON = Common.genericToJSON "_PlayerArenaStats"

defaultPlayerArenaStats :: Common.Arena -> PlayerArenaStats
defaultPlayerArenaStats arena =
  PlayerArenaStats
  { _playerArenaStatsArenaId = Common.arenaId arena
  , _playerArenaStatsArenaName = Common.arenaName arena
  , _playerArenaStatsTotalScore = 0
  , _playerArenaStatsTotalGoals = 0
  , _playerArenaStatsTotalAssists = 0
  , _playerArenaStatsTotalSaves = 0
  , _playerArenaStatsTotalShots = 0
  , _playerArenaStatsTotalDuration = 0
  , _playerArenaStatsNumGames = 0
  , _playerArenaStatsNumWins = 0
  , _playerArenaStatsNumLosses = 0
  }

getStatsArenasHandler :: Common.Handler
getStatsArenasHandler _config connection request = do
  (day, playlists, templates) <- getFilters request
  arenas <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.id,
          arenas.name,
          arena_templates.id,
          arena_templates.name,
          null, -- arena_models.id
          null, -- arena_models.name
          null, -- arena_skins.id
          null -- arena_skins.name
        FROM arenas
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          arena_templates.name IN ? AND
          arenas.name NOT IN ?
        ORDER BY arenas.name ASC
      |]
      (Common.In templates, Common.In arenaNamesToIgnore)
  stats <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.id,
          arenas.name,
          sum(games_players.score),
          sum(games_players.goals),
          sum(games_players.assists),
          sum(games_players.saves),
          sum(games_players.shots),
          count(distinct games.id)
        FROM arenas
        INNER JOIN games ON games.arena_id = arenas.id
        INNER JOIN games_players ON games_players.game_id = games.id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ?
        GROUP BY arenas.id
      |]
      (day, Common.In playlists, Common.In templates)
  let statsById = toMapBy _arenaStatsArenaId stats
  let get x =
        Map.findWithDefault (defaultArenaStats x) (Common.arenaId x) statsById
  let output = map get arenas
  let json = Aeson.toJSON output
  pure (Common.jsonResponse Http.status200 [] json)

data ArenaStats = ArenaStats
  { _arenaStatsArenaId :: Common.Tagged Common.Arena Int
  , _arenaStatsArenaName :: Common.Text
  , _arenaStatsTotalScore :: Int
  , _arenaStatsTotalGoals :: Int
  , _arenaStatsTotalAssists :: Int
  , _arenaStatsTotalSaves :: Int
  , _arenaStatsTotalShots :: Int
  , _arenaStatsNumGames :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow ArenaStats

instance Common.ToJSON ArenaStats where
  toJSON = Common.genericToJSON "_ArenaStats"

defaultArenaStats :: Common.Arena -> ArenaStats
defaultArenaStats arena =
  ArenaStats
  { _arenaStatsArenaId = Common.arenaId arena
  , _arenaStatsArenaName = Common.arenaName arena
  , _arenaStatsTotalScore = 0
  , _arenaStatsTotalGoals = 0
  , _arenaStatsTotalAssists = 0
  , _arenaStatsTotalSaves = 0
  , _arenaStatsTotalShots = 0
  , _arenaStatsNumGames = 0
  }

getStatsBodiesHandler :: Common.Handler
getStatsBodiesHandler _config connection request = do
  (day, playlists, templates) <- getFilters request
  bodies <-
    Database.query
      connection
      [Common.sql|
        SELECT id, name
        FROM bodies
        WHERE name NOT IN ?
        ORDER BY name ASC
      |]
      [Common.In bodyNamesToIgnore]
  stats <-
    Database.query
      connection
      [Common.sql|
        SELECT
          bodies.id,
          bodies.name,
          sum(games_players.score),
          sum(games_players.goals),
          sum(games_players.assists),
          sum(games_players.saves),
          sum(games_players.shots),
          count(games.id),
          count(case when games_players.did_win then 1 end),
          count(case when not games_players.did_win then 1 end)
        FROM bodies
        INNER JOIN games_players ON games_players.body_id = bodies.id
        INNER JOIN games ON games.id = games_players.game_id
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games_players.is_present_at_end = true AND
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ?
        GROUP BY bodies.id
        ORDER BY bodies.id ASC
      |]
      (day, Common.In playlists, Common.In templates)
  let statsById = toMapBy _bodyStatsBodyId stats
  let get x =
        Map.findWithDefault (defaultBodyStats x) (Common.bodyId x) statsById
  let output = map get bodies
  let json = Aeson.toJSON output
  pure (Common.jsonResponse Http.status200 [] json)

data BodyStats = BodyStats
  { _bodyStatsBodyId :: Common.Tagged Common.Body Int
  , _bodyStatsBodyName :: Maybe Common.Text
  , _bodyStatsTotalScore :: Int
  , _bodyStatsTotalGoals :: Int
  , _bodyStatsTotalAssists :: Int
  , _bodyStatsTotalSaves :: Int
  , _bodyStatsTotalShots :: Int
  , _bodyStatsNumGames :: Int
  , _bodyStatsNumWins :: Int
  , _bodyStatsNumLosses :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow BodyStats

instance Common.ToJSON BodyStats where
  toJSON = Common.genericToJSON "_BodyStats"

defaultBodyStats :: Common.Body -> BodyStats
defaultBodyStats body =
  BodyStats
  { _bodyStatsBodyId = Common.bodyId body
  , _bodyStatsBodyName = Common.bodyName body
  , _bodyStatsTotalScore = 0
  , _bodyStatsTotalGoals = 0
  , _bodyStatsTotalAssists = 0
  , _bodyStatsTotalSaves = 0
  , _bodyStatsTotalShots = 0
  , _bodyStatsNumGames = 0
  , _bodyStatsNumWins = 0
  , _bodyStatsNumLosses = 0
  }

getStatsPlayersHandler :: Common.Text -> Common.Handler
getStatsPlayersHandler rawPlayerId _config connection request = do
  maybePlayerId <- getPlayerId connection rawPlayerId
  case maybePlayerId of
    Nothing -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)
    Just playerId -> do
      platform <- getPlatform connection playerId
      namesAndTimes <- getNamesAndTimes connection playerId
      (day, playlists, templates) <- getFilters request
      let after = getAfter request
      games <- getGames connection day playlists templates after playerId
      let gameIds = map playerGameRowGameId games
      gamesPlayers <- getGamesPlayers connection gameIds
      maybePlayerOutputWithSkill <- makePlayerOutputWithSkill connection playerId platform namesAndTimes games gamesPlayers
      let body = Aeson.toJSON maybePlayerOutputWithSkill
      pure (Common.jsonResponse Http.status200 [] body)

makePlayerOutputWithSkill
  :: Sql.Connection
  -> Common.PlayerId
  -> Maybe Common.Platform
  -> [(Common.Text, Common.LocalTime)]
  -> [PlayerGameRow]
  -> [GamePlayerRow]
  -> IO (Maybe PlayerOutput)
makePlayerOutputWithSkill connection playerId platform namesAndTimes games gamesPlayers =
  case makePlayerOutput platform namesAndTimes games gamesPlayers of
    Nothing -> pure Nothing
    Just playerOutput -> do
      playerOutputWithSkill <- addSkillToPlayerOutput connection playerId playerOutput
      pure (Just playerOutputWithSkill)

addSkillToPlayerOutput :: Sql.Connection -> Common.PlayerId -> PlayerOutput -> IO PlayerOutput
addSkillToPlayerOutput connection playerId output = do
  results <- Database.query connection
    [Common.sql|
      select distinct on (playlists.id)
        playlists.id,
        playlists.name,
        player_skills.matches_played,
        player_skills.division,
        player_skills.tier,
        player_skills.mmr
      from player_skills
      inner join playlists on playlists.id = player_skills.playlist_id
      where
        player_skills.player_id = ? and
        playlists.id in ?
      order by playlists.id asc, player_skills.created_at desc
    |] (playerId, Common.In competitivePlaylists)
  let skills = foldr
        (\(_ :: Common.PlaylistId, playlistName, matchesPlayed, division, tier, mmr) ->
          Map.insert playlistName SkillOutput
            { skillOutputMatchesPlayed = matchesPlayed
            , skillOutputDivision = division
            , skillOutputTier = tier
            , skillOutputMmr = mmr
            })
          Map.empty
        results
  games <- addSkillToGames connection (playerOutputGames output)
  pure output
    { playerOutputGames = games
    , playerOutputSkills = skills
    }

addSkillToGames :: Sql.Connection -> [GameOutput] -> IO [GameOutput]
addSkillToGames connection games =  mapM (addSkillToGame connection) games

addSkillToGame :: Sql.Connection -> GameOutput -> IO GameOutput
addSkillToGame connection game = do
  let playlistId = gameOutputPlaylistId game
  let playedAt = gameOutputPlayedAt game
  players <- addSkillToPlayers connection playlistId playedAt (gameOutputPlayers game)
  pure game { gameOutputPlayers = players }

addSkillToPlayers :: Sql.Connection -> Common.PlaylistId -> Common.LocalTime -> [GamePlayerOutput] -> IO [GamePlayerOutput]
addSkillToPlayers connection playlistId playedAt players = mapM (addSkillToPlayer connection playlistId playedAt) players

addSkillToPlayer :: Sql.Connection -> Common.PlaylistId -> Common.LocalTime -> GamePlayerOutput -> IO GamePlayerOutput
addSkillToPlayer connection playlistId playedAt player = do
  let playerId = gamePlayerOutputPlayerId player
  skills <- Database.query connection [Common.sql|
    select matches_played, division, tier, mmr
    from player_skills
    where
      playlist_id = ? and
      player_id = ? and
      created_at > timestamp ? - interval '1 week' and
      created_at < timestamp ? + interval '1 week'
    order by abs(extract(epoch from created_at - ?)) asc
    limit 1
  |] (playlistId, playerId, playedAt, playedAt, playedAt)
  let skill = case skills of
        (matchesPlayed, division, tier, mmr) : _ -> Just SkillOutput
          { skillOutputMatchesPlayed = matchesPlayed
          , skillOutputDivision = division
          , skillOutputTier = tier
          , skillOutputMmr = mmr
          }
        _ -> Nothing
  pure player { gamePlayerOutputSkill = skill }

getGames :: Sql.Connection
         -> Time.Day
         -> [Int]
         -> [String]
         -> Time.LocalTime
         -> Common.PlayerId
         -> IO [PlayerGameRow]
getGames connection day playlists templates after player =
  Database.query
    connection
    [Common.sql|
      SELECT
        games.id,
        playlists.id,
        playlists.name,
        arenas.id,
        arenas.name,
        arena_skins.id,
        arena_skins.name,
        arena_models.id,
        arena_models.name,
        arena_templates.id,
        arena_templates.name,
        games.played_at,
        games.duration,
        games.blue_goals,
        games.orange_goals
      FROM games
      INNER JOIN arenas ON arenas.id = games.arena_id
      LEFT OUTER JOIN arena_skins ON arena_skins.id = arenas.skin_id
      INNER JOIN arena_models ON arena_models.id = arenas.model_id
      INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
      INNER JOIN games_players ON games_players.game_id = games.id
      INNER JOIN playlists ON playlists.id = games.playlist_id
      WHERE
        games.played_at >= ? AND
        games.played_at < ? AND
        playlists.id IN ? AND
        arena_templates.name IN ? AND
        games_players.player_id = ?
      ORDER BY games.played_at DESC
      LIMIT 10
    |]
    (day, after, Common.In playlists, Common.In templates, player)

data PlayerGameRow = PlayerGameRow
  { playerGameRowGameId :: Int
  , playerGameRowPlaylistId :: Common.PlaylistId
  , playerGameRowPlaylistName :: Maybe Common.Text
  , playerGameRowArenaId :: Int
  , playerGameRowArenaName :: Maybe Common.Text
  , playerGameRowArenaSkinId :: Maybe Int
  , playerGameRowArenaSkinName :: Maybe Common.Text
  , playerGameRowArenaModelId :: Int
  , playerGameRowArenaModelName :: Maybe Common.Text
  , playerGameRowArenaTemplateId :: Int
  , playerGameRowArenaTemplateName :: Maybe Common.Text
  , playerGameRowPlayedAt :: Time.LocalTime
  , playerGameRowDuration :: Int
  , playerGameRowBlueGoals :: Int
  , playerGameRowOrangeGoals :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow PlayerGameRow

getGamesPlayers :: Sql.Connection -> [Int] -> IO [GamePlayerRow]
getGamesPlayers connection gameIds =
  Database.query
    connection
    [Common.sql|
      SELECT
        games_players.id,
        games_players.game_id,
        games_players.player_id,
        players.platform_id,
        platforms.name,
        players.remote_id,
        players.local_id,
        games_players.name,
        games_players.xp,
        games_players.is_blue,
        games_players.is_present_at_end,
        games_players.score,
        games_players.goals,
        games_players.assists,
        games_players.saves,
        games_players.shots,
        games_players.body_id,
        bodies.name,
        games_players.decal_id,
        decals.name,
        games_players.wheel_id,
        wheels.name,
        games_players.rocket_trail_id,
        rocket_trails.name,
        games_players.antenna_id,
        antennas.name,
        games_players.topper_id,
        toppers.name,
        games_players.wheel_paint_id,
        games_players.topper_paint_id,
        games_players.primary_color_id,
        games_players.accent_color_id,
        games_players.primary_finish_id,
        games_players.accent_finish_id,
        games_players.fov,
        games_players.height,
        games_players.angle,
        games_players.distance,
        games_players.stiffness,
        games_players.swivel_speed
      FROM games_players
      INNER JOIN players ON players.id = games_players.player_id
      INNER JOIN platforms ON platforms.id = players.platform_id
      LEFT OUTER JOIN bodies ON bodies.id = games_players.body_id
      LEFT OUTER JOIN decals ON decals.id = games_players.decal_id
      LEFT OUTER JOIN wheels ON wheels.id = games_players.wheel_id
      LEFT OUTER JOIN rocket_trails ON rocket_trails.id = games_players.rocket_trail_id
      LEFT OUTER JOIN antennas ON antennas.id = games_players.antenna_id
      LEFT OUTER JOIN toppers ON toppers.id = games_players.topper_id
      WHERE game_id IN ?
      ORDER BY game_id ASC, player_id ASC
    |]
    [Common.In gameIds]

data GamePlayerRow = GamePlayerRow
  { _gamePlayerRowId :: Int
  , gamePlayerRowGameId :: Int
  , gamePlayerRowPlayerId :: Common.PlayerId
  , gamePlayerRowPlatformId :: Int
  , gamePlayerRowPlatformName :: Common.Text
  , gamePlayerRowRemoteId :: Common.Text
  , gamePlayerRowLocalId :: Int
  , gamePlayerRowName :: Common.Text
  , gamePlayerRowXp :: Int
  , gamePlayerRowIsBlue :: Bool
  , gamePlayerRowIsPresentAtEnd :: Bool
  , gamePlayerRowScore :: Int
  , gamePlayerRowGoals :: Int
  , gamePlayerRowAssists :: Int
  , gamePlayerRowSaves :: Int
  , gamePlayerRowShots :: Int
  , gamePlayerRowBodyId :: Int
  , gamePlayerRowBodyName :: Maybe Common.Text
  , gamePlayerRowDecalId :: Int
  , gamePlayerRowDecalName :: Maybe Common.Text
  , gamePlayerRowWheelId :: Int
  , gamePlayerRowWheelName :: Maybe Common.Text
  , gamePlayerRowRocketTrailId :: Int
  , gamePlayerRowRocketTrailName :: Maybe Common.Text
  , gamePlayerRowAntennaId :: Int
  , gamePlayerRowAntennaName :: Maybe Common.Text
  , gamePlayerRowTopperId :: Int
  , gamePlayerRowTopperName :: Maybe Common.Text
  , gamePlayerRowWheelPaintId :: Maybe Int
  , gamePlayerRowTopperPaintId :: Maybe Int
  , gamePlayerRowPrimaryColorId :: Int
  , gamePlayerRowAccentColorId :: Int
  , gamePlayerRowPrimaryFinishId :: Int
  , gamePlayerRowAccentFinishId :: Int
  , gamePlayerRowFov :: Float
  , gamePlayerRowHeight :: Float
  , gamePlayerRowAngle :: Float
  , gamePlayerRowDistance :: Float
  , gamePlayerRowStiffness :: Float
  , gamePlayerRowSwivelSpeed :: Float
  } deriving (Eq, Common.Generic, Show)

instance Common.FromRow GamePlayerRow

data PlayerOutput = PlayerOutput
  { playerOutputName :: Common.Text
  , playerOutputAliases :: [Common.Text]
  , playerOutputLastPlayedAt :: Common.LocalTime
  , playerOutputPlatform :: Common.Platform
  , playerOutputGames :: [GameOutput]
  , playerOutputSkills :: Map.Map Text.Text SkillOutput
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON PlayerOutput where
  toJSON = Common.genericToJSON "PlayerOutput"

makePlayerOutput
  :: Maybe Common.Platform
  -> [(Common.Text, Common.LocalTime)]
  -> [PlayerGameRow]
  -> [GamePlayerRow]
  -> Maybe PlayerOutput
makePlayerOutput maybePlatform namesAndTimes games players = do
  platform <- maybePlatform
  (name, aliases, lastPlayedAt) <-
    case namesAndTimes of
      (n, t):nts -> pure (n, map fst nts, t)
      _ -> Nothing
  let playersByGame =
        foldr
          (\player -> Map.insertWith (++) (gamePlayerRowGameId player) [player])
          Map.empty
          players
  let gamesWithPlayers =
        map
          (\game ->
             ( game
             , Map.findWithDefault [] (playerGameRowGameId game) playersByGame))
          games
  pure
    PlayerOutput
    { playerOutputName = name
    , playerOutputAliases = aliases
    , playerOutputLastPlayedAt = lastPlayedAt
    , playerOutputPlatform = platform
    , playerOutputGames = map makeGameOutput gamesWithPlayers
    , playerOutputSkills = Map.empty
    }

getPlatform :: Sql.Connection -> Common.PlayerId -> IO (Maybe Common.Platform)
getPlatform connection playerId = do
  result <-
    Database.query
      connection
      [Common.sql|
        SELECT platforms.id, platforms.name
        FROM platforms
        INNER JOIN players ON players.platform_id = platforms.id
        WHERE players.id = ?
      |]
      [playerId]
  case result of
    [platform] -> pure (Just platform)
    _ -> pure Nothing

data GameOutput = GameOutput
  { gameOutputId :: Int
  , gameOutputPlaylistId :: Common.PlaylistId
  , gameOutputPlaylistName :: Maybe Common.Text
  , gameOutputArena :: ArenaOutput
  , gameOutputPlayedAt :: Time.LocalTime
  , gameOutputDuration :: Int
  , gameOutputBlueGoals :: Int
  , gameOutputOrangeGoals :: Int
  , gameOutputPlayers :: [GamePlayerOutput]
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON GameOutput where
  toJSON = Common.genericToJSON "GameOutput"

makeGameOutput :: (PlayerGameRow, [GamePlayerRow]) -> GameOutput
makeGameOutput (game, players) =
  GameOutput
  { gameOutputId = playerGameRowGameId game
  , gameOutputPlaylistId = playerGameRowPlaylistId game
  , gameOutputPlaylistName = playerGameRowPlaylistName game
  , gameOutputArena = makeArenaOutput game
  , gameOutputPlayedAt = playerGameRowPlayedAt game
  , gameOutputDuration = playerGameRowDuration game
  , gameOutputBlueGoals = playerGameRowBlueGoals game
  , gameOutputOrangeGoals = playerGameRowOrangeGoals game
  , gameOutputPlayers = map makeGamePlayerOutput players
  }

data ArenaOutput = ArenaOutput
  { arenaOutputId :: Int
  , arenaOutputName :: Maybe Common.Text
  , arenaOutputSkinId :: Maybe Int
  , arenaOutputSkinName :: Maybe Common.Text
  , arenaOutputModelId :: Int
  , arenaOutputModelName :: Maybe Common.Text
  , arenaOutputTemplateId :: Int
  , arenaOutputTemplateName :: Maybe Common.Text
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON ArenaOutput where
  toJSON = Common.genericToJSON "ArenaOutput"

makeArenaOutput :: PlayerGameRow -> ArenaOutput
makeArenaOutput game =
  ArenaOutput
  { arenaOutputId = playerGameRowArenaId game
  , arenaOutputName = playerGameRowArenaName game
  , arenaOutputSkinId = playerGameRowArenaSkinId game
  , arenaOutputSkinName = playerGameRowArenaSkinName game
  , arenaOutputModelId = playerGameRowArenaModelId game
  , arenaOutputModelName = playerGameRowArenaModelName game
  , arenaOutputTemplateId = playerGameRowArenaTemplateId game
  , arenaOutputTemplateName = playerGameRowArenaTemplateName game
  }

data GamePlayerOutput = GamePlayerOutput
  { gamePlayerOutputPlayerId :: Common.PlayerId
  , gamePlayerOutputPlatformId :: Int
  , gamePlayerOutputPlatformName :: Common.Text
  , gamePlayerOutputRemoteId :: Common.Text
  , gamePlayerOutputLocalId :: Int
  , gamePlayerOutputName :: Common.Text
  , gamePlayerOutputXp :: Int
  , gamePlayerOutputIsOnBlueTeam :: Bool
  , gamePlayerOutputIsPresentAtEnd :: Bool
  , gamePlayerOutputScore :: Int
  , gamePlayerOutputGoals :: Int
  , gamePlayerOutputAssists :: Int
  , gamePlayerOutputSaves :: Int
  , gamePlayerOutputShots :: Int
  , gamePlayerOutputLoadout :: LoadoutOutput
  , gamePlayerOutputCamera :: CameraOutput
  , gamePlayerOutputSkill :: Maybe SkillOutput
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON GamePlayerOutput where
  toJSON = Common.genericToJSON "GamePlayerOutput"

makeGamePlayerOutput :: GamePlayerRow -> GamePlayerOutput
makeGamePlayerOutput player =
  GamePlayerOutput
  { gamePlayerOutputPlayerId = gamePlayerRowPlayerId player
  , gamePlayerOutputPlatformId = gamePlayerRowPlatformId player
  , gamePlayerOutputPlatformName = gamePlayerRowPlatformName player
  , gamePlayerOutputRemoteId = gamePlayerRowRemoteId player
  , gamePlayerOutputLocalId = gamePlayerRowLocalId player
  , gamePlayerOutputName = gamePlayerRowName player
  , gamePlayerOutputXp = gamePlayerRowXp player
  , gamePlayerOutputIsOnBlueTeam = gamePlayerRowIsBlue player
  , gamePlayerOutputIsPresentAtEnd = gamePlayerRowIsPresentAtEnd player
  , gamePlayerOutputScore = gamePlayerRowScore player
  , gamePlayerOutputGoals = gamePlayerRowGoals player
  , gamePlayerOutputAssists = gamePlayerRowAssists player
  , gamePlayerOutputSaves = gamePlayerRowSaves player
  , gamePlayerOutputShots = gamePlayerRowShots player
  , gamePlayerOutputLoadout = makeLoadoutOutput player
  , gamePlayerOutputCamera = makeCameraOutput player
  , gamePlayerOutputSkill = Nothing
  }

data SkillOutput = SkillOutput
  { skillOutputMatchesPlayed :: Int
  , skillOutputDivision :: Int
  , skillOutputTier :: Int
  , skillOutputMmr :: Double
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON SkillOutput where
  toJSON = Common.genericToJSON "skillOutput"

data LoadoutOutput = LoadoutOutput
  { loadoutOutputBodyId :: Int
  , loadoutOutputBodyName :: Maybe Common.Text
  , loadoutOutputDecalId :: Int
  , loadoutOutputDecalName :: Maybe Common.Text
  , loadoutOutputWheelId :: Int
  , loadoutOutputWheelName :: Maybe Common.Text
  , loadoutOutputRocketTrailId :: Int
  , loadoutOutputRocketTrailName :: Maybe Common.Text
  , loadoutOutputAntennaId :: Int
  , loadoutOutputAntennaName :: Maybe Common.Text
  , loadoutOutputTopperId :: Int
  , loadoutOutputTopperName :: Maybe Common.Text
  , loadoutOutputWheelPaintId :: Maybe Int
  , loadoutOutputTopperPaintId :: Maybe Int
  , loadoutOutputPrimaryColorId :: Int
  , loadoutOutputAccentColorId :: Int
  , loadoutOutputPrimaryFinishId :: Int
  , loadoutOutputAccentFinishId :: Int
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON LoadoutOutput where
  toJSON = Common.genericToJSON "LoadoutOutput"

makeLoadoutOutput :: GamePlayerRow -> LoadoutOutput
makeLoadoutOutput player =
  LoadoutOutput
  { loadoutOutputBodyId = gamePlayerRowBodyId player
  , loadoutOutputBodyName = gamePlayerRowBodyName player
  , loadoutOutputDecalId = gamePlayerRowDecalId player
  , loadoutOutputDecalName = gamePlayerRowDecalName player
  , loadoutOutputWheelId = gamePlayerRowWheelId player
  , loadoutOutputWheelName = gamePlayerRowWheelName player
  , loadoutOutputRocketTrailId = gamePlayerRowRocketTrailId player
  , loadoutOutputRocketTrailName = gamePlayerRowRocketTrailName player
  , loadoutOutputAntennaId = gamePlayerRowAntennaId player
  , loadoutOutputAntennaName = gamePlayerRowAntennaName player
  , loadoutOutputTopperId = gamePlayerRowTopperId player
  , loadoutOutputTopperName = gamePlayerRowTopperName player
  , loadoutOutputWheelPaintId = gamePlayerRowWheelPaintId player
  , loadoutOutputTopperPaintId = gamePlayerRowTopperPaintId player
  , loadoutOutputPrimaryColorId = gamePlayerRowPrimaryColorId player
  , loadoutOutputAccentColorId = gamePlayerRowAccentColorId player
  , loadoutOutputPrimaryFinishId = gamePlayerRowPrimaryFinishId player
  , loadoutOutputAccentFinishId = gamePlayerRowAccentFinishId player
  }

data CameraOutput = CameraOutput
  { cameraOutputFov :: Float
  , cameraOutputHeight :: Float
  , cameraOutputAngle :: Float
  , cameraOutputDistance :: Float
  , cameraOutputStiffness :: Float
  , cameraOutputSwivelSpeed :: Float
  } deriving (Eq, Common.Generic, Show)

instance Common.ToJSON CameraOutput where
  toJSON = Common.genericToJSON "CameraOutput"

makeCameraOutput :: GamePlayerRow -> CameraOutput
makeCameraOutput player =
  CameraOutput
  { cameraOutputFov = gamePlayerRowFov player
  , cameraOutputHeight = gamePlayerRowHeight player
  , cameraOutputAngle = gamePlayerRowAngle player
  , cameraOutputDistance = gamePlayerRowDistance player
  , cameraOutputStiffness = gamePlayerRowStiffness player
  , cameraOutputSwivelSpeed = gamePlayerRowSwivelSpeed player
  }

getNamesAndTimes :: Sql.Connection
                 -> Common.PlayerId
                 -> IO [(Common.Text, Common.LocalTime)]
getNamesAndTimes connection playerId =
  Database.query
    connection
    [Common.sql|
      SELECT games_players.name, max(games.played_at) AS last_played_at
      FROM games_players
      INNER JOIN games ON games.id = games_players.game_id
      WHERE games_players.player_id = ?
      GROUP BY games_players.name
      ORDER BY last_played_at DESC
    |]
    [playerId]

getPlayerId :: Sql.Connection -> Common.Text -> IO (Maybe Common.PlayerId)
getPlayerId connection rawPlayerId = do
  let maybePlayerId = Read.readMaybe (Text.unpack rawPlayerId)
  rows <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [maybePlayerId :: Maybe Int]
  case rows of
    [[playerId]] -> pure (Just playerId)
    _ -> pure Nothing

getStatsSummaryHandler :: Common.Handler
getStatsSummaryHandler _config connection request = do
  (day, playlists, templates) <- getFilters request
  [[numGames, numBlueWins, numOrangeWins]] <-
    Database.query
      connection
      [Common.sql|
        SELECT
          count(games.id),
          count(CASE WHEN blue_win THEN 1 END),
          count(CASE WHEN not blue_win THEN 1 END)
        FROM games
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          played_at >= ? AND
          playlist_id IN ? AND
          arena_templates.name IN ?
      |]
      (day, Common.In playlists, Common.In templates)
  let blueWinPercentage = makeRatio numBlueWins numGames
  let orangeWinPercentage = makeRatio numOrangeWins numGames
  arenaFrequencies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.name,
          count(games.id)
        FROM games
        INNER JOIN arenas ON arenas.id = games.arena_id
        INNER JOIN arena_templates ON arena_templates.id = arenas.template_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          arena_templates.name IN ? AND
          arenas.name NOT IN ?
        GROUP BY arenas.name
      |]
      (day, Common.In playlists, Common.In templates, Common.In arenaNamesToIgnore)
  let arenaPercents =
        map
          (\(arena, frequency) -> (arena, makeRatio frequency numGames))
          arenaFrequencies
  bodyFrequencies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          coalesce(bodies.name, to_char(games_players.body_id, 'FM9999')) AS body,
          count(games.id)
        FROM games_players
        INNER JOIN games ON games.id = games_players.game_id
        INNER JOIN bodies ON bodies.id = games_players.body_id
        WHERE
          games.played_at >= ? AND
          games.playlist_id IN ? AND
          bodies.name NOT IN ?
        GROUP BY body
      |]
      (day, Common.In playlists, Common.In bodyNamesToIgnore)
  let numBodies = sum (map snd bodyFrequencies)
  let bodyPercents =
        map
          (\(k, frequency) -> (k, makeRatio frequency numBodies))
          bodyFrequencies
  let status = Http.status200
  let headers = []
  let body =
        Aeson.object
          [ ( Text.pack "bodyFreqPct"
            , Aeson.object
                (map (\(k, percent) -> (k, Aeson.toJSON percent)) bodyPercents))
          , ( Text.pack "mapFreqPct"
            , Aeson.object
                (map
                   (\(arena, percent) -> (arena, Aeson.toJSON percent))
                   arenaPercents))
          , ( Text.pack "winPct"
            , Aeson.object
                [ (Text.pack "blue", Aeson.toJSON blueWinPercentage)
                , (Text.pack "orange", Aeson.toJSON orangeWinPercentage)
                ])
          , (Text.pack "numGames", Aeson.toJSON numGames)
          ]
  let response = Common.jsonResponse status headers body
  pure response

makeRatio :: Integer -> Integer -> Float
makeRatio numerator denominator =
  if denominator == 0
    then 0
    else fromRational (numerator Ratio.% denominator)

getFilters :: Wai.Request -> IO (Time.Day, [Int], [String])
getFilters request = do
  let query = Wai.queryString request
  day <- getDay query
  let playlists = getPlaylists query
  let templates = getTemplates query
  pure (day, playlists, templates)

getAfter :: Wai.Request -> Time.LocalTime
getAfter request =
  let query = Wai.queryString request
      whitespace = False
      locale = Time.defaultTimeLocale
      format = "%Y-%m-%dT%H:%M:%S"
      future = Time.LocalTime (Time.fromGregorian 3000 1 1) Time.midnight
  in case Common.getParam "after" query of
    Just input -> case Time.parseTimeM whitespace locale format input of
      Just after -> after
      _ -> future
    _ -> future

getDay :: Common.Query -> IO Time.Day
getDay query = do
  now <- Time.getCurrentTime
  let today = Time.utctDay now
  let day =
        case Common.getParam "time" query of
          Just "month" -> Time.addDays (-28) today
          Just "week" -> Time.addDays (-7) today
          Just "day" -> Time.addDays (-1) today
          _ -> startOfSeason4
  pure day

startOfSeason4 :: Time.Day
startOfSeason4 = Time.fromGregorian 2017 3 22

_startOfSeason3 :: Time.Day
_startOfSeason3 = Time.fromGregorian 2016 6 20

getPlaylists :: Common.Query -> [Int]
getPlaylists query =
  case Common.getParam "playlist" query of
    Just "ranked1v1" -> [competitiveSoloDuel]
    Just "ranked2v2" -> [competitiveDoubles]
    Just "ranked3v3solo" -> [competitiveSoloStandard]
    Just "ranked3v3" -> [competitiveStandard]
    _ -> competitivePlaylists

competitivePlaylists :: [Int]
competitivePlaylists =
  [ competitiveSoloDuel
  , competitiveDoubles
  , competitiveSoloStandard
  , competitiveStandard
  ]

competitiveSoloDuel :: Int
competitiveSoloDuel = 10

competitiveDoubles :: Int
competitiveDoubles = 11

competitiveSoloStandard :: Int
competitiveSoloStandard = 12

competitiveStandard :: Int
competitiveStandard = 13

getTemplates :: Common.Query -> [String]
getTemplates query =
  case Common.getParam "map" query of
    Just "arc" -> [starbaseArcTemplate]
    Just "standard" -> [standardTemplate]
    Just "wasteland" -> [wastelandTemplate]
    _ -> competitiveTemplates

competitiveTemplates :: [String]
competitiveTemplates =
  [standardTemplate, starbaseArcTemplate, wastelandTemplate]

standardTemplate :: String
standardTemplate = "Standard"

starbaseArcTemplate :: String
starbaseArcTemplate = "Starbase ARC"

wastelandTemplate :: String
wastelandTemplate = "Wasteland"

bodyNamesToIgnore :: [String]
bodyNamesToIgnore =
  [ "Armadillo"
  , "Hogsticker"
  , "Sweet Tooth"
  ]

arenaNamesToIgnore :: [String]
arenaNamesToIgnore =
  [ "stadium_winter_p"
  ]
