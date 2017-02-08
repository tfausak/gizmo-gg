{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Text.Read as Read

getStatsPlayersHandler :: Common.Text -> Common.Handler
getStatsPlayersHandler rawPlayerId _config connection _request = do
  let playerId = Maybe.fromMaybe 0 (Read.readMaybe (Text.unpack rawPlayerId))
  maybePlayerId <-
    Database.query
      connection
      [Common.sql| SELECT id FROM players WHERE id = ? |]
      [playerId :: Int]
  case maybePlayerId :: [[Int]] of
    [[_]] -> do
      [[numBlueGames, numOrangeGames, numBlueWins, numOrangeWins, totalScore, totalGoals, totalAssists, totalSaves, totalShots, secondsPlayed]] <-
        Database.query
          connection
          [Common.sql|
            SELECT
              count(CASE WHEN games_players.is_blue THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue THEN 1 END),
              count(CASE WHEN games_players.is_blue AND games.blue_score > games.orange_score THEN 1 END),
              count(CASE WHEN NOT games_players.is_blue and games.orange_score > games.blue_score THEN 1 END),
              sum(games_players.score),
              sum(games_players.goals),
              sum(games_players.assists),
              sum(games_players.saves),
              sum(games_players.shots),
              sum(replays.duration)
            FROM games
            INNER JOIN games_players
              ON games_players.game_id = games.id
            INNER JOIN replays
              ON replays.game_id = games.id
            WHERE
              games_players.player_id = ? AND
              games_players.is_present_at_end = true
          |]
          [playerId]
      let numGames = numBlueGames + numOrangeGames
      let numWins = numBlueWins + numOrangeWins
      let body =
            Aeson.object
              [ (Text.pack "num_games", Aeson.toJSON numGames)
              , (Text.pack "num_wins", Aeson.toJSON numWins)
              , (Text.pack "num_losses", Aeson.toJSON (numGames - numWins))
              , ( Text.pack "win_pct"
                , Aeson.toJSON (makeRatio numWins numGames))
              , (Text.pack "num_blue_games", Aeson.toJSON numBlueGames)
              , (Text.pack "num_blue_wins", Aeson.toJSON numBlueWins)
              , ( Text.pack "num_blue_losses"
                , Aeson.toJSON (numBlueGames - numBlueWins))
              , ( Text.pack "blue_win_pct"
                , Aeson.toJSON (makeRatio numBlueWins numBlueGames))
              , (Text.pack "num_orange_wins", Aeson.toJSON numOrangeWins)
              , ( Text.pack "num_orange_losses"
                , Aeson.toJSON (numOrangeGames - numOrangeWins))
              , ( Text.pack "orange_win_pct"
                , Aeson.toJSON (makeRatio numOrangeWins numOrangeGames))
              , (Text.pack "total_score", Aeson.toJSON totalScore)
              , (Text.pack "total_goals", Aeson.toJSON totalGoals)
              , (Text.pack "total_assists", Aeson.toJSON totalAssists)
              , (Text.pack "total_saves", Aeson.toJSON totalSaves)
              , (Text.pack "total_shots", Aeson.toJSON totalShots)
              , (Text.pack "secondsPlayed", Aeson.toJSON secondsPlayed)
              , ( Text.pack "score_per_second"
                , Aeson.toJSON (makeRatio totalScore secondsPlayed))
              , ( Text.pack "goals_per_second"
                , Aeson.toJSON (makeRatio totalGoals secondsPlayed))
              , ( Text.pack "assists_per_second"
                , Aeson.toJSON (makeRatio totalAssists secondsPlayed))
              , ( Text.pack "saves_per_second"
                , Aeson.toJSON (makeRatio totalSaves secondsPlayed))
              , ( Text.pack "shots_per_second"
                , Aeson.toJSON (makeRatio totalShots secondsPlayed))
              ]
      pure (Common.jsonResponse Http.status200 [] body)
    _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)

getStatsSummaryHandler :: Common.Handler
getStatsSummaryHandler _config connection request = do
  let query = Wai.queryString request
  let startOfSeason = Time.fromGregorian 2016 6 20
  now <- Time.getCurrentTime
  let today = Time.utctDay now
  let time =
        case lookup (ByteString.pack "time") query of
          Just (Just x) ->
            case ByteString.unpack x of
              "month" -> Time.addDays (-28) today
              "week" -> Time.addDays (-7) today
              _ -> startOfSeason
          _ -> startOfSeason
  let allRanked = [10, 11, 12, 13] :: [Int]
  let playlists =
        case lookup (ByteString.pack "playlist") query of
          Just (Just x) ->
            case ByteString.unpack x of
              "ranked1v1" -> [10]
              "ranked2v2" -> [11]
              "ranked3v3solo" -> [12]
              "ranked3v3" -> [13]
              _ -> allRanked
          _ -> allRanked
  [[numGames, numBlueWins, numOrangeWins]] <-
    Database.query
      connection
      [Common.sql|
        SELECT
          count(*),
          count(CASE WHEN games.blue_score > games.orange_score THEN 1 END),
          count(CASE WHEN games.blue_score < games.orange_score THEN 1 END)
        FROM replays
        INNER JOIN games
          ON games.id = replays.game_id
        WHERE
          replays.recorded_at >= ? AND
          games.playlist_id IN ?
      |]
      (time, Common.In playlists)
  let blueWinPercentage = makeRatio numBlueWins numGames
  let orangeWinPercentage = makeRatio numOrangeWins numGames
  arenaFrequencies <-
    Database.query
      connection
      [Common.sql|
        SELECT
          arenas.name,
          count(*)
        FROM replays
        INNER JOIN games
          ON games.id = replays.game_id
        INNER JOIN arenas
          ON arenas.id = games.arena_id
        WHERE
          replays.recorded_at >= ? AND
          games.playlist_id IN ?
        GROUP BY arenas.name
      |]
      (time, Common.In playlists)
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
          count(*)
        FROM games_players
        INNER JOIN games
          ON games.id = games_players.game_id
        INNER JOIN replays
          ON replays.game_id = games.id
        INNER JOIN bodies
          ON bodies.id = games_players.body_id
        WHERE
          replays.recorded_at >= ? AND
          games.playlist_id IN ?
        GROUP BY body
      |]
      (time, Common.In playlists)
  let numBodies = sum (map snd bodyFrequencies)
  let bodyPercents =
        map
          (\(k, frequency) -> (k, makeRatio frequency numBodies))
          bodyFrequencies
  let status = Http.status200
  let headers = []
  let body =
        Aeson.object
          [ ( Text.pack "body_freq_pct"
            , Aeson.object
                (map (\(k, percent) -> (k, Aeson.toJSON percent)) bodyPercents))
          , ( Text.pack "map_freq_pct"
            , Aeson.object
                (map
                   (\(arena, percent) -> (arena, Aeson.toJSON percent))
                   arenaPercents))
          , ( Text.pack "win_pct"
            , Aeson.object
                [ (Text.pack "blue", Aeson.toJSON blueWinPercentage)
                , (Text.pack "orange", Aeson.toJSON orangeWinPercentage)
                ])
          ]
  let response = Common.jsonResponse status headers body
  pure response

makeRatio :: Integer -> Integer -> Float
makeRatio numerator denominator =
  if denominator == 0
    then 0
    else fromRational (numerator Ratio.% denominator)
