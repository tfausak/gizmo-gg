{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Fixed as Fixed
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common

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
          replays.recorded_at >= ?
      |]
      [time]
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
          replays.recorded_at >= ?
        GROUP BY arenas.name
      |]
      [time]
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
          replays.recorded_at >= ?
        GROUP BY body
      |]
      [time]
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

makeRatio :: Integer -> Integer -> Fixed.Centi
makeRatio numerator denominator =
  if denominator == 0
    then 0
    else fromRational (numerator Ratio.% denominator)
