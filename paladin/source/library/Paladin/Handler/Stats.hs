{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
import qualified Data.Fixed as Fixed
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common

getStatsSummaryHandler :: Common.Handler
getStatsSummaryHandler _config connection _request = do
  [[numGames, numBlueWins, numOrangeWins]] <-
    Database.query_
      connection
      [Common.sql|
        SELECT
          count(id),
          count(CASE WHEN blue_score > orange_score THEN 1 END),
          count(CASE WHEN blue_score < orange_score THEN 1 END)
        FROM games
      |]
  let blueWinPercentage = makeRatio numBlueWins numGames
  let orangeWinPercentage = makeRatio numOrangeWins numGames
  arenaFrequencies <-
    Database.query_
      connection
      [Common.sql|
        SELECT
          arenas.name,
          count(games.id)
        FROM games
        INNER JOIN arenas
          ON arenas.id = games.arena_id
        GROUP BY arenas.name
      |]
  let arenaPercents =
        map
          (\(arena, frequency) -> (arena, makeRatio frequency numGames))
          arenaFrequencies
  let status = Http.status200
  let headers = []
  let body =
        Aeson.object
          [ ( Text.pack "map_freq_pct"
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
