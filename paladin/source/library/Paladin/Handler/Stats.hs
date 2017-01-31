{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Stats where

import qualified Data.Aeson as Aeson
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
        count(*),
        count(CASE WHEN blue_score > orange_score THEN 1 END),
        count(CASE WHEN blue_score < orange_score THEN 1 END)
      FROM GAMES
    |]
  let ratio :: Integer -> Integer -> Float
      ratio n d = fromRational (n Ratio.% d)
  let blueWinPercentage = ratio numBlueWins numGames
  let orangeWinPercentage = ratio numOrangeWins numGames
  let status = Http.status200
  let headers = []
  let body =
        Aeson.object
          [ ( Text.pack "win_pct"
            , Aeson.object
                [ (Text.pack "blue", Aeson.toJSON blueWinPercentage)
                , (Text.pack "orange", Aeson.toJSON orangeWinPercentage)
                ])
          ]
  let response = Common.jsonResponse status headers body
  pure response
