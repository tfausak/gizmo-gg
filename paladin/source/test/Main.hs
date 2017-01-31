{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test
import qualified Paladin

main :: IO ()
main = do
  config <- Paladin.getConfig
  connection <- Paladin.connect config
  Paladin.runMigrations config connection
  let application = Paladin.makeApplication config connection
  let applicationWithMiddleware = Paladin.applyMiddleware application
  Test.runSession session applicationWithMiddleware

session :: Test.Session ()
session = do
  do let request = Test.defaultRequest
     response <- Test.request request
     assertJson 200 Aeson.Null response
  do let request = Test.defaultRequest & setPath "/not-found"
     response <- Test.request request
     assertJson 404 Aeson.Null response
  do let request = Test.defaultRequest & setPath "/arenas"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Arena]) response
  do let request = Test.defaultRequest & setPath "/game-modes"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.GameMode]) response
  do let request = Test.defaultRequest & setPath "/game-types"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.GameType]) response
  do let request = Test.defaultRequest & setPath "/games-players"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.GamePlayer]) response
  do let request = Test.defaultRequest & setPath "/games"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Game]) response
  do let request = Test.defaultRequest & setPath "/parse-errors"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.ParseError]) response
  do let request = Test.defaultRequest & setPath "/parsers"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Parser]) response
  do let request = Test.defaultRequest & setPath "/platforms"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Platform]) response
  do let request = Test.defaultRequest & setPath "/players"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Player]) response
  do let request = Test.defaultRequest & setPath "/playlists"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Playlist]) response
  do let request = Test.defaultRequest & setPath "/replays"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Replay]) response
  do let request = Test.defaultRequest & setPath "/servers"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Server]) response
  do let request = Test.defaultRequest & setPath "/stats/summary"
     response <- Test.request request
     let json =
           Aeson.object
             [ ( "win_pct"
               , Aeson.object
                   [("blue", Aeson.Number 0), ("orange", Aeson.Number 0)])
             ]
     assertJson 200 json response
  do let request = Test.defaultRequest & setPath "/uploads"
     response <- Test.request request
     assertJson 200 ([] :: [Paladin.Upload]) response
  do let request = Test.defaultRequest & setPath "/uploads/invalid-id"
     response <- Test.request request
     assertJson 400 Aeson.Null response

assertJson
  :: Aeson.ToJSON json
  => Int -> json -> Test.SResponse -> Test.Session ()
assertJson status json response = do
  Test.assertStatus status response
  Test.assertContentType "application/json" response
  let body = Aeson.encode json
  Test.assertBody body response

setPath :: ByteString.ByteString -> Wai.Request -> Wai.Request
setPath path request = Test.setPath request path
