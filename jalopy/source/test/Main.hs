{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Jalopy
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Test

main :: IO ()
main = do
  config <- Jalopy.getConfig
  connection <- Jalopy.connect config
  Jalopy.runMigrations config connection
  let application = Jalopy.makeApplication config connection
  let applicationWithMiddleware = Jalopy.applyMiddleware application
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
     assertJson 200 ([] :: [Jalopy.Arena]) response
  do let request = Test.defaultRequest & setPath "/game-modes"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.GameMode]) response
  do let request = Test.defaultRequest & setPath "/game-types"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.GameType]) response
  do let request = Test.defaultRequest & setPath "/games-players"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.GamePlayer]) response
  do let request = Test.defaultRequest & setPath "/games"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Game]) response
  do let request = Test.defaultRequest & setPath "/parse-errors"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.ParseError]) response
  do let request = Test.defaultRequest & setPath "/parsers"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Parser]) response
  do let request = Test.defaultRequest & setPath "/platforms"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Platform]) response
  do let request = Test.defaultRequest & setPath "/players"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Player]) response
  do let request = Test.defaultRequest & setPath "/playlists"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Playlist]) response
  do let request = Test.defaultRequest & setPath "/replays"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Replay]) response
  do let request = Test.defaultRequest & setPath "/servers"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Server]) response
  do let request = Test.defaultRequest & setPath "/uploads"
     response <- Test.request request
     assertJson 200 ([] :: [Jalopy.Upload]) response
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
