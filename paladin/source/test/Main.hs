module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as ByteString
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
  assertStatus 200 "/"
  assertStatus 404 "/not-found"
  assertStatus 200 "/arenas"
  assertStatus 200 "/game-modes"
  assertStatus 200 "/game-types"
  assertStatus 200 "/games-players"
  assertStatus 200 "/games"
  assertStatus 200 "/parse-errors"
  assertStatus 200 "/parsers"
  assertStatus 200 "/platforms"
  assertStatus 200 "/players"
  assertStatus 200 "/playlists"
  assertStatus 200 "/replays"
  assertStatus 200 "/servers"
  assertStatus 200 "/stats/bodies"
  assertStatus 200 "/stats/summary"
  assertStatus 200 "/uploads"
  assertStatus 400 "/uploads/invalid-id"
  assertStatus 404 "/uploads/0"

assertStatus :: Int -> String -> Test.Session ()
assertStatus status rawPath = do
  let path = ByteString.pack rawPath
  let request = Test.setPath Test.defaultRequest path
  response <- Test.request request
  Test.assertStatus status response
