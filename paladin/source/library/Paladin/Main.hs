module Paladin.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Server as Server
import qualified Paladin.Worker as Worker

main :: IO ()
main = do
  config <- Config.getConfig
  print config
  connection <- Database.connect config
  Database.runMigrations config connection
  _ <- Worker.startWorker config connection
  _ <- Server.startServer config connection
  Monad.forever (Concurrent.threadDelay 1000000)
