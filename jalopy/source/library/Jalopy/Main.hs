module Jalopy.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Jalopy.Config as Config
import qualified Jalopy.Database as Database
import qualified Jalopy.Server as Server
import qualified Jalopy.Worker as Worker

main :: IO ()
main = do
  config <- Config.getConfig
  print config
  connection <- Database.connect config
  Database.runMigrations config connection
  _ <- Worker.startWorker config connection
  _ <- Server.startServer config connection
  Monad.forever (Concurrent.threadDelay 1000000)
