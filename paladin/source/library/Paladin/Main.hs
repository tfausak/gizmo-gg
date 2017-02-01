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
  Monad.when
    (Config.configWorker config)
    (do _ <- Worker.startWorker config connection
        pure ())
  Monad.when
    (Config.configServer config)
    (do _ <- Server.startServer config connection
        pure ())
  Monad.forever (Concurrent.threadDelay 1000000)
