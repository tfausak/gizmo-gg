module Paladin.Main where

import qualified Control.Monad as Monad
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Server as Server
import qualified Paladin.Utility as Utility
import qualified Paladin.Worker as Worker
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
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
  Monad.forever (Utility.sleep 1)
