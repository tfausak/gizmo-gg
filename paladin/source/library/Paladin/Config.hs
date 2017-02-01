module Paladin.Config where

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Maybe as Maybe
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Envy as Envy

getConfig :: IO Config
getConfig = do
  result <- Envy.decodeEnv
  case result of
    Left message -> fail message
    Right config -> pure config

data Config = Config
  { configDatabase :: ByteString.ByteString
  , configDirectory :: String
  , configMigrate :: Bool
  , configPort :: Warp.Port
  , configServer :: Bool
  , configUrl :: String
  , configWorker :: Bool
  } deriving (Eq, Show)

instance Envy.DefConfig Config where
  defConfig =
    let port = 8080
    in Config
       { configDatabase = ByteString.pack ""
       , configDirectory = "data"
       , configMigrate = True
       , configPort = port
       , configServer = True
       , configUrl = "http://localhost:" ++ show port
       , configWorker = True
       }

instance Envy.FromEnv Config where
  fromEnv = do
    let prefix = "PALADIN_"
    let envMaybe name = Envy.envMaybe (prefix ++ name)
    maybeDatabase <- envMaybe "DATABASE"
    maybeDirectory <- envMaybe "DIRECTORY"
    maybeMigrate <- envMaybe "MIGRATE"
    maybePort <- envMaybe "PORT"
    maybeServer <- envMaybe "SERVER"
    maybeUrl <- envMaybe "CONNECT"
    maybeWorker <- envMaybe "WORKER"
    let withDefault field = Maybe.fromMaybe (field Envy.defConfig)
    pure
      Config
      { configDatabase = withDefault configDatabase maybeDatabase
      , configDirectory = withDefault configDirectory maybeDirectory
      , configMigrate = withDefault configMigrate maybeMigrate
      , configPort = withDefault configPort maybePort
      , configServer = withDefault configServer maybeServer
      , configUrl = withDefault configUrl maybeUrl
      , configWorker = withDefault configWorker maybeWorker
      }
