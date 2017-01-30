module Jalopy.Config where

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
  , configUrl :: String
  } deriving (Eq, Show)

instance Envy.DefConfig Config where
  defConfig =
    let port = 8080
    in Config
       { configDatabase = ByteString.pack ""
       , configDirectory = "data"
       , configMigrate = True
       , configPort = port
       , configUrl = "http://localhost:" ++ show port
       }

instance Envy.FromEnv Config where
  fromEnv = do
    maybeDatabase <- Envy.envMaybe "JALOPY_DATABASE"
    maybeDirectory <- Envy.envMaybe "JALOPY_DIRECTORY"
    maybeMigrate <- Envy.envMaybe "JALOPY_MIGRATE"
    maybePort <- Envy.envMaybe "JALOPY_PORT"
    maybeUrl <- Envy.envMaybe "JALOPY_CONNECT"
    let withDefault field = Maybe.fromMaybe (field Envy.defConfig)
    pure
      Config
      { configDatabase = withDefault configDatabase maybeDatabase
      , configDirectory = withDefault configDirectory maybeDirectory
      , configMigrate = withDefault configMigrate maybeMigrate
      , configPort = withDefault configPort maybePort
      , configUrl = withDefault configUrl maybeUrl
      }
