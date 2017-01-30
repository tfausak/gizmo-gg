module Jalopy.Database where

import qualified Data.ByteString.Char8 as ByteString
import qualified Database.PostgreSQL.Simple as Sql
import qualified Database.PostgreSQL.Simple.Migration as Migration
import qualified Jalopy.Config as Config

connect :: Config.Config -> IO Sql.Connection
connect config = Sql.connectPostgreSQL (Config.configDatabase config)

runMigrations :: Config.Config -> Sql.Connection -> IO ()
runMigrations config connection = do
  let verbose = True
  let initialize =
        Migration.MigrationContext
          Migration.MigrationInitialization
          verbose
          connection
  let migrate =
        Migration.MigrationContext
          (Migration.MigrationDirectory "migrations")
          verbose
          connection
  if Config.configMigrate config
    then do
      _ <- Migration.runMigration initialize
      _ <- Migration.runMigration migrate
      pure ()
    else pure ()

execute
  :: Sql.ToRow row
  => Sql.Connection -> Sql.Query -> row -> IO ()
execute connection q row = do
  sql <- Sql.formatQuery connection q row
  ByteString.putStrLn sql
  _ <- Sql.execute connection q row
  pure ()

query
  :: (Sql.ToRow rowIn, Sql.FromRow rowOut)
  => Sql.Connection -> Sql.Query -> rowIn -> IO [rowOut]
query connection q row = do
  sql <- Sql.formatQuery connection q row
  ByteString.putStrLn sql
  Sql.query connection q row

query_
  :: Sql.FromRow row
  => Sql.Connection -> Sql.Query -> IO [row]
query_ connection q = do
  sql <- Sql.formatQuery connection q ()
  ByteString.putStrLn sql
  Sql.query_ connection q
