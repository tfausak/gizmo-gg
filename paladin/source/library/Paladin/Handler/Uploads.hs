{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Uploads where

import Data.Function ((&))

import qualified Crypto.Hash as Hash
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as Sql
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Paladin.Storage as Storage
import qualified Paladin.Utility as Utility
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Parse as Parse
import qualified System.FilePath as FilePath
import qualified Text.Read as Read

getUploadsHandler :: Common.Handler
getUploadsHandler =
  Common.jsonHandler
    Common.uploadProxy
    [Common.sql|
      SELECT
        id,
        created_at,
        name,
        size,
        hash,
        started_parsing_at,
        parser_id,
        finished_parsing_at,
        parse_error_id,
        replay_id
      FROM uploads
      ORDER BY created_at DESC
    |]

postUploadHandler :: Common.Handler
postUploadHandler config connection request = do
  let options =
        Parse.defaultParseRequestBodyOptions &
        Parse.setMaxRequestFileSize 10485760 &
        Parse.setMaxRequestKeyLength 100 &
        Parse.setMaxRequestNumFiles 1
  let backEnd = Parse.lbsBackEnd
  (_params, files) <- Parse.parseRequestBodyEx options backEnd request
  case lookup (ByteString.pack "replay") files of
    Just file -> do
      let name = ByteString.unpack (Parse.fileName file)
      case FilePath.takeExtension name of
        ".replay" -> do
          let contents = Parse.fileContent file
          let digest = Hash.hashlazy contents
          let hash = Utility.Tagged (show digest)
          fileExists <- Storage.doesUploadFileExist config hash
          if fileExists
            then pure (Common.jsonResponse Http.status400 [] Aeson.Null)
            else do
              saveUpload config digest contents
              uploadId <- insertUpload connection name digest contents
              let url = "/uploads/" ++ show uploadId
              pure
                (Common.jsonResponse
                   Http.status303
                   [ ( Http.hLocation
                     , ByteString.pack (Common.makeUrl config url))
                   ]
                   ())
        _ -> pure (Common.jsonResponse Http.status400 [] Aeson.Null)
    _ -> pure (Common.jsonResponse Http.status400 [] Aeson.Null)

saveUpload :: Config.Config
           -> Hash.Digest Hash.SHA1
           -> LazyByteString.ByteString
           -> IO ()
saveUpload config digest contents = do
  let hash = Utility.Tagged (show digest)
  Storage.putUploadFile config hash contents

insertUpload
  :: Sql.Connection
  -> String
  -> Hash.Digest Hash.SHA1
  -> LazyByteString.ByteString
  -> IO Int
insertUpload connection name digest contents = do
  let hash = show digest
  let size = LazyByteString.length contents
  [Sql.Only uploadId] <-
    Database.query
      connection
      [Common.sql|
        INSERT INTO uploads (hash, name, size)
        VALUES (?, ?, ?)
        RETURNING id
      |]
      (hash, name, size)
  pure uploadId

getUploadHandler :: Common.Text -> Common.Handler
getUploadHandler rawUploadId _config connection _request =
  case rawUploadId & Text.unpack & Read.readMaybe of
    Nothing -> pure (Common.jsonResponse Http.status400 [] Aeson.Null)
    Just uploadId -> do
      uploads <-
        Database.query
          connection
          [Common.sql|
            SELECT
              id,
              created_at,
              name,
              size,
              hash,
              started_parsing_at,
              parser_id,
              finished_parsing_at,
              parse_error_id,
              replay_id
            FROM uploads
            WHERE id = ?
          |]
          [uploadId :: Int]
      case uploads of
        [upload] ->
          pure
            (Common.jsonResponse Http.status200 [] (upload :: Common.Upload))
        _ -> pure (Common.jsonResponse Http.status404 [] Aeson.Null)
