{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Paladin.Handler.Uploads where

import Data.Function ((&))

import qualified Control.Monad as Monad
import qualified Crypto.Hash as Hash
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Parse as Parse
import qualified Paladin.Config as Config
import qualified Paladin.Database as Database
import qualified Paladin.Handler.Common as Common
import qualified Paladin.Storage as Storage
import qualified Paladin.Utility as Utility
import qualified System.FilePath as FilePath
import qualified Text.Read as Read

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
          Monad.unless fileExists (saveUpload config digest contents)
          uploadId <- insertUpload connection name digest contents
          let url = "/uploads/" ++ show uploadId
          pure
            (Common.jsonResponse
               Http.status303
               [(Http.hLocation, ByteString.pack (Common.makeUrl config url))]
               ())
        _ -> pure (Common.jsonResponse Http.status415 [] Aeson.Null)
    _ -> do
      let code = 422
      let message = ByteString.pack "Unprocessable Entity"
      let status = Http.mkStatus code message
      let headers = []
      let body = Aeson.Null
      let response = Common.jsonResponse status headers body
      pure response

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
        ON CONFLICT (hash) DO UPDATE SET hash = excluded.hash
        RETURNING id
      |]
      (hash, name, size)
  pure uploadId

getUploadHandler :: Common.Text -> Common.Handler
getUploadHandler rawUploadId config connection _request =
  let notFound = Common.jsonResponse Http.status404 [] Aeson.Null
  in case rawUploadId & Text.unpack & Read.readMaybe & fmap Common.Tagged of
    Nothing -> pure notFound
    Just uploadId -> do
      maybeUpload <- getUpload connection uploadId
      case maybeUpload of
        Nothing -> pure notFound
        Just upload -> do
          maybeReplay <- case Common.uploadReplayId upload of
            Nothing -> pure Nothing
            Just replayId -> getReplay connection replayId
          let uploadInfo = makeUploadInfo config upload maybeReplay
          pure (Common.jsonResponse Http.status200 [] uploadInfo)

data UploadInfo = UploadInfo
  { uploadInfoState :: String
  , uploadInfoGame :: Maybe String
  } deriving (Common.Generic)

instance Common.ToJSON UploadInfo where
  toJSON = Common.genericToJSON "uploadInfo"

makeUploadInfo
  :: Common.Config
  -> Common.Upload
  -> Maybe Common.Replay
  -> UploadInfo
makeUploadInfo config upload maybeReplay =
  case maybeReplay of
    Just replay -> UploadInfo
      { uploadInfoState = "success"
      , uploadInfoGame = Just (Common.makeUrl config ("/games/" ++ show (Common.tagValue (Common.replayGameId replay))))
      }
    Nothing -> UploadInfo
      { uploadInfoState = case Common.uploadFinishedParsingAt upload of
        Just _ -> "failure"
        Nothing -> "pending"
      , uploadInfoGame = Nothing
      }

getUpload :: Sql.Connection -> Common.UploadId -> IO (Maybe Common.Upload)
getUpload connection uploadId = do
  uploads <- Database.query connection [Common.sql|
    select
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
    from uploads
    where id = ?
  |] [uploadId]
  pure (Maybe.listToMaybe uploads)

getReplay
  :: Sql.Connection
  -> Common.ReplayId
  -> IO (Maybe Common.Replay)
getReplay connection replayId = do
  replays <- Database.query connection [Common.sql|
    select
      id,
      created_at,
      major_version,
      minor_version,
      recorded_at,
      custom_name,
      duration,
      game_id
    from replays
    where id = ?
  |] [replayId]
  pure (Maybe.listToMaybe replays)
