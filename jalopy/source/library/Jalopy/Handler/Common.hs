{-# LANGUAGE ScopedTypeVariables #-}

module Jalopy.Handler.Common
  ( module Export
  , module Jalopy.Handler.Common
  ) where

import Database.PostgreSQL.Simple.SqlQQ as Export (sql)
import Jalopy.Entity as Export

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Proxy as Proxy
import qualified Database.PostgreSQL.Simple as Sql
import qualified Jalopy.Config as Config
import qualified Jalopy.Database as Database
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

type Handler = Config.Config -> Sql.Connection -> Wai.Request -> IO Wai.Response

jsonHandler
  :: forall a.
     (Sql.FromRow a, Aeson.ToJSON a)
  => Proxy.Proxy a -> Sql.Query -> Handler
jsonHandler _proxy query _config connection _request = do
  rows <- Database.query_ connection query
  pure (jsonResponse Http.status200 [] (rows :: [a]))

jsonResponse
  :: Aeson.ToJSON a
  => Http.Status -> Http.ResponseHeaders -> a -> Wai.Response
jsonResponse status otherHeaders json =
  let headers =
        (Http.hContentType, ByteString.pack "application/json") : otherHeaders
      body = Aeson.encode json
  in Wai.responseLBS status headers body

notFoundHandler :: Handler
notFoundHandler _config _connection _request =
  pure (jsonResponse Http.status404 [] Aeson.Null)

makeUrl :: Config.Config -> String -> String
makeUrl config url = Config.configUrl config ++ url
