module Jalopy.Server where

import Data.Function ((&))

import qualified Control.Concurrent as Concurrent
import qualified Database.PostgreSQL.Simple as Sql
import qualified Jalopy.Config as Config
import qualified Jalopy.Router as Router
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as Logger

startServer :: Config.Config -> Sql.Connection -> IO Concurrent.ThreadId
startServer config connection = do
  let application = makeApplication config connection
  let applicationWithMiddleware = applyMiddleware application
  let port = Config.configPort config
  let run = Warp.run port applicationWithMiddleware
  Concurrent.forkIO run

makeApplication :: Config.Config -> Sql.Connection -> Wai.Application
makeApplication config connection request respond = do
  let handler = Router.route request
  response <- handler config connection request
  respond response

applyMiddleware :: Wai.Middleware
applyMiddleware application =
  application & Gzip.gzip Gzip.def & Logger.logStdout
