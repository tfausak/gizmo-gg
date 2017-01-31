module Paladin.Handler.Root where

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as Http
import qualified Paladin.Handler.Common as Common

getRootHandler :: Common.Handler
getRootHandler _config _connection _request =
  pure (Common.jsonResponse Http.status200 [] Aeson.Null)
