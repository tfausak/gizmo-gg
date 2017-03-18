module Paladin.Utility
  ( module Export
  , module Paladin.Utility
  ) where

import Paladin.Utility.Guid as Export
import Paladin.Utility.Tagged as Export

import qualified Control.Concurrent as Concurrent

sleep :: Int -> IO ()
sleep seconds = Concurrent.threadDelay (seconds * 1000000)
