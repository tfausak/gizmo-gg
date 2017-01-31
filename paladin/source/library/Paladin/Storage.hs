module Paladin.Storage where

import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Paladin.Config as Config
import qualified Paladin.Utility as Utility
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

getUploadFile :: Config.Config
              -> Utility.Tagged Hash.SHA1 String
              -> IO LazyByteString.ByteString
getUploadFile config hash = do
  path <- getUploadFilePath config hash
  LazyByteString.readFile path

doesUploadFileExist :: Config.Config
                    -> Utility.Tagged Hash.SHA1 String
                    -> IO Bool
doesUploadFileExist config hash = do
  path <- getUploadFilePath config hash
  Directory.doesFileExist path

putUploadFile
  :: Config.Config
  -> Utility.Tagged Hash.SHA1 String
  -> LazyByteString.ByteString
  -> IO ()
putUploadFile config hash contents = do
  path <- getUploadFilePath config hash
  LazyByteString.writeFile path contents

getUploadFilePath :: Config.Config
                  -> Utility.Tagged Hash.SHA1 String
                  -> IO FilePath
getUploadFilePath config hash = do
  let name = Utility.tagValue hash
  let dataDirectory = Config.configDirectory config
  let directory = FilePath.joinPath [dataDirectory, "uploads", take 2 name]
  Directory.createDirectoryIfMissing True directory
  let path = FilePath.joinPath [directory, name]
  pure path
