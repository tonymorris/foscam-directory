module Data.Foscam.Directory(
  getFoscamDirectoryContents
) where

import Data.Foscam.File.Filename
import System.Directory
import Text.Trifecta
import Text.Trifecta.Delta
import Data.ByteString.UTF8 as UTF8
import Prelude

data FoscamDirectoryFile =
  Isn't FilePath
  | Is Filename
  deriving (Eq, Ord, Show)

getFoscamDirectoryContents ::
  FilePath
  -> IO [FoscamDirectoryFile]
getFoscamDirectoryContents p =
  let fromResult (Success n) =
        Is n
      fromResult (Failure _) =
        Isn't p
  in map (fromResult . parseString filename (Directed (UTF8.fromString p) 0 0 0 0)) <$> getDirectoryContents p
