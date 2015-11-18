{-# LANGUAGE NoImplicitPrelude #-}

module Data.Foscam.Directory(
  getFoscamDirectoryContents
, FoscamDirectoryFile(Isn't, Is)
) where

import Data.Foscam.File.Filename(Filename, filename)
import System.Directory(getDirectoryContents)
import Text.Trifecta(Result(Success, Failure), parseString)
import Text.Trifecta.Delta(Delta(Directed))
import Data.ByteString.UTF8 as UTF8(fromString)
import Data.Eq(Eq)
import Data.Ord(Ord)
import System.IO(IO, FilePath)
import Control.Category((.))
import Data.Functor(fmap, (<$>))
import Prelude(Show)

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
  in fmap (fromResult . parseString filename (Directed (UTF8.fromString p) 0 0 0 0)) <$> getDirectoryContents p
