{-# LANGUAGE NoImplicitPrelude #-}

module Data.Foscam.Directory(
  getFoscamDirectoryContents
, FoscamDirectoryFile(Isn't, Is)
, filenames
) where

import Control.Category(id)
import Data.ByteString.UTF8 as UTF8(fromString)
import Data.Eq(Eq)
import Data.Foscam.File.Filename(Filename, filename)
import Data.Functor(fmap, (<$>))
import Data.List(foldr)
import Data.Ord(Ord)
import System.Directory(getDirectoryContents)
import System.IO(IO, FilePath)
import Text.Trifecta(Result(Success, Failure), parseString)
import Text.Trifecta.Delta(Delta(Directed))
import Prelude(Show)

data FoscamDirectoryFile =
  Isn't FilePath
  | Is FilePath Filename
  deriving (Eq, Ord, Show)

getFoscamDirectoryContents ::
  FilePath
  -> IO [FoscamDirectoryFile]
getFoscamDirectoryContents p =
  (fmap (\f -> 
    case parseString filename (Directed (UTF8.fromString p) 0 0 0 0) f of
      Success n -> Is p n
      Failure _ -> Isn't f)) <$> getDirectoryContents p

filenames ::
  [FoscamDirectoryFile]
  -> [(FilePath, Filename)]
filenames =
  foldr (\f -> case f of
                 Isn't _ -> id
                 Is p n -> ((p,n):)) []
