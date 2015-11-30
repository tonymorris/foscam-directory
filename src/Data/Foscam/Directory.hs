{-# LANGUAGE NoImplicitPrelude #-}

module Data.Foscam.Directory(
  getFoscamDirectoryContents
, FoscamDirectoryFile(Isn't, Is)
, _FoscamDirectory
, _Isn't
, _Is
, _Isn'tFilename
, _IsFilename
) where

import Control.Category((.))
import Control.Lens(Lens', Prism', Traversal', lens, prism', _1, _2)
import Data.ByteString.UTF8 as UTF8(fromString)
import Data.Eq(Eq)
import Data.Foscam.File.Filename(Filename, filename)
import Data.Functor(fmap, (<$>))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import System.Directory(getDirectoryContents)
import System.IO(IO, FilePath)
import Text.Trifecta(Result(Success, Failure), parseString)
import Text.Trifecta.Delta(Delta(Directed))
import Prelude(Show)

data FoscamDirectoryFile =
  Isn't FilePath FilePath
  | Is FilePath Filename
  deriving (Eq, Ord, Show)

getFoscamDirectoryContents ::
  FilePath
  -> IO [FoscamDirectoryFile]
getFoscamDirectoryContents p =
  (fmap (\f -> 
    case parseString filename (Directed (UTF8.fromString p) 0 0 0 0) f of
      Success n -> Is p n
      Failure _ -> Isn't p f)) <$> getDirectoryContents p

_FoscamDirectory ::
  Lens' FoscamDirectoryFile FilePath
_FoscamDirectory =
  lens
    (\d -> case d of 
             Isn't p _ -> p
             Is    p _ -> p)
    (\d p -> case d of 
               Isn't _ x -> Isn't p x
               Is    _ x -> Is p x)

_Isn't ::
  Prism' FoscamDirectoryFile (FilePath, FilePath)
_Isn't =
  prism'
    (\(d, p) -> Isn't d p)
    (\d -> case d of
             Isn't p x -> Just (p, x)
             Is    _ _ -> Nothing)

_Is ::
  Prism' FoscamDirectoryFile (FilePath, Filename)
_Is =
  prism'
    (\(d, p) -> Is d p)
    (\d -> case d of
             Isn't _ _ -> Nothing
             Is    p x -> Just (p, x))

_IsFilename ::
  Traversal' FoscamDirectoryFile Filename
_IsFilename =
  _Is . _2

_Isn'tFilename ::
  Traversal' FoscamDirectoryFile FilePath
_Isn'tFilename =
  _Is . _1
