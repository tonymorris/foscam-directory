{-# LANGUAGE NoImplicitPrelude #-}

module Data.Foscam.Directory(
  getFoscamDirectoryContents
, FoscamDirectoryFile(Isn't, Is)
, _FoscamDirectoryFileIso
, _FoscamDirectory
, _FoscamFilename
, _Isn't
, _Is
, _Isn'tFilename
, _IsFilename
) where

import Control.Category((.))
import Control.Lens(Lens', Prism', Traversal', Iso', iso, prism', _1, _2)
import Data.ByteString.UTF8 as UTF8(fromString)
import Data.Either(Either(Left, Right))
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

_FoscamDirectoryFileIso ::
  Iso'
    FoscamDirectoryFile
    (FilePath, Either FilePath Filename)
_FoscamDirectoryFileIso =
  iso
    (\d -> case d of
             Isn't p n -> (p, Left n)
             Is    p n -> (p, Right n))
    (\(p, n) -> case n of
                  Left q -> Isn't p q
                  Right q -> Is   p q)

_FoscamDirectory ::
  Lens' FoscamDirectoryFile FilePath
_FoscamDirectory =
  _FoscamDirectoryFileIso . _1

_FoscamFilename ::
  Lens' FoscamDirectoryFile (Either FilePath Filename)
_FoscamFilename =
  _FoscamDirectoryFileIso . _2

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
