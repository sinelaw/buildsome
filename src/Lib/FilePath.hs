{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, OverloadedStrings, UnboxedTuples #-}
module Lib.FilePath
  ( FilePath
  , isAbsolute, null, toString
  , splitFileName
  , canonicalizePath
  , canonicalizePathAsRelative
  , dropTrailingPathSeparator, addTrailingPathSeparator
  , (</>), (<.>)
  , takeDirectory, takeFileName
  , makeRelative, makeRelativeToCurrentDirectory
  , exists
  , toBS, fromBS
  ) where


import Prelude.Compat hiding (FilePath, null)
import qualified Data.ByteString.Short as SB
import Control.Exception (catch, throwIO)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.IO.Exception (IOErrorType(..))
import Lib.ByteString (unprefixed)
import System.IO.Error (ioeGetErrorType)
import qualified Data.ByteString.Char8 as BS8
import qualified System.Posix.ByteString as Posix
import Data.Char (chr)
import Data.Binary (Binary(..))

type FilePath = SB.ShortByteString

instance Binary SB.ShortByteString where
  get = SB.toShort <$> get
  put = put . SB.fromShort

null :: SB.ShortByteString -> Bool
null = SB.null
toString :: SB.ShortByteString -> String
toString = map (chr . fromIntegral) . SB.unpack
toBS :: SB.ShortByteString -> ByteString
toBS = SB.fromShort
fromBS :: ByteString -> SB.ShortByteString
fromBS = SB.toShort

{-# INLINE exists #-}
exists :: FilePath -> IO Bool
exists path
  | SB.null path = return True
  | otherwise = Posix.fileExist (SB.fromShort path) `catch`
    \e ->
    case ioeGetErrorType e of
    InappropriateType -> return False
    _ -> throwIO e

splitPath :: FilePath -> [FilePath]
splitPath path
  | isAbsolute path = map SB.toShort $ "/" : BS8.split '/' (BS8.tail $ SB.fromShort path)
  | otherwise = map SB.toShort $ BS8.split '/' (SB.fromShort path)

joinPath :: [FilePath] -> FilePath
joinPath (path:paths) | path == "/" = SB.toShort ("/" <> BS8.intercalate "/" (map SB.fromShort paths))
joinPath       paths                = SB.toShort $        BS8.intercalate "/" $ map SB.fromShort paths

onFst :: (a -> a') -> (a, b) -> (a', b)
onFst f (x, y) = (f x, y)

splitFileName :: FilePath -> (FilePath, SB.ShortByteString)
splitFileName = onFst joinPath . f . splitPath
  where
    f [] = ([], "")
    f [x] = ([], x)
    f (x:xs) = onFst (x:) $ f xs

takeDirectory :: FilePath -> FilePath
takeDirectory = joinPath . reverse . drop 1 . reverse . splitPath

takeFileName :: FilePath -> FilePath
takeFileName path =
  case splitPath path of
  [] -> ""
  xs -> last xs

removeRedundantComponents :: FilePath -> FilePath
removeRedundantComponents =
  joinPath .
  foldr step [] .
  filter (/= ".") .
  splitPath
  where
    step "/" xs = "/" : xs
    step ".." xs = ".." : xs
    step _ ("..":xs) = xs
    step x xs = x:xs

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator x
  | "/" `BS8.isSuffixOf` (SB.fromShort x) = SB.toShort $ BS8.init $ SB.fromShort x
  | otherwise = x

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator x
  | "/" `BS8.isSuffixOf` (SB.fromShort x) = x
  | otherwise = x <> "/"

(</>) :: FilePath -> FilePath -> FilePath
"." </> y = y
"" </> y = y
x </> y
  | isAbsolute y = y
  | otherwise = dropTrailingPathSeparator x <> "/" <> y

(<.>) :: FilePath -> ByteString -> FilePath
f <.> g = f <> "." <> (SB.toShort g)

isAbsolute :: FilePath -> Bool
isAbsolute = ("/" `BS8.isPrefixOf`) . SB.fromShort

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative prefix full =
  case unprefixed prefix' full' of
  Nothing -> full
  Just suffix -> SB.toShort $ fromMaybe suffix $ unprefixed "/" suffix
  where
    prefix' = SB.fromShort $ dropTrailingPathSeparator prefix
    full' = SB.fromShort $ dropTrailingPathSeparator full

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory path = (`makeRelative` path) . SB.toShort <$> Posix.getWorkingDirectory

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  curDir <- SB.toShort <$> Posix.getWorkingDirectory
  return $ dropTrailingPathSeparator $ removeRedundantComponents $ curDir </> path

canonicalizePathAsRelative :: FilePath -> IO FilePath
canonicalizePathAsRelative path = makeRelativeToCurrentDirectory =<< canonicalizePath path
