{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Lib.Directory
  ( getMFileStatus
  , catchDoesNotExist
  , removeFileOrDirectory
  , removeFileOrDirectoryOrNothing
  , createDirectories
  , getDirectoryContents
  , getDirectoryContentsHash
  , makeAbsolutePath
  , copyFile
  , renameFile
  ) where


import Prelude.Compat hiding (FilePath)

import Data.Monoid ((<>))
import Control.Monad
import Lib.Exception (bracket)
import Lib.FilePath (FilePath, (</>))
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.FilePath as FilePath
import qualified System.Directory as Dir
import qualified System.Posix.ByteString as Posix
import qualified Lib.Hash as Hash
import           Lib.Hash (Hash)

catchDoesNotExist :: IO a -> IO a -> IO a
catchDoesNotExist act handler =
  act `E.catch` \e ->
  if isDoesNotExistErrorType (ioeGetErrorType e)
  then handler
  else E.throwIO e

getMFileStatus :: FilePath -> IO (Maybe Posix.FileStatus)
getMFileStatus path = {-# SCC "getMFileStatus" #-} do
  doesExist <- FilePath.exists path
  if doesExist
    then (Just <$> Posix.getFileStatus path) `catchDoesNotExist` return Nothing
    else return Nothing

createDirectories :: FilePath -> IO ()
createDirectories path
  | BS8.null path = return ()
  | otherwise = do
    doesExist <- FilePath.exists path
    unless doesExist $ do
      createDirectories $ FilePath.takeDirectory path
      Posix.createDirectory path 0o777

removeFileByStat :: IO () -> FilePath -> IO ()
removeFileByStat notExist path = do
  mFileStat <- getMFileStatus path
  case mFileStat of
    Nothing -> notExist
    Just fileStat
      | Posix.isRegularFile  fileStat -> Posix.removeLink path
      | Posix.isSymbolicLink fileStat -> Posix.removeLink path
      | Posix.isDirectory    fileStat -> Dir.removeDirectoryRecursive $ BS8.unpack path
      | otherwise -> error $ "removeFileOrDirectoryOrNothing: unsupported filestat " ++ show path

removeFileOrDirectoryOrNothing :: FilePath -> IO ()
removeFileOrDirectoryOrNothing = removeFileByStat $ return ()

removeFileOrDirectory :: FilePath -> IO ()
removeFileOrDirectory path =
  removeFileByStat
  -- Try to remove the file when it doesn't exist in order to generate
  -- the meaningful IO exception:
  (Posix.removeLink path) path

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  bracket (Posix.openDirStream path) Posix.closeDirStream go
  where
    go dirStream = do
      fn <- Posix.readDirStream dirStream
      if BS8.null fn
        then return []
        else (fn :) <$> go dirStream

getDirectoryContentsHash :: FilePath -> IO Hash
getDirectoryContentsHash path = {-# SCC "getDirectoryContentsHash" #-}
  bracket (Posix.openDirStream path) Posix.closeDirStream (go Hash.empty)
  where
    go !hash !dirStream = do
      fn <- Posix.readDirStream dirStream
      if BS8.null fn
        then return hash
        else go (hash <> Hash.md5 fn) dirStream

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath path = (</> path) <$> Posix.getWorkingDirectory

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = Posix.createLink src dst --Dir.copyFile (BS8.unpack src) (BS8.unpack dst)

renameFile :: FilePath -> FilePath -> IO ()
renameFile src dst = Posix.rename src dst
