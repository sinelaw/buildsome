{-# LANGUAGE NoImplicitPrelude #-}
module Lib.Directory
  ( getMFileStatus
  , catchDoesNotExist
  , removeFileOrDirectory
  , removeFileOrDirectoryOrNothing
  , createDirectories
  , getDirectoryContents
  , makeAbsolutePath
  ) where


import Prelude.Compat hiding (FilePath)

import Control.Monad
import Lib.Exception (bracket)
import Lib.FilePath (FilePath, (</>))
import System.IO.Error
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.FilePath as FilePath
import qualified System.Directory as Dir
import qualified System.Posix.ByteString as Posix

catchDoesNotExist :: IO a -> IO a -> IO a
catchDoesNotExist act handler =
  act `E.catch` \e ->
  if isDoesNotExistErrorType (ioeGetErrorType e)
  then handler
  else E.throwIO e

getMFileStatus :: FilePath -> IO (Maybe Posix.FileStatus)
getMFileStatus path = do
  doesExist <- FilePath.exists path
  if doesExist
    then (Just <$> Posix.getFileStatus (FilePath.toBS path)) `catchDoesNotExist` return Nothing
    else return Nothing

createDirectories :: FilePath -> IO ()
createDirectories path
  | FilePath.null path = return ()
  | otherwise = do
    doesExist <- FilePath.exists path
    unless doesExist $ do
      createDirectories $ FilePath.takeDirectory path
      Posix.createDirectory (FilePath.toBS path) 0o777

removeFileByStat :: IO () -> FilePath -> IO ()
removeFileByStat notExist path = do
  mFileStat <- getMFileStatus path
  case mFileStat of
    Nothing -> notExist
    Just fileStat
      | Posix.isRegularFile  fileStat -> Posix.removeLink $ FilePath.toBS path
      | Posix.isSymbolicLink fileStat -> Posix.removeLink $ FilePath.toBS path
      | Posix.isDirectory    fileStat -> Dir.removeDirectoryRecursive $ FilePath.toString path
      | otherwise -> error $ "removeFileOrDirectoryOrNothing: unsupported filestat " ++ show path

removeFileOrDirectoryOrNothing :: FilePath -> IO ()
removeFileOrDirectoryOrNothing = removeFileByStat $ return ()

removeFileOrDirectory :: FilePath -> IO ()
removeFileOrDirectory path =
  removeFileByStat
  -- Try to remove the file when it doesn't exist in order to generate
  -- the meaningful IO exception:
  (Posix.removeLink $ FilePath.toBS path) path

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  bracket (Posix.openDirStream $ FilePath.toBS path) Posix.closeDirStream go
  where
    go dirStream = do
      fn <- Posix.readDirStream dirStream
      if BS8.null fn
        then return []
        else (FilePath.fromBS fn :) <$> go dirStream

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath path = (</> path) . FilePath.fromBS <$> Posix.getWorkingDirectory
