{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Buildsome.ContentCache where

import qualified Buildsome.Color as Color
import qualified Buildsome.Db as Db
import           Buildsome.Types (Buildsome(..), BuildTargetEnv(..))
import           Control.Monad (unless, when, forM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Either (EitherT(..), left)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Codec.Compression.Zlib as ZLib
import           Data.Monoid
import           Data.String (IsString(..))
import           Lib.Directory (getMFileStatus)
import qualified Lib.Directory as Dir
import           Lib.FileDesc (FileContentDesc(..), BasicStatEssence(..), FullStatEssence(..), FileStatDesc(..))
import           Lib.FilePath (FilePath, (</>))
import qualified Lib.FilePath as FilePath
import           Lib.Printer (printStrLn)
import           Lib.Hash (Hash)
import qualified Lib.Hash as Hash
import           Lib.Show (show)
import qualified System.Posix.ByteString as Posix

import           Prelude.Compat hiding (FilePath, show)


const_CACHED_OUTPUTS_DIR :: FilePath
const_CACHED_OUTPUTS_DIR = "cached_outputs"
const_MAX_CACHE_SIZE :: Integer
const_MAX_CACHE_SIZE = 4 * 1024 * 1024 * 1024

contentCacheDir :: Buildsome -> FilePath
contentCacheDir buildsome = bsBuildsomePath buildsome </> const_CACHED_OUTPUTS_DIR

filesToDelete :: Integral a => a -> [(FilePath, Maybe Posix.FileStatus)] -> (a, [(FilePath, a)])
filesToDelete maxSize fs = foldr go (0, []) fs
    where
        go (fileName, Just stat)
            | Posix.isRegularFile stat = addFile fileName stat
            | otherwise                = id
        go (_fileName, Nothing)        = id

        addFile fileName stat (size, outFiles) = (newSize, newFiles)
            where
                fileSize = fromIntegral $ Posix.fileSize stat
                newSize = size + fileSize
                newFiles =
                    if (newSize > maxSize)
                    then (fileName, fileSize):outFiles
                    else outFiles

toKb :: Integral a => a -> a
toKb x = x `div` 1024

cleanContentCacheDir :: Buildsome -> IO ()
cleanContentCacheDir buildsome = do
  Dir.createDirectories $ contentCacheDir buildsome
  putStr "Checking cache dir size..."
  savedSize <- Db.readIRef $ Db.cachedOutputsUsage (bsDb buildsome)
  case savedSize of
      Just x | x < const_MAX_CACHE_SIZE -> putStrLn $ "OK, " <> show (toKb $ const_MAX_CACHE_SIZE - x) <> "kb left before cleanup."
      _ -> cleanContentCacheDir' buildsome

cleanContentCacheDir' :: Buildsome -> IO ()
cleanContentCacheDir' buildsome = do
  files <- Dir.getDirectoryContents (contentCacheDir buildsome)
      >>= mapM (return . ((contentCacheDir buildsome <> "/") <>))
      >>= mapM (\fileName -> (fileName,) <$> getMFileStatus fileName)
  let (totalSize, filesToRemove) = filesToDelete const_MAX_CACHE_SIZE $ sortOn (fmap Posix.modificationTimeHiRes . snd) files
      numRemoved = length $ filesToRemove
      bytesSaved = sum (map snd filesToRemove)
  if numRemoved > 0
      then putStrLn $ concat
           [ "Cache dir ", show (contentCacheDir buildsome)
           , " contains ", show (length files)
           , " files, totaling ", show (toKb totalSize)
           , "kb. Going to remove ", show numRemoved, " oldest cache files"
           , ", saving ", show (toKb $ bytesSaved), "kb." ]
      else putStr "Updating from disk..." >> putStrLn "OK."
  forM_ filesToRemove (Posix.removeLink . fst)
  Db.writeIRef (Db.cachedOutputsUsage (bsDb buildsome)) $ totalSize - bytesSaved

mkTargetWithHashPath :: Buildsome -> Hash -> FilePath
mkTargetWithHashPath buildsome contentHash = contentCacheDir buildsome </> Base16.encode (Hash.asByteString contentHash)-- (outPath <> "." <> Base16.encode contentHash)

compress :: FilePath -> FilePath -> IO ()
compress inFile outFile = BS.readFile (BS8.unpack inFile) >>= (BS.writeFile (BS8.unpack outFile) . ZLib.compress)

decompress :: FilePath -> FilePath -> IO ()
decompress inFile outFile = BS.readFile (BS8.unpack inFile) >>= (BS.writeFile (BS8.unpack outFile) . ZLib.decompress)

addFileToCache :: Buildsome -> FilePath -> Posix.FileStatus -> Hash -> IO ()
addFileToCache buildsome outPath _stat contentHash = do
  let targetPath = mkTargetWithHashPath buildsome contentHash
  -- putStrLn $ BS8.unpack ("Caching: " <> outPath <> " -> " <> targetPath)
  alreadyExists <- FilePath.exists targetPath
  unless alreadyExists $ do
    Dir.createDirectories $ FilePath.takeDirectory targetPath
    compress outPath targetPath
    compressedStat <- Posix.getFileStatus targetPath
    savedSize <- fromMaybe 0 <$> Db.readIRef (Db.cachedOutputsUsage $ bsDb buildsome)
    Db.writeIRef (Db.cachedOutputsUsage (bsDb buildsome))
        $ savedSize + (fromIntegral $ Posix.fileSize compressedStat)

refreshFromContentCache :: (IsString e, MonadIO m) =>
  BuildTargetEnv -> FilePath -> Maybe FileContentDesc -> Maybe FileStatDesc -> EitherT e m ()
refreshFromContentCache
  BuildTargetEnv{..} filePath (Just (FileContentDescRegular contentHash)) (Just (FileStatOther fullStat)) = do
  liftIO (FilePath.exists cachedPath) >>= \oldExists ->
    unless oldExists $ left "No cached copy"
  liftIO $ do
    printStrLn btePrinter $ bsRender bteBuildsome
      $ mconcat [ "Restoring: " <> cPath (show cachedPath) <> " -> " <> cPath (show filePath) ]
    removeIfExists filePath
    removeIfExists tempFile
    -- Update the cached file's mtime so as to make cache cleanup by last usage possible
    Posix.touchFile cachedPath

    Dir.createDirectories $ FilePath.takeDirectory filePath
    -- Set stat attributes before creating the file, so the target file is created with correct
    -- attrs from the start
    -- TODO use proper tempfile naming
    decompress cachedPath tempFile
    -- TODO set other stat fields?
    -- TODO these may cause failure later (over permissions) if the user running now is not the one
    -- recorded in the log!
    Posix.setFileMode tempFile (fileMode $ basicStatEssence fullStat)
    Posix.setOwnerAndGroup tempFile (fileOwner $ basicStatEssence fullStat) (fileGroup $ basicStatEssence fullStat)
    Dir.renameFile tempFile filePath
    Posix.setFileTimesHiRes filePath (statusChangeTimeHiRes fullStat) (modificationTimeHiRes fullStat)

  where Color.Scheme{..} = Color.scheme
        cachedPath = mkTargetWithHashPath bteBuildsome contentHash
        tempFile = filePath <> "._buildsome_temp"
        removeIfExists f =
          FilePath.exists f >>= \exists -> when exists $ Posix.removeLink f

refreshFromContentCache _ _ _ _ = left "No cached info"
