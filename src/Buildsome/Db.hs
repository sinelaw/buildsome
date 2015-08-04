{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Buildsome.Db
  ( Db(..), with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..)
  , OutputDesc(..)
  , ExecutionLog(..), executionLog
  , FileContentDescCache(..), fileContentDescCache
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import Prelude.Compat hiding (FilePath)

import Buildsome.BuildId (BuildId)

import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.IORef
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Lib.Binary (encode, decode)
import Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import Lib.Exception (bracket)
import Lib.FileDesc (FileContentDesc, FileModeDesc, FileStatDesc)
import Lib.FilePath (FilePath, (</>), (<.>))
import qualified Lib.FSHook as FSHook
import Lib.Makefile (Makefile)
import Lib.Makefile.Monad (PutStrLn)
import Lib.StdOutputs (StdOutputs(..))
import Lib.TimeInstances ()
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Database.LevelDB.Base as LevelDB
import qualified Lib.Makefile as Makefile
import qualified System.Posix.ByteString as Posix

schemaVersion :: ByteString
schemaVersion = "schema.ver.19"

data Db = Db
  { dbLevel :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  , dbCachedStats :: IORef (Map FilePath (Maybe Posix.FileStatus))
  , dbCachedLogs :: IORef (Bool, Map FilePath ExecutionLog)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

data Reason
  = BecauseSpeculative Reason
  | BecauseHintFrom [FilePath]
  | BecauseHooked FSHook.AccessDoc
  | BecauseContainerDirectoryOfInput Reason FilePath
  | BecauseContainerDirectoryOfOutput FilePath
  | BecauseInput Reason FilePath
  | BecauseRequested ByteString
  deriving (Generic, Show)
instance Binary Reason


data InputDesc = InputDesc
  { idModeAccess :: Maybe (Reason, FileModeDesc)
  , idStatAccess :: Maybe (Reason, FileStatDesc)
  , idContentAccess :: Maybe (Reason, FileContentDesc)
  } deriving (Generic, Show)
instance Binary InputDesc

data FileDesc ne e
  = FileDescNonExisting ne
  | FileDescExisting e
  deriving (Generic, Eq, Ord, Show)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show)
instance Binary OutputDesc

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elCommand :: ByteString -- Mainly for debugging
  , elInputsDescs :: Map FilePath (FileDesc Reason (POSIXTime, InputDesc))
  , elOutputsDescs :: Map FilePath (FileDesc () OutputDesc)
  , elStdoutputs :: StdOutputs ByteString
  , elSelfTime :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = LevelDB.put (dbLevel db) def key $ encode val

getKey :: Binary a => Db -> ByteString -> IO (Maybe a)
getKey db key = fmap decode <$> LevelDB.get (dbLevel db) def key

deleteKey :: Db -> ByteString -> IO ()
deleteKey db = LevelDB.delete (dbLevel db) def

options :: LevelDB.Options
options =
    LevelDB.defaultOptions
    { LevelDB.createIfMissing = True
    , LevelDB.errorIfExists = False
    }

withLevelDb :: FilePath -> (LevelDB.DB -> IO a) -> IO a
withLevelDb dbPath =
  LevelDB.withDB (BS8.unpack (dbPath </> schemaVersion)) options

with :: FilePath -> (Db -> IO a) -> IO a
with rawDbPath body = do
  dbPath <- makeAbsolutePath rawDbPath
  createDirectories dbPath
  statCache <- newIORef M.empty
  logCache <- newIORef (False, M.empty)
  withLevelDb dbPath $ \levelDb ->
    withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
    withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs -> do
      let db = (Db levelDb registeredOutputs leakedOutputs statCache logCache)
      load db
      r <- body db
      save db
      return r
  where
    withIORefFile path =
      bracket (newIORef =<< decodeFileOrEmpty path) (writeBack path)
    writeBack path ref = do
      BS8.writeFile (BS8.unpack (path <.> "tmp")) .
        BS8.unlines . S.toList =<< readIORef ref
      Posix.rename (path <.> "tmp") path
    decodeFileOrEmpty path =
      (S.fromList . BS8.lines <$> BS8.readFile (BS8.unpack path))
      `catchDoesNotExist` return S.empty

data IRef a = IRef
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef :: IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

mkMemIRef :: Ord k => k -> IORef (Bool, Map k a) -> IRef a
mkMemIRef th ioRef = IRef
   {
    readIRef = (M.lookup th . snd) <$> readIORef ioRef
   , writeIRef = \v -> do
      atomicModifyIORef' ioRef
          (\(_, cache) -> ((True, M.insert th v cache), ()))
   , delIRef = do
      atomicModifyIORef' ioRef
          (\(_, cache) -> ((True, M.delete th cache), ()))
   }

executionLog :: Makefile.Target -> Db -> IRef ExecutionLog
executionLog target db = mkMemIRef targetKey (dbCachedLogs db)
  where
    targetKey = MD5.hash $ Makefile.targetCmds target -- TODO: Canonicalize commands (whitespace/etc)

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache = mkIRefKey

load' :: (Binary k, Binary a) => String -> IORef (Bool, (Map k a)) -> IO ()
load' name ioRef = do
    putStrLn "Loading db..."
    cacheBS <- BS8.readFile name
    let cache = if BS8.null cacheBS
                 then M.empty
                 else decode cacheBS
    putStrLn "Done loading db."
    atomicWriteIORef ioRef (False, cache)

save' :: Binary a => String -> IORef (Bool, a) -> IO ()
save' name ioRef = do
    putStrLn "Saving db..."
    (changed, val) <- readIORef ioRef
    if changed
    then BS8.writeFile name $ encode val
    else return ()
    putStrLn "Done saving db."

load :: Db -> IO ()
load db = do
    load' "Buildsome.mk.db/mtimeLog" (dbCachedLogs db)

save :: Db -> IO ()
save db = do
    save' "Buildsome.mk.db/mtimeLog" (dbCachedLogs db)


type MFileContentDesc = FileDesc () FileContentDesc

data MakefileParseCache = MakefileParseCache
  { mpcInputs :: (FilePath, Map FilePath MFileContentDesc)
  , mpcOutput :: (Makefile, [PutStrLn])
  } deriving (Generic)
instance Binary MakefileParseCache

makefileParseCache :: Db -> Makefile.Vars -> IRef MakefileParseCache
makefileParseCache db vars =
    mkIRefKey ("makefileParseCache_Schema.1:" <> MD5.hash (encode vars)) db
