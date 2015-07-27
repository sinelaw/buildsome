{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, TupleSections #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..)
  , OutputDesc(..)
  , MTimeExecutionLog(..), mtimeExecutionLog
  , ExecutionLog(..), executionLog
  , toMTimeExecutionLog
  , FileContentDescCache(..), fileContentDescCache
  , Reason
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
import Lib.ColorText (ColorText)
import Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import Lib.Exception (bracket)
import Lib.FileDesc (FileContentDesc, FileModeDesc, FileStatDesc)
import Lib.FilePath (FilePath, (</>), (<.>))
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
schemaVersion = "schema.ver.17"

data Db = Db
  { dbLevel :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
  , dbCachedLogs :: IORef (Map ByteString MTimeExecutionLog)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

type Reason = ColorText

data InputDesc a = InputDesc
  { idModeAccess :: Maybe (a, FileModeDesc)
  , idStatAccess :: Maybe (a, FileStatDesc)
  , idContentAccess :: Maybe (a, FileContentDesc)
  } deriving (Generic, Show)
instance Binary a => Binary (InputDesc a)

idDropReasons :: InputDesc a -> InputDesc ()
idDropReasons InputDesc{..} =
    InputDesc
    { idModeAccess = fmap f idModeAccess
    , idStatAccess = fmap f idStatAccess
    , idContentAccess = fmap f idContentAccess
    }
    where f (_,b) = ((), b)

data FileDesc ne e
  = FileDescNonExisting ne
  | FileDescExisting e
  deriving (Generic, Eq, Ord, Show)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

fdescMapNonExisting :: (a -> b) -> FileDesc a e -> FileDesc b e
fdescMapNonExisting f (FileDescNonExisting x) = FileDescNonExisting $ f x
fdescMapNonExisting _ (FileDescExisting e) = FileDescExisting e

fdescMapExisting :: (a -> b) -> FileDesc ne a -> FileDesc ne b
fdescMapExisting _ (FileDescNonExisting x) = FileDescNonExisting x
fdescMapExisting f (FileDescExisting e) = FileDescExisting $ f e


data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show)
instance Binary OutputDesc

data ExecutionLogType =
    LogTypeMTime | LogTypeFull
    deriving (Generic, Show)
instance Binary ExecutionLogType

data MTimeExecutionLog = MTimeExecutionLog
  { elmInputsDescs :: Map FilePath (FileDesc () (POSIXTime, InputDesc ()))
  , elmOutputsDescs :: Map FilePath (FileDesc () OutputDesc)
  , elmSelfTime :: DiffTime
  } deriving (Generic, Show)
instance Binary MTimeExecutionLog

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elInputsDescs :: Map FilePath (FileDesc Reason (POSIXTime, InputDesc Reason))
  , elOutputsDescs :: Map FilePath (FileDesc () OutputDesc)
  , elStdoutputs :: StdOutputs ByteString
  , elSelfTime :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

toMTimeExecutionLog :: ExecutionLog -> MTimeExecutionLog
toMTimeExecutionLog ExecutionLog{..} =
  MTimeExecutionLog
  { elmInputsDescs = M.map (fdescMapExisting f . (fdescMapNonExisting $ const ())) elInputsDescs
  , elmOutputsDescs = M.map ((fdescMapNonExisting $ const ())) $ elOutputsDescs
  , elmSelfTime = elSelfTime
  }
  where f (ptime, idesc) = (ptime, idDropReasons idesc)

registeredOutputsRef :: Db -> IORef (Set FilePath)
registeredOutputsRef = dbRegisteredOutputs

leakedOutputsRef :: Db -> IORef (Set FilePath)
leakedOutputsRef = dbLeakedOutputs

setKey :: Binary a => Db -> ByteString -> a -> IO ()
setKey db key val = LevelDB.put (dbLevel db) def key $ encode val

getKey :: (Show a, Binary a) => Db -> ByteString -> IO (Maybe a)
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
  cache <- newIORef M.empty
  withLevelDb dbPath $ \levelDb ->
    withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
    withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
    body (Db levelDb registeredOutputs leakedOutputs cache)
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

mkIRefKey :: (Show a, Binary a) => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

-- TODO: Canonicalize commands (whitespace/etc)
targetHash :: Makefile.TargetType output input -> ByteString
targetHash = MD5.hash . Makefile.targetCmds

mtimeExecutionLog :: Makefile.Target -> Db -> IRef MTimeExecutionLog
mtimeExecutionLog t db = ir
  {
    readIRef = do
      cache <- readIORef $ dbCachedLogs db
      case M.lookup th cache of
        Nothing -> readIRef ir
        Just x -> return $ Just x
  , writeIRef = \v -> do
      _ <- atomicModifyIORef' (dbCachedLogs db)
           (\cache -> (M.insert th v cache, ()))
      writeIRef ir $ v
  , delIRef = do
      _ <- atomicModifyIORef' (dbCachedLogs db)
           (\cache -> (M.delete th cache, ()))
      delIRef ir
  }
  where
    th = targetHash t
    ir = mkIRefKey (encode . (LogTypeMTime, ) $ targetHash t) db

executionLog :: Makefile.Target -> Db -> IRef ExecutionLog
executionLog = mkIRefKey . encode . (LogTypeFull, ) . targetHash

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache = mkIRefKey

type MFileContentDesc = FileDesc () FileContentDesc

data MakefileParseCache = MakefileParseCache
  { mpcInputs :: (FilePath, Map FilePath MFileContentDesc)
  , mpcOutput :: (Makefile, [PutStrLn])
  } deriving (Generic, Show)
instance Binary MakefileParseCache

makefileParseCache :: Db -> Makefile.Vars -> IRef MakefileParseCache
makefileParseCache db vars =
    mkIRefKey ("makefileParseCache_Schema.1:" <> MD5.hash (encode vars)) db
