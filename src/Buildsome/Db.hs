{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDesc(..), FileDesc(..)
  , OutputDesc(..)
  , ExecutionLog(..), executionLog
  , ExecutionSummary(..), executionSummary
  , summarizeExecutionLog
  , FileContentDescCache(..), fileContentDescCache
  , Reason
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Prelude.Compat          hiding (FilePath)

import           Buildsome.BuildId       (BuildId)
import qualified Crypto.Hash.MD5         as MD5
import           Data.Binary             (Binary (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS8
import           Data.Default            (def)
import           Data.IORef
import           Data.Map                (Map)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Time.Clock         (DiffTime)
import           Data.Time.Clock.POSIX   (POSIXTime)
import qualified Database.LevelDB.Base   as LevelDB
import           GHC.Generics            (Generic)
import           Lib.Binary              (decode, encode)
import           Lib.ColorText           (ColorText)
import           Lib.Directory           (catchDoesNotExist, createDirectories, makeAbsolutePath)
import           Lib.Exception           (bracket)
import           Lib.FileDesc            (FileContentDesc, FileModeDesc, FileStatDesc)
import           Lib.FilePath            (FilePath, (<.>), (</>))
import qualified Lib.FSHook              as FSHook
import           Lib.Makefile            (Makefile)
import qualified Lib.Makefile            as Makefile
import           Lib.Makefile.Monad      (PutStrLn)
import           Lib.StdOutputs          (StdOutputs (..))
import           Lib.TimeInstances       ()
import qualified System.Posix.ByteString as Posix

schemaVersion :: ByteString
schemaVersion = "schema.ver.18"

data Db = Db
  { dbLevel             :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs     :: IORef (Set FilePath)
  }

data FileContentDescCache = FileContentDescCache
  { fcdcModificationTime :: POSIXTime
  , fcdcFileContentDesc  :: FileContentDesc
  } deriving (Generic, Show)
instance Binary FileContentDescCache

type Reason = ColorText

data InputDesc = InputDesc
  { idModeAccess    :: Maybe (Reason, FileModeDesc)
  , idStatAccess    :: Maybe (Reason, FileStatDesc)
  , idContentAccess :: Maybe (Reason, FileContentDesc)
  } deriving (Generic, Show)
instance Binary InputDesc

inputDescAccessType :: InputDesc -> Maybe FSHook.AccessType
inputDescAccessType InputDesc { idContentAccess = Just _ } = Just FSHook.AccessTypeFull
inputDescAccessType InputDesc { idStatAccess = Just _ } = Just FSHook.AccessTypeStat
inputDescAccessType InputDesc { idModeAccess = Just _ } = Just FSHook.AccessTypeModeOnly
inputDescAccessType _ = Nothing

data FileDesc ne e
  = FileDescNonExisting !ne
  | FileDescExisting !e
  deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)
instance (Binary ne, Binary e) => Binary (FileDesc ne e)

bimapFileDesc :: (ne -> ne') -> (e -> e') -> FileDesc ne e -> FileDesc ne' e'
bimapFileDesc f _ (FileDescNonExisting x) = FileDescNonExisting (f x)
bimapFileDesc _ g (FileDescExisting x) = FileDescExisting (g x)

data OutputDesc = OutputDesc
  { odStatDesc    :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show)
instance Binary OutputDesc

data ExecutionSummary = ExecutionSummary
  { esInputsDescs  :: Map FilePath (FileDesc () (POSIXTime, FSHook.AccessType))
  , esOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , esSelfTime     :: DiffTime
  , esStdErr       :: ByteString
  } deriving (Generic, Show)
instance Binary ExecutionSummary

data ExecutionLog = ExecutionLog
  { elBuildId      :: BuildId
  , elCommand      :: ByteString -- Mainly for debugging
  , elInputsDescs  :: Map FilePath (FileDesc Reason (POSIXTime, InputDesc))
  , elOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , elStdoutputs   :: StdOutputs ByteString
  , elSelfTime     :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

second :: (b -> b') -> (a, b) -> (a, b')
second f (x, y) = (x, f y)

summarizeExecutionLog :: ExecutionLog -> ExecutionSummary
summarizeExecutionLog ExecutionLog{..} =
  ExecutionSummary
  { esInputsDescs = bimapFileDesc (const ()) (second assertAccessType) <$> elInputsDescs
  , esOutputsDescs = elOutputsDescs
  , esSelfTime = elSelfTime
  , esStdErr = stdErr elStdoutputs
  }
  where
    assertAccessType =
      fromMaybe (error "Input accessed but no access type?!") . inputDescAccessType

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
  withLevelDb dbPath $ \levelDb ->
    withIORefFile (dbPath </> "outputs") $ \registeredOutputs ->
    withIORefFile (dbPath </> "leaked_outputs") $ \leakedOutputs ->
    body (Db levelDb registeredOutputs leakedOutputs)
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
  { readIRef  :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef   :: IO ()
  }

mkIRefKey :: Binary a => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = getKey db key
  , writeIRef = setKey db key
  , delIRef = deleteKey db key
  }

data ExecutionLogType = ExecutionLogTypeSummary  | ExecutionLogTypeFull
    deriving (Generic)

instance Binary ExecutionLogType

targetKey :: Makefile.TargetType output input -> ByteString
targetKey = MD5.hash . Makefile.targetCmds

executionLog :: Makefile.Target -> Db -> IRef ExecutionLog
executionLog = mkIRefKey . (encode ExecutionLogTypeFull <>) . targetKey

executionSummary :: Makefile.Target -> Db -> IRef ExecutionSummary
executionSummary = mkIRefKey . (encode ExecutionLogTypeSummary <>) .  targetKey

fileContentDescCache :: FilePath -> Db -> IRef FileContentDescCache
fileContentDescCache = mkIRefKey

type MFileContentDesc = FileDesc () FileContentDesc

data MakefileParseCache = MakefileParseCache
  { mpcInputs :: (FilePath, Map FilePath MFileContentDesc)
  , mpcOutput :: (Makefile, [PutStrLn])
  } deriving (Generic)
instance Binary MakefileParseCache

makefileParseCache :: Db -> Makefile.Vars -> IRef MakefileParseCache
makefileParseCache db vars =
    mkIRefKey ("makefileParseCache_Schema.1:" <> MD5.hash (encode vars)) db
