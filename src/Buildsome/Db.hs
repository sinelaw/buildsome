{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Buildsome.Db
  ( Db, with
  , registeredOutputsRef, leakedOutputsRef
  , InputDescWith(..)
  , InputDesc, inputDescDropReasons
  , OutputDesc(..)
  , ExecutionLog(..)
  , executionLogUpdate
  , executionLogLookup
  , latestExecutionLog
  , FileDescInput, FileDescInputNoReasons
  , FileContentDescCache(..), fileContentDescCache
  , Reason(..)
  , IRef(..)
  , MFileContentDesc, MakefileParseCache(..), makefileParseCache
  ) where

import           Buildsome.BuildId (BuildId)
import qualified Crypto.Hash.MD5 as MD5
import           Data.Binary (Binary(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Default (def)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Time.Clock (DiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Database.LevelDB.Base as LevelDB
import           GHC.Generics (Generic)
import           Lib.Binary (encode, decode)
import           Lib.Directory (catchDoesNotExist, createDirectories, makeAbsolutePath)
import           Lib.Exception (bracket)
import qualified Lib.FSHook as FSHook
import           Lib.FileDesc (FileDesc(..), bimapFileDesc, FileContentDesc, FileModeDesc, FileStatDesc)
import           Lib.FilePath (FilePath, (</>), (<.>))
import           Lib.Makefile (Makefile)
import qualified Lib.Makefile as Makefile
import           Lib.Makefile.Monad (PutStrLn)

import           Lib.NonEmptyMap (NonEmptyMap)
import qualified Lib.NonEmptyMap as NonEmptyMap
import           Lib.StdOutputs (StdOutputs(..))
import           Lib.TimeInstances ()
import qualified System.Posix.ByteString as Posix
import           Control.Monad (forM)

import           Prelude.Compat hiding (FilePath)

schemaVersion :: ByteString
schemaVersion = "schema.ver.21"

data Db = Db
  { dbLevel :: LevelDB.DB
  , dbRegisteredOutputs :: IORef (Set FilePath)
  , dbLeakedOutputs :: IORef (Set FilePath)
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
  | BecauseChildOfFullyRequestedDirectory Reason
  | BecauseContainerDirectoryOfInput Reason FilePath
  | BecauseContainerDirectoryOfOutput FilePath
  | BecauseInput Reason FilePath
  | BecauseRequested ByteString
  deriving (Generic, Show, Ord, Eq)
instance Binary Reason


data InputDescWith a = InputDescWith
  { idModeAccess :: Maybe (a, FileModeDesc)
  , idStatAccess :: Maybe (a, FileStatDesc)
  , idContentAccess :: Maybe (a, FileContentDesc)
  } deriving (Generic, Show, Ord, Eq, Functor)
instance Binary a => Binary (InputDescWith a)

type InputDesc = InputDescWith Reason

inputDescDropReasons :: InputDesc -> InputDescWith ()
inputDescDropReasons = fmap (const ())

data OutputDesc = OutputDesc
  { odStatDesc :: FileStatDesc
  , odContentDesc :: Maybe FileContentDesc -- Nothing if directory
  } deriving (Generic, Show, Eq)
instance Binary OutputDesc

-- TODO: naming...
type FileDescInput = FileDesc Reason (POSIXTime, InputDesc)
type FileDescInputNoReasons = FileDesc () (InputDescWith ())

fileDescInputDropReasons :: FileDescInput -> FileDescInputNoReasons
fileDescInputDropReasons = bimapFileDesc (const ()) (inputDescDropReasons . snd)

data ExecutionLog = ExecutionLog
  { elBuildId :: BuildId
  , elCommand :: ByteString
  , elInputsDescs :: Map FilePath FileDescInput
  , elOutputsDescs :: Map FilePath (FileDesc () (POSIXTime, OutputDesc))
  , elStdoutputs :: StdOutputs ByteString
  , elSelfTime :: DiffTime
  } deriving (Generic, Show)
instance Binary ExecutionLog

data ExecutionLogNodeKey = ExecutionLogNodeKey ByteString -- [(FilePath, FileDescInputNoReasons)]
  deriving (Generic, Show)
instance Binary ExecutionLogNodeKey

data ExecutionLogNode
  = ExecutionLogNodeBranch (NonEmptyMap FilePath (NonEmptyMap FileDescInputNoReasons ExecutionLogNodeKey))
  | ExecutionLogNodeLeaf ExecutionLog
  deriving (Generic, Show)
instance Binary ExecutionLogNode


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
  { readIRef :: IO (Maybe a)
  , writeIRef :: a -> IO ()
  , delIRef :: IO ()
  }

data TargetLogType
  = TargetLogLatestExecutionLog
  | TargetLogExecutionLogNode
  deriving (Show, Eq, Ord, Generic)
instance Binary TargetLogType

mkIRefKey :: (Binary a) => ByteString -> Db -> IRef a
mkIRefKey key db = IRef
  { readIRef = {-# SCC "readIRef" #-} getKey db key
  , writeIRef = {-# SCC "writeIRef" #-} setKey db key
  , delIRef = {-# SCC "deleteKey" #-} deleteKey db key
  }

-- TODO: Canonicalize commands (whitespace/etc)
targetKey :: TargetLogType -> Makefile.Target -> ByteString
targetKey targetLogType target = MD5.hash $ encode targetLogType <> Makefile.targetCmds target

executionLogNode :: ExecutionLogNodeKey -> Db -> IRef ExecutionLogNode
executionLogNode (ExecutionLogNodeKey k) =
    mkIRefKey k

executionLogLookup :: Makefile.Target -> Db -> (FilePath -> IO FileDescInputNoReasons) -> IO (Maybe ExecutionLog)
executionLogLookup target db getCurFileDesc =
    executionLogLookup' (executionLogNode (executionLogNodeRootKey target) db) db getCurFileDesc

maybeCmp :: Eq a => Maybe a -> Maybe a -> Bool
maybeCmp (Just x) (Just y) = x == y
maybeCmp _        _        = True

cmpFileDescInput :: FileDescInputNoReasons -> FileDescInputNoReasons -> Bool
cmpFileDescInput (FileDescExisting a) (FileDescExisting b)   =
    maybeCmp (idModeAccess a) (idModeAccess b)
    && maybeCmp (idStatAccess a) (idStatAccess b)
    && maybeCmp (idContentAccess a) (idContentAccess b)
cmpFileDescInput FileDescNonExisting{} FileDescNonExisting{} = True
cmpFileDescInput _                    _                      = False

executionLogLookup' :: IRef ExecutionLogNode -> Db -> (FilePath -> IO FileDescInputNoReasons) -> IO (Maybe ExecutionLog)
executionLogLookup' iref db getCurFileDesc = {-# SCC "executionLogLookup'" #-} do
    eln <- readIRef iref
    case eln of
        Nothing -> return Nothing
        Just (ExecutionLogNodeLeaf el) -> return $ Just el
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            logs <-
                forM (NonEmptyMap.toList mapOfMaps) $ \(filePath, mapOfFileDescs) -> do
                    curFileDesc <- getCurFileDesc filePath
                    let matchingKeys = map snd $ filter ((cmpFileDescInput curFileDesc) . fst) (NonEmptyMap.toList mapOfFileDescs)
                    case matchingKeys of
                        [] -> return Nothing
                        (k:_) -> executionLogLookup' (executionLogNode k db) db getCurFileDesc
            case logs of
            case catMaybes logs of
                [] -> return Nothing
                (x:_) -> return $ Just x

executionLogNodeKey :: ExecutionLogNodeKey -> FilePath -> FileDescInputNoReasons -> ExecutionLogNodeKey
executionLogNodeKey (ExecutionLogNodeKey oldKey) filePath fileDescInput = ExecutionLogNodeKey $ MD5.hash $ (oldKey) <> encode filePath <> encode fileDescInput

executionLogNodeRootKey :: Makefile.Target -> ExecutionLogNodeKey
executionLogNodeRootKey = ExecutionLogNodeKey . targetKey TargetLogExecutionLogNode

executionLogInsert :: Db -> ExecutionLogNodeKey -> ExecutionLog -> [(FilePath, FileDescInputNoReasons)] -> [(FilePath, FileDescInputNoReasons)] -> IO ()
executionLogInsert db key el inputsPassed inputsLeft = {-# SCC "executionLogInsert" #-} do
    let iref = executionLogNode key db
    -- putStrLn $ "executionLogInsert: " <> "inputsPassed: " <> (show $ length inputsPassed) <> ", inputsLeft: " <> (show $ length inputsLeft)
    case inputsLeft of
        [] -> do
            -- TODO check if exists at current iref and panic?
            writeIRef iref $ ExecutionLogNodeLeaf el
        (i@(inputFile, inputFileDesc):is) -> do
            let nextKey = executionLogNodeKey key inputFile inputFileDesc
                mapOfMaps = NonEmptyMap.singleton inputFile (NonEmptyMap.singleton inputFileDesc nextKey)
            writeIRef iref $ ExecutionLogNodeBranch mapOfMaps
            executionLogInsert db nextKey el (i:inputsPassed) is

executionLogUpdate :: Makefile.Target -> Db -> ExecutionLog -> IO ()
executionLogUpdate target db el = executionLogUpdate' (executionLogNode key db) key db el [] inputFiles
    where
        key = executionLogNodeRootKey target
        inputFiles = map (\(f, d) -> (f, fileDescInputDropReasons d)) . Map.toList $ elInputsDescs el


executionLogUpdate' :: IRef ExecutionLogNode -> ExecutionLogNodeKey -> Db -> ExecutionLog
                       -> [(FilePath, FileDescInputNoReasons)] -> [(FilePath, FileDescInputNoReasons)] -> IO ()
executionLogUpdate' iref key db el inputsPassed [] = do
    -- TODO validate current iref key matches inputsPassed?
    writeIRef iref (ExecutionLogNodeLeaf el)
executionLogUpdate' iref key db el inputsPassed inputsLeft@(i@(inputFile, inputFileDesc):is) = {-# SCC "executionLogUpdate'_branch" #-} do
    eln <- readIRef iref
    case eln of
        Nothing -> do
            let nextKey = executionLogNodeKey key inputFile inputFileDesc
                newMapOfMaps = NonEmptyMap.singleton inputFile (NonEmptyMap.singleton inputFileDesc nextKey)
            writeIRef iref (ExecutionLogNodeBranch newMapOfMaps)
            executionLogInsert db nextKey el inputsPassed inputsLeft
        Just ExecutionLogNodeLeaf{} -> error "wat" -- TODO
        Just (ExecutionLogNodeBranch mapOfMaps) -> do
            case NonEmptyMap.lookup inputFile mapOfMaps of
                Nothing -> do
                    let nextKey = executionLogNodeKey key inputFile inputFileDesc
                        updatedMapOfMaps = NonEmptyMap.insert inputFile (NonEmptyMap.singleton inputFileDesc nextKey) mapOfMaps
                    writeIRef iref (ExecutionLogNodeBranch updatedMapOfMaps)
                    executionLogInsert db nextKey el inputsPassed inputsLeft
                Just mapOfFileDescs -> do
                    let matchingKeys = map snd $ filter ((cmpFileDescInput inputFileDesc) . fst) (NonEmptyMap.toList mapOfFileDescs)
                    case matchingKeys of
                        [] -> do
                            let nextKey = executionLogNodeKey key inputFile inputFileDesc
                                updatedMapOfMaps = NonEmptyMap.insert inputFile (NonEmptyMap.insert inputFileDesc nextKey mapOfFileDescs) mapOfMaps
                            writeIRef iref (ExecutionLogNodeBranch updatedMapOfMaps)
                            executionLogInsert db nextKey el inputsPassed inputsLeft
                        (key:_) -> executionLogUpdate' (executionLogNode key db) key db el (i:inputsPassed) is

latestExecutionLog :: Makefile.Target -> Db -> IRef ExecutionLog
latestExecutionLog = mkIRefKey . targetKey TargetLogLatestExecutionLog

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
